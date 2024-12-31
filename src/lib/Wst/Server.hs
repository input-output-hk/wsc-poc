{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

{-| servant server for stablecoin POC
-}
module Wst.Server(runServer) where

import Cardano.Api.Shelley qualified as C
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, asks)
import Convex.Class (MonadBlockchain, MonadUtxoQuery)
import Data.Data (Proxy (..))
import Network.Wai.Handler.Warp qualified as Warp
import Servant (Server, ServerT)
import Servant.API (NoContent (..), (:<|>) (..))
import Servant.Server (hoistServer, serve)
import Wst.App (WstApp, runWstAppServant)
import Wst.AppError (AppError)
import Wst.Offchain.BuildTx.ProgrammableLogic (alwaysSucceedsArgs,
                                               fromTransferEnv,
                                               programmableTokenAssetId)
import Wst.Offchain.Endpoints.Deployment qualified as Endpoints
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query qualified as Query
import Wst.Server.Types (APIInEra, AddToBlacklistArgs (..), BuildTxAPI,
                         IssueProgrammableTokenArgs (..), QueryAPI,
                         SeizeAssetsArgs (..), TextEnvelopeJSON (..),
                         TransferProgrammableTokenArgs (..))

runServer :: (Env.HasRuntimeEnv env, Env.HasDirectoryEnv env) => env -> IO ()
runServer env = do
  let app = serve (Proxy @APIInEra) (server env)
      port = 8081
  Warp.run port app

server :: forall env. (Env.HasRuntimeEnv env, Env.HasDirectoryEnv env) => env -> Server APIInEra
server env = hoistServer (Proxy @APIInEra) (runWstAppServant env) $
  healthcheck
  :<|> queryApi @env @C.ConwayEra
  :<|> txApi @env

healthcheck :: Applicative m => m NoContent
healthcheck = pure NoContent

queryApi :: forall env era. C.IsBabbageBasedEra era => ServerT (QueryAPI era) (WstApp env era)
queryApi = Query.globalParamsNode

txApi :: forall env. (Env.HasDirectoryEnv env) => ServerT (BuildTxAPI C.ConwayEra) (WstApp env C.ConwayEra)
txApi =
  issueProgrammableTokenEndpoint @C.ConwayEra @env
  :<|> transferProgrammableTokenEndpoint @C.ConwayEra @env
  :<|> addToBlacklistEndpoint
  :<|> seizeAssetsEndpoint

issueProgrammableTokenEndpoint :: forall era env m.
  ( MonadReader env m
  , Env.HasDirectoryEnv env
  , MonadBlockchain era m
  , MonadError (AppError era) m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  )
  => IssueProgrammableTokenArgs -> m (TextEnvelopeJSON (C.Tx era))
issueProgrammableTokenEndpoint IssueProgrammableTokenArgs{itaAssetName, itaQuantity, itaIssuer} = do
  operatorEnv <- Env.loadOperatorEnvFromAddress itaIssuer
  dirEnv <- asks Env.directoryEnv

      -- FIXME: Replace alwaysSucceedsArgs with blacklist monetary policy as soon as it is finished
  let tokenArgs = alwaysSucceedsArgs
  Env.withEnv $ Env.withOperator operatorEnv $ Env.withDirectory dirEnv $ do
    TextEnvelopeJSON <$> Endpoints.issueProgrammableTokenTx tokenArgs itaAssetName itaQuantity

paymentCredentialFromAddress :: C.Address C.ShelleyAddr -> C.PaymentCredential
paymentCredentialFromAddress = \case
  C.ShelleyAddress _ cred _ -> C.fromShelleyPaymentCredential cred

paymentKeyHashFromAddress :: C.Address C.ShelleyAddr -> C.Hash C.PaymentKey
paymentKeyHashFromAddress = \case
  C.ShelleyAddress _ (C.fromShelleyPaymentCredential -> C.PaymentCredentialByKey cred) _ -> cred
  _ -> error "Expected PaymentCredentialByKey"

transferProgrammableTokenEndpoint :: forall era env m.
  ( MonadReader env m
  , Env.HasDirectoryEnv env
  , MonadBlockchain era m
  , MonadError (AppError era) m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  )
  => TransferProgrammableTokenArgs -> m (TextEnvelopeJSON (C.Tx era))
transferProgrammableTokenEndpoint TransferProgrammableTokenArgs{ttaSender, ttaRecipient, ttaAssetName, ttaQuantity, ttaIssuer} = do
  operatorEnv <- Env.loadOperatorEnvFromAddress ttaSender
  dirEnv <- asks Env.directoryEnv
  programmableBaseLogicCred <- asks (Env.programmableLogicBaseCredential . Env.directoryEnv)
  let transferLogic = Env.mkTransferLogicEnv programmableBaseLogicCred (paymentKeyHashFromAddress ttaIssuer)
  assetId <- programmableTokenAssetId <$> Env.getGlobalParams <*> pure (fromTransferEnv transferLogic) <*> pure ttaAssetName
  Env.withEnv $ Env.withOperator operatorEnv $ Env.withDirectory dirEnv $ Env.withTransfer transferLogic $ do
    TextEnvelopeJSON <$> Endpoints.transferSmartTokensTx assetId ttaQuantity (paymentCredentialFromAddress ttaRecipient)

addToBlacklistEndpoint :: forall era env m.
  ( MonadReader env m
  , Env.HasDirectoryEnv env
  , MonadBlockchain era m
  , MonadError (AppError era) m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  )
  => AddToBlacklistArgs -> m (TextEnvelopeJSON (C.Tx era))
addToBlacklistEndpoint AddToBlacklistArgs{atbIssuer, atbBlacklistAddress} = do
  let badCred = paymentCredentialFromAddress atbBlacklistAddress
  operatorEnv <- Env.loadOperatorEnvFromAddress atbIssuer
  dirEnv <- asks Env.directoryEnv
  programmableBaseLogicCred <- asks (Env.programmableLogicBaseCredential . Env.directoryEnv)
  let transferLogic = Env.mkTransferLogicEnv programmableBaseLogicCred (paymentKeyHashFromAddress atbIssuer)
  Env.withEnv $ Env.withOperator operatorEnv $ Env.withDirectory dirEnv $ Env.withTransfer transferLogic $ do
    TextEnvelopeJSON <$> Endpoints.blacklistCredentialTx badCred

seizeAssetsEndpoint :: forall era env m.
  ( MonadReader env m
  , Env.HasDirectoryEnv env
  , MonadBlockchain era m
  , MonadError (AppError era) m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  )
  => SeizeAssetsArgs -> m (TextEnvelopeJSON (C.Tx era))
seizeAssetsEndpoint SeizeAssetsArgs{saIssuer, saTarget} = do
  let badCred = paymentCredentialFromAddress saTarget
  operatorEnv <- Env.loadOperatorEnvFromAddress saIssuer
  dirEnv <- asks Env.directoryEnv
  programmableBaseLogicCred <- asks (Env.programmableLogicBaseCredential . Env.directoryEnv)
  let transferLogic = Env.mkTransferLogicEnv programmableBaseLogicCred (paymentKeyHashFromAddress saIssuer)
  Env.withEnv $ Env.withOperator operatorEnv $ Env.withDirectory dirEnv $ Env.withTransfer transferLogic $ do
    TextEnvelopeJSON <$> Endpoints.seizeCredentialAssetsTx badCred
