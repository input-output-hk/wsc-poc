{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

{-| servant server for stablecoin POC
-}
module Wst.Server(
  runServer,
  ServerArgs(..),
  defaultServerArgs
  ) where

import Cardano.Api.Shelley qualified as C
import Control.Lens qualified as L
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, asks)
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (MonadBlockchain, MonadUtxoQuery)
import Data.Data (Proxy (..))
import Network.Wai.Handler.Warp qualified as Warp
import PlutusTx.Prelude qualified as P
import Servant (Server, ServerT)
import Servant.API (NoContent (..), (:<|>) (..))
import Servant.Server (hoistServer, serve)
import SmartTokens.Types.PTokenDirectory (blnKey)
import Wst.App (WstApp, runWstAppServant)
import Wst.AppError (AppError)
import Wst.Offchain.BuildTx.ProgrammableLogic (alwaysSucceedsArgs,
                                               fromTransferEnv,
                                               programmableTokenAssetId)
import Wst.Offchain.Endpoints.Deployment qualified as Endpoints
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query (UTxODat (uDatum))
import Wst.Offchain.Query qualified as Query
import Wst.Server.Types (APIInEra, AddToBlacklistArgs (..), BuildTxAPI,
                         IssueProgrammableTokenArgs (..), QueryAPI,
                         SeizeAssetsArgs (..), SerialiseAddress (..),
                         TextEnvelopeJSON (..),
                         TransferProgrammableTokenArgs (..))
import SmartTokens.Core.Scripts (ScriptTarget(Production))

data ServerArgs =
  ServerArgs
    { saPort :: !Int
    , saStaticFiles :: Maybe FilePath
    }
    deriving stock (Eq, Show)

defaultServerArgs :: ServerArgs
defaultServerArgs =
  ServerArgs
    { saPort = 8080
    , saStaticFiles = Nothing
    }

runServer :: (Env.HasRuntimeEnv env, Env.HasDirectoryEnv env) => env -> ServerArgs -> IO ()
runServer env ServerArgs{saPort} = do
  let app  = serve (Proxy @APIInEra) (server env)
      port = saPort
  Warp.run port app

server :: forall env. (Env.HasRuntimeEnv env, Env.HasDirectoryEnv env) => env -> Server APIInEra
server env = hoistServer (Proxy @APIInEra) (runWstAppServant env) $
  healthcheck
  :<|> queryApi @env
  :<|> txApi @env

healthcheck :: Applicative m => m NoContent
healthcheck = pure NoContent

queryApi :: forall env. (Env.HasDirectoryEnv env) => ServerT (QueryAPI C.ConwayEra) (WstApp env C.ConwayEra)
queryApi =
  Query.globalParamsNode
  :<|> queryBlacklistedNodes (Proxy @C.ConwayEra)
  :<|> queryUserFunds @C.ConwayEra @env (Proxy @C.ConwayEra)
  :<|> queryAllFunds @C.ConwayEra @env (Proxy @C.ConwayEra)

txApi :: forall env. (Env.HasDirectoryEnv env) => ServerT (BuildTxAPI C.ConwayEra) (WstApp env C.ConwayEra)
txApi =
  issueProgrammableTokenEndpoint @C.ConwayEra @env
  :<|> transferProgrammableTokenEndpoint @C.ConwayEra @env
  :<|> addToBlacklistEndpoint
  :<|> seizeAssetsEndpoint

queryBlacklistedNodes :: forall era env m.
  ( MonadUtxoQuery m
  , C.IsBabbageBasedEra era
  , MonadReader env m
  , Env.HasDirectoryEnv env
  )
  => Proxy era
  -> SerialiseAddress (C.Address C.ShelleyAddr)
  -> m [C.Hash C.PaymentKey]
queryBlacklistedNodes _ (SerialiseAddress addr) = do
  programmableBaseLogicCred <- asks (Env.programmableLogicBaseCredential . Env.directoryEnv)
  let transferLogic = Env.mkTransferLogicEnv Production programmableBaseLogicCred (paymentKeyHashFromAddress addr)
      getHash =
        either (error "deserialiseFromRawBytes failed") id
        . C.deserialiseFromRawBytes (C.proxyToAsType $ Proxy @(C.Hash C.PaymentKey))
        . P.fromBuiltin
        . blnKey
        . uDatum
  Env.withEnv $ Env.withTransfer transferLogic (fmap (fmap getHash) (Query.blacklistNodes @era))

txOutValue :: C.IsMaryBasedEra era => C.TxOut C.CtxUTxO era -> C.Value
txOutValue = L.view (L._TxOut . L._2 . L._TxOutValue)

queryUserFunds :: forall era env m.
  ( MonadUtxoQuery m
  , C.IsBabbageBasedEra era
  , MonadReader env m
  , Env.HasDirectoryEnv env
  , MonadBlockchain era m
  )
  => Proxy era
  -> SerialiseAddress (C.Address C.ShelleyAddr)
  -> m C.Value
queryUserFunds _ (SerialiseAddress addr) =
  foldMap (txOutValue . Query.uOut) <$> Query.userProgrammableOutputs @era @env (paymentCredentialFromAddress addr)

queryAllFunds :: forall era env m.
  ( MonadUtxoQuery m
  , C.IsBabbageBasedEra era
  , MonadReader env m
  , Env.HasDirectoryEnv env
  )
  => Proxy era
  -> m C.Value
queryAllFunds _ = foldMap (txOutValue . Query.uOut) <$> Query.programmableLogicOutputs @era @env

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
  let tokenArgs = alwaysSucceedsArgs Production
  programmableBaseLogicCred <- asks (Env.programmableLogicBaseCredential . Env.directoryEnv)
  Env.withEnv $ Env.withOperator operatorEnv $ Env.withDirectory dirEnv $ Env.withTransfer (Env.mkTransferLogicEnv Production programmableBaseLogicCred (paymentKeyHashFromAddress itaIssuer)) $ do
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
  let transferLogic = Env.mkTransferLogicEnv Production programmableBaseLogicCred (paymentKeyHashFromAddress ttaIssuer)
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
  let transferLogic = Env.mkTransferLogicEnv Production programmableBaseLogicCred (paymentKeyHashFromAddress atbIssuer)
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
  let transferLogic = Env.mkTransferLogicEnv Production programmableBaseLogicCred (paymentKeyHashFromAddress saIssuer)
  Env.withEnv $ Env.withOperator operatorEnv $ Env.withDirectory dirEnv $ Env.withTransfer transferLogic $ do
    TextEnvelopeJSON <$> Endpoints.seizeCredentialAssetsTx badCred
