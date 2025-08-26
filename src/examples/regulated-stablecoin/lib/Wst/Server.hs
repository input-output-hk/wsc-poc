{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-| servant server for stablecoin POC
-}
module Wst.Server(
  runServer,
  ServerArgs(..),
  staticFilesFromEnv,
  demoFileFromEnv,
  CombinedAPI,
  defaultServerArgs
  ) where

import Blammo.Logging.Simple (HasLogger, Message ((:#)), MonadLogger, logInfo,
                              (.=))
import Blockfrost.Client.Types qualified as Blockfrost
import Cardano.Api.Shelley qualified as C
import Control.Lens qualified as L
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, asks)
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (MonadBlockchain (sendTx), MonadUtxoQuery,
                     utxosByPaymentCredential)
import Convex.Utxos qualified as Utxos
import Data.Aeson.Types (KeyValue)
import Data.Data (Proxy (..))
import Data.List (nub)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors
import PlutusTx.Prelude qualified as P
import ProgrammableTokens.OffChain.Env.Operator qualified as Env
import ProgrammableTokens.OffChain.Query qualified as Query
import Servant (Server, ServerT)
import Servant.API (NoContent (..), Raw, (:<|>) (..))
import Servant.Server (hoistServer, serve)
import Servant.Server.StaticFiles (serveDirectoryWebApp)
import SmartTokens.Types.PTokenDirectory (blnKey)
import System.Environment qualified
import Wst.App (WstApp, runWstAppServant)
import Wst.AppError (AppError (..))
import Wst.Offchain.BuildTx.Failing (BlacklistedTransferPolicy (..))
import Wst.Offchain.Endpoints.Deployment qualified as Endpoints
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query (UTxODat (uDatum))
import Wst.Offchain.Query qualified as Query
import Wst.Server.DemoEnvironment (DemoEnvRoute, runDemoEnvRoute)
import Wst.Server.DemoEnvironment qualified as DemoEnvironment
import Wst.Server.Types (APIInEra, AddVKeyWitnessArgs (..),
                         BlacklistNodeArgs (..), BuildTxAPI,
                         IssueProgrammableTokenArgs (..), QueryAPI,
                         SeizeAssetsArgs (..), SerialiseAddress (..),
                         TextEnvelopeJSON (..),
                         TransferProgrammableTokenArgs (..),
                         UserBalanceResponse (..))

-- | Rest API combined with a Raw endpoint
--   for static files
type CombinedAPI =
  APIInEra
  :<|> DemoEnvRoute
  :<|> Raw

data ServerArgs =
  ServerArgs
    { saPort :: !Int
    , saStaticFiles         :: Maybe FilePath
    , saDemoEnvironmentFile :: Maybe FilePath
    }
    deriving stock (Eq, Show)

{-| Try to read the location of the static files from the 'WST_STATIC_FILES'
variable, if it has not been set.
-}
staticFilesFromEnv :: MonadIO m => ServerArgs -> m ServerArgs
staticFilesFromEnv sa@ServerArgs{saStaticFiles} = case saStaticFiles of
  Just _ -> pure sa
  Nothing -> do
    files' <- liftIO (System.Environment.lookupEnv "WST_STATIC_FILES")
    pure sa{saStaticFiles = files'}

demoFileFromEnv :: MonadIO m => ServerArgs -> m ServerArgs
demoFileFromEnv sa@ServerArgs{saDemoEnvironmentFile} = case saDemoEnvironmentFile of
  Just _ -> pure sa
  Nothing -> do
    files' <- liftIO (System.Environment.lookupEnv "WST_DEMO_ENV")
    pure sa{saDemoEnvironmentFile = files'}

defaultServerArgs :: ServerArgs
defaultServerArgs =
  ServerArgs
    { saPort = 8080
    , saStaticFiles = Nothing
    , saDemoEnvironmentFile = Nothing
    }

runServer :: (Env.HasRuntimeEnv env, Env.HasDirectoryEnv env, HasLogger env) => env -> ServerArgs -> IO ()
runServer env ServerArgs{saPort, saStaticFiles, saDemoEnvironmentFile} = do
  let bf   = Blockfrost.projectId $ Env.envBlockfrost $ Env.runtimeEnv env
  demoEnv <-
    fromMaybe (DemoEnvironment.previewNetworkDemoEnvironment bf)
    <$> traverse DemoEnvironment.loadFromFile saDemoEnvironmentFile
  let app  = cors (const $ Just simpleCorsResourcePolicy)
        $ case saStaticFiles of
            Nothing -> serve (Proxy @APIInEra) (server env)
            Just fp -> serve (Proxy @CombinedAPI) (server env :<|> runDemoEnvRoute demoEnv :<|> serveDirectoryWebApp fp)
      port = saPort
  Warp.run port app

server :: forall env. (Env.HasRuntimeEnv env, Env.HasDirectoryEnv env, HasLogger env) => env -> Server APIInEra
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
  :<|> computeUserAddress (Proxy @C.ConwayEra)

txApi :: forall env. (Env.HasDirectoryEnv env, HasLogger env) => ServerT (BuildTxAPI C.ConwayEra) (WstApp env C.ConwayEra)
txApi =
  (issueProgrammableTokenEndpoint @C.ConwayEra @env
  :<|> transferProgrammableTokenEndpoint @C.ConwayEra @env
  :<|> addToBlacklistEndpoint
  :<|> removeFromBlacklistEndpoint
  :<|> seizeAssetsEndpoint
  )
  :<|> pure . addWitnessEndpoint
  :<|> submitTxEndpoint


computeUserAddress :: forall era env m.
  ( MonadReader env m
  , Env.HasDirectoryEnv env
  , C.IsShelleyBasedEra era
  , MonadBlockchain era m
  )
  => Proxy era
  -> SerialiseAddress (C.Address C.ShelleyAddr)
  -> m (C.Address C.ShelleyAddr)
computeUserAddress _ (SerialiseAddress addr) = do
  let C.ShelleyAddress _ paymentCredential _stakeCredential  = addr
  Env.programmableTokenReceivingAddress @era (C.fromShelleyPaymentCredential paymentCredential) >>= \case
    C.AddressInEra (C.ShelleyAddressInEra _) addr_ -> pure addr_

    -- This is impossible as we construct the address with makeShelleyAddressInEra
    -- But the compiler doesn't realise that.
    C.AddressInEra C.ByronAddressInAnyEra _ -> error "Unexpected byron address"

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
  (transferLogic, ble) <- Env.transferLogicForDirectory (paymentKeyHashFromAddress addr)
  let getHash =
        either (error "deserialiseFromRawBytes failed") id
        . C.deserialiseFromRawBytes (C.proxyToAsType $ Proxy @(C.Hash C.PaymentKey))
        . P.fromBuiltin
        . blnKey
        . uDatum
      nonHeadNodes (P.fromBuiltin . blnKey . uDatum -> hsh) = hsh /= ""
  Env.withEnv $ Env.withBlacklist ble $ Env.withTransfer transferLogic (fmap getHash . filter nonHeadNodes <$> (Query.blacklistNodes @era))

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
  -> m UserBalanceResponse
queryUserFunds _ (SerialiseAddress addr) = do
  let credential = paymentCredentialFromAddress addr
  ubrProgrammableTokens <- foldMap (txOutValue . Query.uOut) <$> Query.userProgrammableOutputs @era @env credential
  otherUTxOs <- utxosByPaymentCredential credential
  let userBalance = C.selectLovelace (Utxos.totalBalance otherUTxOs)
      adaOnly     = Map.size $ Utxos._utxos $ Utxos.onlyAda otherUTxOs
  pure
    UserBalanceResponse
      { ubrProgrammableTokens
      , ubrUserLovelace   = C.lovelaceToQuantity userBalance
      , ubrAdaOnlyOutputs = adaOnly
      }

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
issueProgrammableTokenEndpoint IssueProgrammableTokenArgs{itaAssetName, itaQuantity, itaIssuer, itaRecipient} = do
  let C.ShelleyAddress _network cred _stake = itaRecipient
      destinationCredential = C.fromShelleyPaymentCredential cred
  operatorEnv <- Env.loadOperatorEnvFromAddress @_ @era itaIssuer
  dirEnv <- asks Env.directoryEnv
  (logic, _) <- Env.transferLogicForDirectory (paymentKeyHashFromAddress itaIssuer)
  Env.withEnv $ Env.withOperator operatorEnv $ Env.withDirectory dirEnv $ Env.withTransfer logic $ do
    TextEnvelopeJSON . fst <$> Endpoints.issueSmartTokensTx itaAssetName itaQuantity destinationCredential

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
  , MonadLogger m
  )
  => TransferProgrammableTokenArgs -> m (TextEnvelopeJSON (C.Tx era))
transferProgrammableTokenEndpoint TransferProgrammableTokenArgs{ttaSender, ttaRecipient, ttaAssetName, ttaQuantity, ttaIssuer, ttaSubmitFailingTx} = do
  operatorEnv <- Env.loadOperatorEnvFromAddress @_ @era ttaSender
  dirEnv <- asks Env.directoryEnv
  (logic, ble) <- Env.transferLogicForDirectory (paymentKeyHashFromAddress ttaIssuer)
  let assetId = Env.programmableTokenAssetId dirEnv logic ttaAssetName
  let policy = if ttaSubmitFailingTx then SubmitFailingTx else DontSubmitFailingTx
  logInfo $ "Transfer programmable tokens" :# [logPolicy policy, logSender ttaSender, logRecipient ttaRecipient]
  Env.withEnv $ Env.withOperator operatorEnv $ Env.withBlacklist ble $ Env.withDirectory dirEnv $ Env.withTransfer logic $ do
    TextEnvelopeJSON <$> Endpoints.transferSmartTokensTx policy assetId ttaQuantity (paymentCredentialFromAddress ttaRecipient)

addToBlacklistEndpoint :: forall era env m.
  ( MonadReader env m
  , Env.HasDirectoryEnv env
  , MonadBlockchain era m
  , MonadError (AppError era) m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  )
  => BlacklistNodeArgs -> m (TextEnvelopeJSON (C.Tx era))
addToBlacklistEndpoint BlacklistNodeArgs{bnaIssuer, bnaBlacklistAddress, bnaReason} = do
  let badCred = paymentCredentialFromAddress bnaBlacklistAddress
  operatorEnv <- Env.loadOperatorEnvFromAddress @_ @era bnaIssuer
  dirEnv <- asks Env.directoryEnv
  (transferLogic, ble) <- Env.transferLogicForDirectory (paymentKeyHashFromAddress bnaIssuer)
  Env.withEnv $ Env.withOperator operatorEnv $ Env.withBlacklist ble $ Env.withDirectory dirEnv $ Env.withTransfer transferLogic $ do
    TextEnvelopeJSON <$> Endpoints.insertBlacklistNodeTx bnaReason badCred

removeFromBlacklistEndpoint :: forall era env m.
  ( MonadReader env m
  , Env.HasDirectoryEnv env
  , MonadBlockchain era m
  , MonadError (AppError era) m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  )
  => BlacklistNodeArgs -> m (TextEnvelopeJSON (C.Tx era))
removeFromBlacklistEndpoint BlacklistNodeArgs{bnaIssuer, bnaBlacklistAddress} = do
  let badCred = paymentCredentialFromAddress bnaBlacklistAddress
  operatorEnv <- Env.loadOperatorEnvFromAddress @_ @era bnaIssuer
  dirEnv <- asks Env.directoryEnv
  (transferLogic, ble) <- Env.transferLogicForDirectory (paymentKeyHashFromAddress bnaIssuer)
  Env.withEnv $ Env.withOperator operatorEnv $ Env.withBlacklist ble $ Env.withDirectory dirEnv $ Env.withTransfer transferLogic $ do
    TextEnvelopeJSON <$> Endpoints.removeBlacklistNodeTx badCred

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
seizeAssetsEndpoint SeizeAssetsArgs{saIssuer, saTarget, saReason} = do
  let badCred = paymentCredentialFromAddress saTarget
  operatorEnv <- Env.loadOperatorEnvFromAddress @_ @era saIssuer
  dirEnv <- asks Env.directoryEnv
  (transferLogic, _) <- Env.transferLogicForDirectory (paymentKeyHashFromAddress saIssuer)
  Env.withEnv $ Env.withOperator operatorEnv $ Env.withDirectory dirEnv $ Env.withTransfer transferLogic $ do
    TextEnvelopeJSON <$> Endpoints.seizeCredentialAssetsTx saReason badCred

addWitnessEndpoint :: forall era. AddVKeyWitnessArgs era -> TextEnvelopeJSON (C.Tx era)
addWitnessEndpoint AddVKeyWitnessArgs{avwTx, avwVKeyWitness} =
  let C.Tx txBody txWits = unTextEnvelopeJSON avwTx
      vkey = unTextEnvelopeJSON avwVKeyWitness
      x = C.makeSignedTransaction (nub $ vkey : txWits) txBody
  in TextEnvelopeJSON x

submitTxEndpoint :: forall era m.
  ( MonadBlockchain era m
  , MonadError (AppError era) m
  )
  =>  TextEnvelopeJSON (C.Tx era) -> m C.TxId
submitTxEndpoint (TextEnvelopeJSON tx) = do
  either (throwError . SubmitError) pure =<< sendTx tx

-- structured Logging

logPolicy :: (KeyValue e kv) => BlacklistedTransferPolicy -> kv
logPolicy p = "policy" .= p

logSender :: (KeyValue e kv) => C.Address C.ShelleyAddr -> kv
logSender p = "sender" .= p

logRecipient :: (KeyValue e kv) => C.Address C.ShelleyAddr -> kv
logRecipient p = "recipient" .= p
