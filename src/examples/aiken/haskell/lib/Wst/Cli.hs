{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Wst.Cli(runMain) where

import Blammo.Logging.Logger (flushLogger)
import Blammo.Logging.Simple (Message ((:#)), MonadLogger, logError, logInfo,
                              runLoggerLoggingT, (.=))
import Cardano.Api.Shelley qualified as C
import Control.Monad.Error.Lens (throwing)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (runReaderT)
import Convex.Class qualified as Convex
import Convex.Wallet.Operator (Operator, OperatorConfigSigning, Signing)
import Convex.Wallet.Operator qualified as Operator
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as ByteString
import Data.Functor (void)
import Data.String (IsString (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Options.Applicative (customExecParser, disambiguate, helper, idm, info,
                            prefs, showHelpOnEmpty, showHelpOnError)
import PlutusLedgerApi.V1 qualified as PV1
import ProgrammableTokens.OffChain.Endpoints qualified as Endpoints
import ProgrammableTokens.OffChain.Env qualified as Env
import ProgrammableTokens.OffChain.Env.Directory qualified as Directory
import ProgrammableTokens.OffChain.Query qualified as Query
import SmartTokens.Core.Scripts (ScriptTarget (Production))
import Wst.Aiken.Error qualified as Error
import Wst.Cli.App (runWstApp)
import Wst.Cli.Command (Command (..), PolicyCommand (..), parseCommand)
import Wst.Cli.Command qualified as Command
import Wst.Cli.Env qualified as Env

runMain :: IO ()
runMain = do
  customExecParser
    (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
    (info (helper <*> parseCommand) idm)
    >>= runCommand

runCommand :: Command -> IO ()
runCommand com = do
  runtimeEnv <- Env.loadRuntimeEnv
  case com of
    GenerateOperator -> do
      key <- C.generateSigningKey C.AsPaymentKey
      putStrLn $ Text.unpack $ C.serialiseToBech32 key
      void $ runWstApp runtimeEnv $ do
        networkId <- Convex.queryNetworkId
        let key'     = Operator.PESigning key
            operator = Operator.Operator key' Nothing
            addr     = Operator.operatorAddress networkId operator
        liftIO $ putStrLn $ Text.unpack $ C.serialiseAddress addr
    DeployRegistry operatorConfig targetFile -> do
      result <- runWstApp runtimeEnv $ do
        logInfo $ "cip-143-cli deploy" :# [ "target_file" .= targetFile ]
        operator <- loadOperator operatorConfig
        opEnv <- Env.loadConvexOperatorEnv operator
        flip runReaderT (Env.cliOperatorEnv runtimeEnv opEnv) $ do

          (tx, env@Env.DirectoryScriptRoot{Env.srTxIn, Env.srIssuanceCborHexTxIn, Env.srTarget}) <- Endpoints.deployCip143RegistryTx Production
          logInfo $ "Created deployment Tx" :#
            [ "registry_tx_in" .= srTxIn
            , "cbox_hex_tx_in" .= srIssuanceCborHexTxIn
            , "script_target"  .= srTarget
            , "tx_id"          .= C.getTxId (C.getTxBody tx)
            ]
          void $ Convex.sendTx $ Operator.signTxOperator operator tx

          liftIO $ ByteString.writeFile targetFile $ Aeson.encode env
        flushLogger
      case result of
        Left err -> runLoggerLoggingT runtimeEnv $ logError (fromString $ show err)
        Right a -> pure a
    Query -> do
      result <- runWstApp runtimeEnv $ do
        logInfo "cip-143-cli query"
        dir <- liftIO Directory.loadFromFile >>= either (throwing Error._BlueprintJsonError) pure
        nodes <- runReaderT (Query.registryNodes @C.ConwayEra) dir
        logInfo $ "registry" :#
          [ "nodes.count" .= length nodes
          ]
        flushLogger
      case result of
        Left err -> runLoggerLoggingT runtimeEnv $ logError (fromString $ show err)
        Right a -> pure a

    PolicyCom policy policyCommand -> do
      result <- runWstApp runtimeEnv $ do
        dir <- liftIO Directory.loadFromFile >>= either (throwing Error._BlueprintJsonError) pure
        transferPolicy <- Command.loadTransferPolicy policy
        case policyCommand of
          Register operatorConfig -> do
            logInfo "cip-143-cli policy register"
            operator <- loadOperator operatorConfig
            opEnv <- Env.loadConvexOperatorEnv operator
            flip runReaderT (Env.combinedEnv dir opEnv transferPolicy) $ do
              tx <- Endpoints.registerCip143PolicyTransferScripts
              logInfo $ "Created policy stake script registration tx" :#
                [ "tx_id"          .= C.getTxId (C.getTxBody tx)
                ]
              void $ Convex.sendTx $ Operator.signTxOperator operator tx
            flushLogger
          Issue operatorConfig assetName quantity redeemer -> do
            logInfo "cip-143-cli policy issue"
            operator <- loadOperator operatorConfig
            opEnv <- Env.loadConvexOperatorEnv operator
            flip runReaderT (Env.combinedEnv dir opEnv transferPolicy) $ do
              let red = PV1.toBuiltin redeemer
              tx <- Endpoints.registerCip143PolicyTx assetName quantity red
              logInfo $ "Created policy token issuance tx" :#
                [ "tx_id"          .= C.getTxId (C.getTxBody tx)
                , "quantity"       .= quantity
                , "asset_name"     .= assetName
                , "redeemer"       .= TE.decodeUtf8 (Base16.encode (C.serialiseToCBOR $ C.fromPlutusData $ PV1.fromBuiltin red))
                ]
              void $ Convex.sendTx $ Operator.signTxOperator operator tx
            flushLogger
          Transfer -> pure ()
      case result of
        Left err -> runLoggerLoggingT runtimeEnv $ logError (fromString $ show err)
        Right a -> pure a

loadOperator :: (MonadLogger m, MonadIO m, Convex.MonadBlockchain C.ConwayEra m) => OperatorConfigSigning -> m (Operator Signing)
loadOperator opConfig = do
  logInfo $ "Loading operator files" :# ["key_file" .= Operator.ocSigningKeyFile opConfig]
  op <- liftIO (Operator.loadOperatorFiles opConfig)
  networkId <- Convex.queryNetworkId
  let addr     = Operator.operatorAddress networkId op
  logInfo $ "Operator ready" :# ["address" .= addr]
  pure op
