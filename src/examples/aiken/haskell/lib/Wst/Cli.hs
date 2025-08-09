{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Wst.Cli(runMain) where

import Blammo.Logging.Logger (flushLogger)
import Blammo.Logging.Simple (Message ((:#)), MonadLogger, logError, logInfo,
                              runLoggerLoggingT, (.=))
import Cardano.Api qualified as C
import Control.Monad.Error.Lens (throwing)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (runReaderT)
import Convex.Class qualified as Convex
import Convex.Wallet.Operator (Operator, OperatorConfigSigning, Signing)
import Convex.Wallet.Operator qualified as Operator
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as ByteString
import Data.Functor (void)
import Data.String (IsString (..))
import Data.Text qualified as Text
import Options.Applicative (customExecParser, disambiguate, helper, idm, info,
                            prefs, showHelpOnEmpty, showHelpOnError)
import ProgrammableTokens.OffChain.Endpoints qualified as Endpoints
import ProgrammableTokens.OffChain.Env qualified as Env
import ProgrammableTokens.OffChain.Env.Directory qualified as Directory
import SmartTokens.Core.Scripts (ScriptTarget (Production))
import Wst.Aiken.Blueprint qualified as Blueprint
import Wst.Aiken.Error qualified as Error
import Wst.Aiken.Offchain qualified as OffChain
import Wst.Cli.App (runWstApp)
import Wst.Cli.Command (Command (..), RegisterCommand (..), parseCommand)
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
    Deploy operatorConfig targetFile -> do
      result <- runWstApp runtimeEnv $ do
        logInfo $ "cip-132-cli deploy" :# [ "target_file" .= targetFile ]
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
    Register operatorConfig (RegisterAiken blueprint) -> do
      result <- runWstApp runtimeEnv $ do
        logInfo "cip-132-cli register"
        bp <- Blueprint.loadFromFile_ blueprint
                >>= flip OffChain.lookupScripts_ OffChain.blueprintKeys
                >>= OffChain.extractV3Scripts_
        operator <- loadOperator operatorConfig
        opEnv <- Env.loadConvexOperatorEnv operator
        dir <- liftIO Directory.loadFromFile >>= either (throwing Error._BlueprintJsonError) pure
        flip runReaderT (Env.combinedEnv dir opEnv (OffChain.transferLogic bp)) $ do
          tx <- Endpoints.registerCip143PolicyTransferScripts
          logInfo $ "Created script registration tx" :#
            [ "tx_id"          .= C.getTxId (C.getTxBody tx)
            ]
          void $ Convex.sendTx $ Operator.signTxOperator operator tx
        flushLogger
      case result of
        Left err -> runLoggerLoggingT runtimeEnv $ logError (fromString $ show err)
        Right a -> pure a

    _ -> putStrLn $ "UNKNOWN COMMAND " <> show com

loadOperator :: (MonadLogger m, MonadIO m, Convex.MonadBlockchain C.ConwayEra m) => OperatorConfigSigning -> m (Operator Signing)
loadOperator opConfig = do
  logInfo $ "Loading operator files" :# ["key_file" .= Operator.ocSigningKeyFile opConfig]
  op <- liftIO (Operator.loadOperatorFiles opConfig)
  networkId <- Convex.queryNetworkId
  let addr     = Operator.operatorAddress networkId op
  logInfo $ "Operator ready" :# ["address" .= addr]
  pure op
