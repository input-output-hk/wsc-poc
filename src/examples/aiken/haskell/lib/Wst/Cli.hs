{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Wst.Cli(runMain) where

import Blammo.Logging.Logger (flushLogger)
import Blammo.Logging.Simple (Message ((:#)), MonadLogger, logError, logInfo,
                              runLoggerLoggingT, (.=))
import Cardano.Api qualified as C
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (runReaderT)
import Convex.Class qualified as Convex
import Convex.Wallet.Operator (Operator, OperatorConfigSigning, Signing)
import Convex.Wallet.Operator qualified as Operator
import Data.Functor (void)
import Data.String (IsString (..))
import Data.Text qualified as Text
import Options.Applicative (customExecParser, disambiguate, helper, idm, info,
                            prefs, showHelpOnEmpty, showHelpOnError)
import ProgrammableTokens.OffChain.Endpoints qualified as Endpoints
import ProgrammableTokens.OffChain.Env qualified as Env
import SmartTokens.Core.Scripts (ScriptTarget (Production))
import Wst.Cli.App (runWstApp)
import Wst.Cli.Command (Command (..), parseCommand)
import Wst.Cli.Env qualified as Env

runMain :: IO ()
runMain = do
  customExecParser
    (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
    (info (helper <*> parseCommand) idm)
    >>= runCommand

runCommand :: Command -> IO ()
runCommand com = do

  case com of
    GenerateOperator -> do
      runtimeEnv <- Env.loadRuntimeEnv
      key <- C.generateSigningKey C.AsPaymentKey
      putStrLn $ Text.unpack $ C.serialiseToBech32 key
      void $ runWstApp runtimeEnv $ do
        networkId <- Convex.queryNetworkId
        let key'     = Operator.PESigning key
            operator = Operator.Operator key' Nothing
            addr     = Operator.operatorAddress networkId operator
        liftIO $ putStrLn $ Text.unpack $ C.serialiseAddress addr
    Deploy operatorConfig -> do
      runtimeEnv <- Env.loadRuntimeEnv
      result <- runWstApp runtimeEnv $ do
        logInfo "cip-132-cli deploy"
        operator <- loadOperator operatorConfig
        opEnv <- Env.loadConvexOperatorEnv operator
        flip runReaderT (Env.cliOperatorEnv runtimeEnv opEnv) $ do

          (tx, Env.DirectoryScriptRoot{Env.srTxIn, Env.issuanceCborHexTxIn, Env.srTarget}) <- Endpoints.deployCip143RegistryTx Production
          logInfo $ "Created deployment Tx" :#
            [ "registry_tx_in" .= srTxIn
            , "cbox_hex_tx_in" .= issuanceCborHexTxIn
            , "script_target"  .= srTarget
            , "tx_id"          .= C.getTxId (C.getTxBody tx)
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
