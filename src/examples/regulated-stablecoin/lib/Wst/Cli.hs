{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Wst.Cli(runMain) where

import Blammo.Logging.Simple (Message ((:#)), MonadLogger, logDebug, logError,
                              logInfo, runLoggerLoggingT, (.=))
import Cardano.Api qualified as C
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, asks)
import Convex.Class (MonadBlockchain (sendTx), MonadUtxoQuery)
import Convex.Wallet.Operator (Operator (Operator, oPaymentKey),
                               OperatorConfigSigning,
                               PaymentExtendedKey (PESigningEx), signTxOperator,
                               verificationKey)
import Convex.Wallet.Operator qualified as Operator
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Options.Applicative (customExecParser, disambiguate, helper, idm, info,
                            prefs, showHelpOnEmpty, showHelpOnError)
import ProgrammableTokens.OffChain.Env.Operator qualified as Env
import ProgrammableTokens.OffChain.Env.Runtime qualified as Env
import ProgrammableTokens.OffChain.Env.Utils qualified as Utils
import SmartTokens.Core.Scripts (ScriptTarget (Production))
import Wst.App (runWstApp)
import Wst.AppError (AppError)
import Wst.Cli.Command (Command (..), ManageCommand (StartServer, Status),
                        parseCommand)
import Wst.Offchain.Endpoints.Deployment qualified as Endpoints
import Wst.Offchain.Env qualified as Env
import Wst.Server (ServerArgs (..))
import Wst.Server qualified as Server

runMain :: IO ()
runMain = do
  customExecParser
    (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
    (info (helper <*> parseCommand) idm)
    >>= runCommand

runCommand :: Command -> IO ()
runCommand com = do
  env <- Env.addRuntimeEnv <$> Env.loadRuntimeEnv <*> pure Utils.empty
  result <- case com of
    Deploy config -> runWstApp env (deploy config)
    Manage txIn issuanceCborHexTxIn com_ -> do
      let env' = Env.addDirectoryEnvFor (Env.DirectoryScriptRoot txIn issuanceCborHexTxIn Production) env
      runWstApp env' $ case com_ of
        Status -> do
          -- TODO: status check (call the query endpoints and print out a summary of the results)
          logInfo "Manage"
        StartServer options ->
          Server.staticFilesFromEnv options >>=
          Server.demoFileFromEnv >>=
            startServer env'
  case result of
    Left err -> runLoggerLoggingT env $ logError (fromString $ show err)
    Right a -> pure a

deploy :: forall env m. (MonadLogger m, MonadIO m, MonadUtxoQuery m, MonadReader env m, Env.HasRuntimeEnv env, MonadError (AppError C.ConwayEra) m, MonadBlockchain C.ConwayEra m) => OperatorConfigSigning -> m ()
deploy config = do
  logInfo $ "Loading operator files" :# ["key_file" .= Operator.ocSigningKeyFile config]

  -- FIXME: why does this throw?
  -- _operator <- liftIO (Operator.loadOperatorFiles config)

  signingKey <- liftIO
    $ C.readFileTextEnvelope (C.File $ Operator.ocSigningKeyFile config)
    >>= either (error . show) pure

  let operator = Operator (PESigningEx signingKey) Nothing
      operatorPaymentHash = C.verificationKeyHash . verificationKey . oPaymentKey $ operator

  opEnv <- Env.loadOperatorEnv @_ @C.ConwayEra operatorPaymentHash C.NoStakeAddress
  runEnv <- asks Env.runtimeEnv

  let env = Env.addOperatorEnv opEnv $ Utils.singleton runEnv

  -- Use blockfrost backend to run Wst.Offchain.Endpoints.Deployment with the operator's funds
  (tx, root) <- liftIO (runWstApp env $ do
          Endpoints.deployFullTx Production) >>= liftEither

  logInfo $ "Created deployment Tx" :# ["root" .= root]

  -- Then use operator key to sign
  let signedTx = signTxOperator operator tx
  logDebug $ "Signed Deployment Tx" :# ["tx" .= show tx]

  -- Then submit transaction to blockfrost
  sendTx signedTx >>= \case
    Left err -> logError $ "Error sending Tx" :# ["err" .= show err]
    Right txid -> do
      logInfo $ "Tx submitted successfully" :# ["txid" .= show txid]
      (liftIO $ C.writeFileJSON "deployment-root.json" root) >>= either (error . show) pure

startServer :: (Utils.Elem Env.RuntimeEnv els, Utils.Elem Env.DirectoryEnv els, Utils.HMonoModifiable els Env.RuntimeEnv, MonadIO m, MonadLogger m) => Utils.HSet els -> Server.ServerArgs -> m ()
startServer env' serverArgs@ServerArgs{saPort, saStaticFiles} = do
  logInfo $ "starting server" :# ["port" .= saPort, "static_files" .= fromMaybe "(no static files)" saStaticFiles]
  liftIO (Server.runServer env' serverArgs)
