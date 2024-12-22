{-# LANGUAGE OverloadedStrings #-}
module Wst.Cli(runMain) where

import Blammo.Logging.Simple (MonadLogger, logError, logInfo, runLoggerLoggingT)
import Control.Monad.IO.Class (MonadIO (..))
import Convex.Wallet.Operator (OperatorConfigSigning)
import Convex.Wallet.Operator qualified as Operator
import Data.String (IsString (..))
import Options.Applicative (customExecParser, disambiguate, helper, idm, info,
                            prefs, showHelpOnEmpty, showHelpOnError)
import Wst.App (runWstApp)
import Wst.Cli.Command (Command (..), ManageCommand (StartServer, Status),
                        parseCommand)
import Wst.Offchain.Env qualified as Env
import Wst.Server qualified as Server

runMain :: IO ()
runMain = do
  customExecParser
    (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
    (info (helper <*> parseCommand) idm)
    >>= runCommand

runCommand :: Command -> IO ()
runCommand com = do
  env <- Env.addRuntimeEnv <$> Env.loadRuntimeEnv <*> pure Env.empty
  result <- case com of
    Deploy config -> runWstApp env (deploy config)
    Manage txIn com -> do
      let env' = Env.addDirectoryEnvFor txIn env
      runWstApp env' $ case com of
        Status -> do
          -- TODO: status check (call the query endpoints and print out a summary of the results)
          logInfo "Manage"
        StartServer -> do
          logInfo "starting server"
          liftIO (Server.runServer env')

  case result of
    Left err -> runLoggerLoggingT env $ logError (fromString $ show err)
    Right a -> pure a

deploy :: (MonadLogger m, MonadIO m) => OperatorConfigSigning -> m ()
deploy config = do
  logInfo "Loading operator files"
  _operator <- liftIO (Operator.loadOperatorFiles config)
  -- TODO:
  -- Use blockfrost backend to run Wst.Offchain.Endpoints.Deployment with the operator's funds
  -- Then use operator key to sign
  -- Then submit transaction to blockfrost
  -- Convex.Blockfrost.runBLockfrostT for the monadblockchain / monadutxoquery effects
  pure ()
