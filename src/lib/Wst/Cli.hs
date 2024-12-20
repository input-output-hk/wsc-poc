{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Wst.Cli(runMain) where

import Blammo.Logging.Simple (MonadLogger, MonadLoggerIO, WithLogger (..),
                              logError, logInfo, runLoggerLoggingT)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Convex.Wallet.Operator (OperatorConfigSigning)
import Convex.Wallet.Operator qualified as Operator
import Data.String (IsString (..))
import Options.Applicative (customExecParser, disambiguate, helper, idm, info,
                            prefs, showHelpOnEmpty, showHelpOnError)
import Wst.Cli.Command (Command (..), parseCommand)
import Wst.Cli.RuntimeEnv (RuntimeEnv)
import Wst.Cli.RuntimeEnv qualified as RuntimeEnv

runMain :: IO ()
runMain = do
  customExecParser
    (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
    (info (helper <*> parseCommand) idm)
    >>= runCommand

runCommand :: Command -> IO ()
runCommand com = do
  env <- RuntimeEnv.loadEnv
  result <- runWstApp env $ case com of
    Deploy config -> deploy config
    Manage _txIn _com ->
      -- TODO:
      -- * Implement status check (call the query endpoints and print out a summary of the results)
      -- * Start the server
      logInfo "Manage"
  case result of
    Left err -> runLoggerLoggingT env $ logError (fromString $ show err)
    Right a -> pure a

data AppError = AppError
  deriving stock Show

newtype WstApp a = WstApp { unWstApp :: ReaderT RuntimeEnv (ExceptT AppError IO) a }
  deriving newtype (Monad, Applicative, Functor, MonadIO, MonadReader RuntimeEnv, MonadError AppError)
  deriving
    (MonadLogger, MonadLoggerIO)
    via (WithLogger RuntimeEnv (ExceptT AppError IO))

runWstApp :: RuntimeEnv -> WstApp a -> IO (Either AppError a)
runWstApp env WstApp{unWstApp} = runExceptT (runReaderT unWstApp env)

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
