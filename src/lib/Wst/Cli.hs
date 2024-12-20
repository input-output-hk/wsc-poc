{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Wst.Cli(runMain) where

import Blammo.Logging.Simple (Message ((:#)), MonadLogger, MonadLoggerIO,
                              WithLogger (..), logError, logInfo, logWarn,
                              runLoggerLoggingT)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
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
    Deploy -> logInfo "Deploy"
    Manage txIn com -> logInfo "Manage"
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
