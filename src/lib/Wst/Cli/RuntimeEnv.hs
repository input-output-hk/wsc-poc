{-# LANGUAGE TemplateHaskell #-}
{-| Data that we need when running the CLI
-}
module Wst.Cli.RuntimeEnv(
  RuntimeEnv(..),
  loadEnv,
) where

import Blammo.Logging (Logger)
import Blammo.Logging.Logger (HasLogger (..), newLogger)
import Blammo.Logging.LogSettings.Env qualified as LogSettingsEnv
import Blockfrost.Auth (mkProject)
import Blockfrost.Client.Auth qualified as Blockfrost
import Control.Lens (makeLensesFor)
import Data.Text qualified as Text
import System.Environment qualified

data RuntimeEnv
  = RuntimeEnv
      { envLogger     :: Logger
      , envBlockfrost :: Blockfrost.Project

      }

makeLensesFor
  [ ("envLogger", "logger")
  , ("envBlockfrostProject", "blockfrostProject")
  ]
  'RuntimeEnv

instance HasLogger RuntimeEnv where
  loggerL = logger

-- | Load the 'RuntimeEnv' from environment variables
loadEnv :: IO RuntimeEnv
loadEnv =
  RuntimeEnv
    <$> (LogSettingsEnv.parse >>= newLogger)
    <*> fmap (mkProject . Text.pack) (System.Environment.getEnv "WST_BLOCKFROST_TOKEN")
