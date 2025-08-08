{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
module Wst.Cli.Env(
  -- * Cli env.
  CliEnv(..),
  cliOperatorEnv,

  -- * Runtime data
  RuntimeEnv(..),
  HasRuntimeEnv(..),
  loadRuntimeEnv,
  blockfrostProject,
  logger


) where

import Blammo.Logging (Logger)
import Blammo.Logging.Logger (HasLogger (..), newLogger)
import Blammo.Logging.LogSettings.Env qualified as LogSettingsEnv
import Blockfrost.Auth (mkProject)
import Blockfrost.Client.Auth qualified as Blockfrost
import Cardano.Api.Shelley qualified as C
import Control.Lens (makeLensesFor)
import Data.Text qualified as Text
import ProgrammableTokens.OffChain.Env (CombinedEnv, HasDirectoryEnv (..),
                                        HasOperatorEnv (..),
                                        HasTransferLogicEnv (..), OperatorEnv)
import System.Environment qualified

data RuntimeEnv =
  RuntimeEnv
    { ceLogger :: Logger
    , ceBlockfrost :: Blockfrost.Project
    }

makeLensesFor
  [ ("ceLogger", "logger")
  , ("ceBlockfrost", "blockfrostProject")
  ]
  'RuntimeEnv

instance HasLogger RuntimeEnv where
  loggerL = logger

-- | Load the 'RuntimeEnv' from environment variables
loadRuntimeEnv :: IO RuntimeEnv
loadRuntimeEnv =
  RuntimeEnv
    <$> (LogSettingsEnv.parse >>= newLogger)
    <*> fmap (mkProject . Text.pack) (System.Environment.getEnv "CIP_0143_BLOCKFROST_TOKEN")
    -- <*> projectFromEnv

class HasRuntimeEnv e where
  runtimeEnv :: e -> RuntimeEnv

instance HasRuntimeEnv RuntimeEnv where
  runtimeEnv = id

data CliEnv f =
  CliEnv
    { ceRuntime  :: RuntimeEnv
    , ceOperator :: OperatorEnv C.ConwayEra
    }

makeLensesFor
  [ ("ceRuntime", "runtime")
  ]
  'CliEnv

instance HasLogger (CliEnv f) where
  loggerL = runtime . logger

instance HasRuntimeEnv (CliEnv f) where
  runtimeEnv = ceRuntime

-- instance HasDirectoryEnv (CliEnv tr) where
--   directoryEnv = directoryEnv . ceCombined

instance HasOperatorEnv C.ConwayEra (CliEnv tr) where
  operatorEnv = ceOperator

-- instance HasTransferLogicEnv (CliEnv Identity) where
--   transferLogicEnv = transferLogicEnv . ceCombined

cliOperatorEnv :: RuntimeEnv -> OperatorEnv C.ConwayEra -> CliEnv f
cliOperatorEnv = CliEnv
