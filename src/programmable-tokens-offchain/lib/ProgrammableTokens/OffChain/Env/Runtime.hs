{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProgrammableTokens.OffChain.Env.Runtime(
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
import Control.Lens qualified as L
import Data.HSet.Get (HGettable)
import Data.HSet.Get qualified as HSet
import Data.HSet.Modify qualified as HSet
import Data.HSet.Type (HSet)
import Data.HSet.Type qualified as HSet
import Data.Text qualified as Text
import ProgrammableTokens.OffChain.Env.Operator (OperatorEnv)
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
    <*> fmap (mkProject . Text.pack) (System.Environment.getEnv "CIP_143_BLOCKFROST_TOKEN")
    -- <*> projectFromEnv

class HasRuntimeEnv e where
  runtimeEnv :: e -> RuntimeEnv

instance HasRuntimeEnv RuntimeEnv where
  runtimeEnv = id

instance (HGettable els RuntimeEnv) => HasRuntimeEnv (HSet els) where
  runtimeEnv = HSet.hget @_ @RuntimeEnv

instance (HGettable els RuntimeEnv, HSet.HMonoModifiable els RuntimeEnv) => HasLogger (HSet els) where
  loggerL = L.lens get set where
    get = L.view logger . runtimeEnv
    set s x = HSet.hmodify (L.set logger x) s

cliOperatorEnv :: RuntimeEnv -> OperatorEnv C.ConwayEra -> HSet [RuntimeEnv, OperatorEnv C.ConwayEra]
cliOperatorEnv re ope = HSet.HSCons re (HSet.HSCons ope HSet.HSNil)
