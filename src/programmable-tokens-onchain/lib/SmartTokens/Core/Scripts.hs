module SmartTokens.Core.Scripts (
  -- * Build targets
  ScriptTarget(..),
  targetConfig,

  -- * Compile functions
  tryCompile,
  tryCompileTracingAndBinds,
  tryCompileNoTracing,
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Plutarch.Internal.Term (Config (..), LogLevel (LogInfo), Script, Term,
                               TracingMode (..), compile)

{-| Script target environment
-}
data ScriptTarget
  = Debug -- ^ Include debug symbols
  | Production -- ^ No debug symbols
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{-| The plutarch 'Config' for the target
-}
targetConfig :: ScriptTarget -> Config
targetConfig = \case
  Debug      -> _tracingAndBindsConfig
  Production -> prodConfig

tryCompile :: ScriptTarget -> (forall s. Term s a) -> Script
tryCompile tgt x = case compile (targetConfig tgt) x of
  Left e -> error $ "Compilation failed: " <> show e
  Right s -> s

tryCompileTracingAndBinds :: (forall s. Term s a) -> Script
tryCompileTracingAndBinds = tryCompile Debug

tryCompileNoTracing :: (forall s. Term s a) -> Script
tryCompileNoTracing = tryCompile Production

_tracingAndBindsConfig :: Config
_tracingAndBindsConfig = Tracing LogInfo DoTracingAndBinds

_tracingConfig :: Config
_tracingConfig = Tracing LogInfo DoTracing

prodConfig :: Config
prodConfig = NoTracing
