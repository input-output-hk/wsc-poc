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
import Plutarch.Internal.Term (ClosedTerm, Config (..), LogLevel (LogInfo),
                               Script, TracingMode (DoTracingAndBinds), compile)

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
  Debug      -> tracingAndBindsConfig
  Production -> prodConfig

tryCompile :: ScriptTarget -> ClosedTerm a -> Script
tryCompile tgt x = case compile (targetConfig tgt) x of
  Left e -> error $ "Compilation failed: " <> show e
  Right s -> s

tryCompileTracingAndBinds :: ClosedTerm a -> Script
tryCompileTracingAndBinds = tryCompile Debug

tryCompileNoTracing :: ClosedTerm a -> Script
tryCompileNoTracing = tryCompile Production

tracingAndBindsConfig :: Config
tracingAndBindsConfig = Tracing LogInfo DoTracingAndBinds

prodConfig :: Config
prodConfig = NoTracing
