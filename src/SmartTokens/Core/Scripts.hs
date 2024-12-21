module SmartTokens.Core.Scripts (
  tryCompile,
  tryCompileTracingAndBinds,
  tryCompileNoTracing,
) where

import Plutarch

tryCompile :: Config -> ClosedTerm a -> Script
tryCompile cfg x = case compile cfg x of
  Left e -> error $ "Compilation failed: " <> show e
  Right s -> s

tryCompileTracingAndBinds :: ClosedTerm a -> Script
tryCompileTracingAndBinds = tryCompile (Tracing LogInfo DoTracingAndBinds)

tryCompileNoTracing :: ClosedTerm a -> Script
tryCompileNoTracing = tryCompile NoTracing
