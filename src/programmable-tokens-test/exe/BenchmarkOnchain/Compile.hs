{-# LANGUAGE RankNTypes #-}

module BenchmarkOnchain.Compile (compileNoTracing, compileTracing) where

import Plutarch.Internal.Term (Config (NoTracing, Tracing), LogLevel (LogInfo), TracingMode (DoTracing), Script, Term, compile)

compileNoTracing :: (forall s. Term s a) -> Script
compileNoTracing term =
    either (error . ("compile failed: " <>) . show) id (compile NoTracing term)

-- | Diagnostic-only: keeps @ptraceInfoIfFalse@ messages so a failing fixture
-- reports WHICH invariant rejected it. Never used for measurement — tracing
-- changes both the script and its cost.
compileTracing :: (forall s. Term s a) -> Script
compileTracing term =
    either (error . ("compile failed: " <>) . show) id (compile (Tracing LogInfo DoTracing) term)
