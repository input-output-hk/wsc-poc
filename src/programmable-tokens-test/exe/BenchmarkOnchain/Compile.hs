{-# LANGUAGE RankNTypes #-}

module BenchmarkOnchain.Compile (compileNoTracing) where

import Plutarch.Internal.Term (Config (NoTracing), Script, Term, compile)

compileNoTracing :: (forall s. Term s a) -> Script
compileNoTracing term =
    either (error . ("compile failed: " <>) . show) id (compile NoTracing term)
