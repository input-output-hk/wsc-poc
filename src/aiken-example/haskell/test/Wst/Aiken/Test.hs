{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Wst.Aiken.Test(
  tests
) where

import Cardano.Api qualified as C
import Data.Functor (void)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Paths_aiken_example qualified as Pkg
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Wst.Aiken.Blueprint (Blueprint (..))
import Wst.Aiken.Blueprint qualified as Blueprint

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "load blueprint" loadBlueprint
  , testCase "deserialise script" deserialiseScript
  ]

loadBlueprint :: Assertion
loadBlueprint = do
  void loadExample

deserialiseScript :: Assertion
deserialiseScript = do
  Blueprint{validators} <- loadExample
  maybe (fail "Expected script named 'placeholder.placeholder.mint'") pure (Map.lookup "placeholder.placeholder.mint" validators)
    >>= \case
      (C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV3) script) -> do
        let hsh = C.hashScript script
        assertEqual "Script hash" "cc068514c844ed3f6c6d0f131b20cda83dbd50f340242b5740d0f81f" hsh
      _ -> fail "Unexpected script language"

loadExample :: IO Blueprint
loadExample = do
  Pkg.getDataFileName "data/aiken-scripts.json"
    >>= Blueprint.loadFromFile
    >>= either fail pure
