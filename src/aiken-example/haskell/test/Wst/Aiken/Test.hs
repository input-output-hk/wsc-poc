{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Wst.Aiken.Test(
  tests
) where

import Cardano.Api qualified as C
import Data.Functor (void)
import Data.Map qualified as Map
import Paths_aiken_example qualified as Pkg
import SmartTokens.Core.Scripts (ScriptTarget (Debug, Production))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Wst.Aiken.Blueprint (Blueprint (..))
import Wst.Aiken.Blueprint qualified as Blueprint

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "load blueprint" loadBlueprint
  , testCase "deserialise script" deserialiseScript
  , testGroup "emulator" [
      testCase "register" (mockchainSucceedsWithTarget _ action)
  ]
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
        assertEqual "Script hash" "9c4802ae9a38d64cf970d68f34f55c6504c749c64fe0968fea77dac8" hsh
      _ -> fail "Unexpected script language"

loadExample :: IO Blueprint
loadExample = do
  Pkg.getDataFileName "data/aiken-scripts.json"
    >>= Blueprint.loadFromFile
    >>= either fail pure
