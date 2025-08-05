{-# LANGUAGE OverloadedStrings #-}
module ProgrammableTokens.Test.DirectorySet(
  tests
) where

import Cardano.Api qualified as C
import Convex.Utils (failOnError)
import Data.String (IsString (..))
import ProgrammableTokens.Test (ScriptTarget (..))
import ProgrammableTokens.Test qualified as Test
import ProgrammableTokens.Test.Error (TestError)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests = testGroup "unit tests"
  [ scriptTargetTests Debug
  , scriptTargetTests Production
  ]

scriptTargetTests :: ScriptTarget -> TestTree
scriptTargetTests target =
  testGroup (fromString $ show target)
    [ testCase "deploy directory and global params" (Test.mockchainSucceedsWithTarget target (failOnError @_ @(TestError C.ConwayEra) $ Test.deployDirectorySet Test.admin))
    , testGroup "issue programmable tokens"
        [ -- testCase "always succeeds validator" (Test.mockchainSucceedsWithTarget target $ Test.deployDirectorySet >>= issueAlwaysSucceedsValidator)
        ]
    ]
