{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Wst.Aiken.Test
  ( tests,
  )
where

import Cardano.Api qualified as C
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (runReaderT)
import Convex.Class (MonadBlockchain, MonadUtxoQuery)
import Convex.Utils (failOnError)
import Data.Functor (void)
import Data.Map qualified as Map
import Paths_aiken_example qualified as Pkg
import ProgrammableTokens.OffChain.Env qualified as Env
import ProgrammableTokens.Test qualified as Test
import SmartTokens.Core.Scripts (ScriptTarget (Production))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Wst.Aiken.Blueprint (Blueprint (..))
import Wst.Aiken.Blueprint qualified as Blueprint
import Wst.Aiken.Error (AikenError)
import Wst.Aiken.Offchain qualified as Offchain

tests :: TestTree
tests =
  testGroup
    "unit tests"
    [ testCase "load blueprint" loadBlueprint,
      testCase "deserialise script" deserialiseScript,
      testGroup
        "emulator"
        [ testCase "register" (Test.mockchainSucceedsWithTarget Production registerAikenPolicy)
        ]
    ]

loadBlueprint :: Assertion
loadBlueprint = do
  void loadExample

deserialiseScript :: Assertion
deserialiseScript = do
  Blueprint {validators} <- loadExample
  maybe (fail "Expected script named 'transfer.placeholder.mint'") pure (Map.lookup "transfer.placeholder.mint" validators)
    >>= \case
      (C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV3) script) -> do
        let hsh = C.hashScript script
        assertEqual "Script hash" "e42412ef37225695b87ad1701fec45ab657d4a4146959d7c3e36afc2" hsh
      _ -> fail "Unexpected script language"

loadExample :: IO Blueprint
loadExample = do
  Pkg.getDataFileName "data/aiken-scripts.json"
    >>= Blueprint.loadFromFile
    >>= either fail pure

registerAikenPolicy :: forall era m. (MonadIO m, MonadFail m, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, C.IsBabbageBasedEra era, MonadUtxoQuery m) => m ()
registerAikenPolicy = failOnError @_ @(AikenError era) $ do
  bp <- liftIO loadExample >>= flip Offchain.lookupScripts_ Offchain.blueprintKeys >>= Offchain.extractV3Scripts_
  flip runReaderT Production $ do
    scriptRoot <- Test.deployDirectorySet Test.admin
    -- opEnv <- Env.loadConvexOperatorEnv Test.admin
    -- Env.withEnv (Env.directoryOperatorEnv (Env.mkDirectoryEnv scriptRoot) opEnv) $ do
      -- _ <- Offchain.registerBlueprintTx bp
      -- pure ()
    pure ()
