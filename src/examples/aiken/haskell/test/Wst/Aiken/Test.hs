{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Wst.Aiken.Test
  ( tests,
  )
where

import Cardano.Api qualified as C
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (runReaderT)
import Convex.Class (MonadBlockchain, MonadUtxoQuery, sendTx)
import Convex.CoinSelection (AsBalancingError, AsCoinSelectionError)
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet qualified as Wallet
import Convex.Wallet.Operator (signTxOperator)
import Data.Functor (void)
import Data.Map qualified as Map
import Paths_aiken_example qualified as Pkg
import ProgrammableTokens.OffChain.Endpoints qualified as Endpoints
import ProgrammableTokens.OffChain.Env qualified as Env
import ProgrammableTokens.OffChain.Error (AsProgrammableTokensError)
import ProgrammableTokens.OffChain.Query qualified as Query
import ProgrammableTokens.Test qualified as Test
import SmartTokens.Core.Scripts (ScriptTarget (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase, testCaseSteps)
import Wst.Aiken.Blueprint (Blueprint (..))
import Wst.Aiken.Blueprint qualified as Blueprint
import Wst.Aiken.Error (AikenError, AsBlueprintError)
import Wst.Aiken.Offchain qualified as Offchain

tests :: TestTree
tests =
  testGroup
    "unit tests"
    [ testCase "load blueprint" loadBlueprint,
      testCase "deserialise script" deserialiseScript,
      testGroup
        "emulator"
        [ testCaseSteps "register"
            $ Test.mockchainSucceedsWithTarget @(AikenError C.ConwayEra) Debug . registerAikenPolicy
        , testCase "transfer" (Test.mockchainSucceedsWithTarget @(AikenError C.ConwayEra) Debug transferAikenPolicy)
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
        assertEqual "Script hash" "7ed916a50e65ceefc39ca1fbb74d4bacc1518ebe586cd6576ae682f9" hsh
      _ -> fail "Unexpected script language"

loadExample :: IO Blueprint
loadExample = do
  Pkg.getDataFileName "data/aiken-scripts.json"
    >>= Blueprint.loadFromFile
    >>= either fail pure

registerAikenPolicy :: forall era err m.
  ( MonadIO m
  , MonadError err m
  , MonadBlockchain era m
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  , Offchain.AsLookupScriptFailure err
  , AsBlueprintError err
  , AsCoinSelectionError err
  , AsBalancingError err era
  , AsProgrammableTokensError err
  , MonadFail m
  , C.IsConwayBasedEra era
  )
  => (String -> IO ())
  -> m ()
registerAikenPolicy step' = do
  blueprint <- liftIO loadExample >>= flip Offchain.lookupScripts_ Offchain.blueprintKeys >>= Offchain.extractV3Scripts_
  scriptRoot <- runReaderT (Test.deployDirectorySet Test.admin) Production
  let runAsAdmin = Env.runAs Test.admin (Env.mkDirectoryEnv scriptRoot) (Offchain.transferLogic blueprint)
      step = liftIO . step'

  step "Registering stake scripts"
  _ <- runAsAdmin
        $ Endpoints.registerCip143PolicyTransferScripts
            >>= void . sendTx . signTxOperator Test.admin

  step "Registering CIP 143 policy"
  runAsAdmin $ do
    Endpoints.registerCip143PolicyTx "TEST" 1000 ()
      >>= void . sendTx . signTxOperator Test.admin

    Query.registryNodes @C.ConwayEra
      >>= void . Test.expectN 2 "registry outputs"

  pure ()

transferAikenPolicy :: forall era err m.
  ( MonadIO m
  , MonadError err m
  , MonadBlockchain era m
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  , Offchain.AsLookupScriptFailure err
  , AsBlueprintError err
  , AsCoinSelectionError err
  , AsBalancingError err era
  , AsProgrammableTokensError err
  , MonadFail m
  , C.IsConwayBasedEra era
  )
  => m ()
transferAikenPolicy = do
  blueprint <- liftIO loadExample >>= flip Offchain.lookupScripts_ Offchain.blueprintKeys >>= Offchain.extractV3Scripts_
  scriptRoot <- runReaderT (Test.deployDirectorySet Test.admin) Production
  let runAsAdmin = Env.runAs Test.admin (Env.mkDirectoryEnv scriptRoot) (Offchain.transferLogic blueprint)

  _ <- runAsAdmin
        $ Endpoints.registerCip143PolicyTransferScripts
            >>= void . sendTx . signTxOperator Test.admin

  runAsAdmin $ do
    Endpoints.registerCip143PolicyTx "TEST" 1000 ()
      >>= void . sendTx . signTxOperator Test.admin

  let paymentCred = C.PaymentCredentialByKey (Wallet.verificationKeyHash Wallet.w2)

  runAsAdmin $
    Query.userProgrammableOutputs paymentCred
      >>= void . Test.expectN 0 "user programmable outputs"

  runAsAdmin $ do
    Endpoints.transferTokens "TEST" 500 paymentCred ()
      >>= void . sendTx . signTxOperator Test.admin

  runAsAdmin $
    Query.userProgrammableOutputs paymentCred
      >>= void . Test.expectN 1 "user programmable outputs"

  runAsAdmin $ do
    Endpoints.transferTokens "TEST" 500 paymentCred ()
      >>= void . sendTx . signTxOperator Test.admin

  runAsAdmin $
    Query.userProgrammableOutputs paymentCred
      >>= void . Test.expectN 2 "user programmable outputs"

