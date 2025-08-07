{-# LANGUAGE OverloadedStrings #-}
module ProgrammableTokens.Test.DirectorySet(
  tests
) where

import Cardano.Api.Shelley qualified as C
import Control.Monad.Except (MonadError)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadMockchain, MonadUtxoQuery, sendTx)
import Convex.CoinSelection (ChangeOutputPosition (TrailingChange))
import Convex.MockChain.CoinSelection (tryBalanceAndSubmit)
import Convex.Utils (mapError)
import Convex.Wallet.MockWallet qualified as Wallet
import Convex.Wallet.Operator (signTxOperator)
import Data.Functor (void)
import Data.String (IsString (..))
import ProgrammableTokens.OffChain.Env qualified as Env
import ProgrammableTokens.OffChain.Query qualified as Query
import ProgrammableTokens.OffChain.Scripts qualified as Scripts
import ProgrammableTokens.Test (ScriptTarget (..))
import ProgrammableTokens.Test qualified as Test
import ProgrammableTokens.Test.Error (TestError (BalancingError))
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
    [ testCase "deploy directory and global params" (Test.mockchainSucceedsWithTarget @(TestError C.ConwayEra) target $ Test.deployDirectorySet Test.admin)
    , testGroup "issue programmable tokens"
        [ testCase "always succeeds validator" (Test.mockchainSucceedsWithTarget @(TestError C.ConwayEra) target $ Test.deployDirectorySet Test.admin >>= issueAlwaysSucceedsValidator)
        ]
    ]

{-| Issue some tokens with the "always succeeds" validator
-}
issueAlwaysSucceedsValidator :: (MonadUtxoQuery m, MonadFail m, MonadError (TestError C.ConwayEra) m, MonadMockchain C.ConwayEra m) => Env.DirectoryScriptRoot -> m ()
issueAlwaysSucceedsValidator scriptRoot = do
  let dirEnv      = Env.mkDirectoryEnv scriptRoot
      transferEnv = Env.alwaysSucceedsTransferLogic Production
      runAs' = Env.runAs Test.admin dirEnv transferEnv
  runAs' registerAlwaysSucceedsStakingCert

  runAs' $ do
    Test.issueProgrammableTokenTx "dummy asset" 100
      >>= void . sendTx . signTxOperator Test.admin
    Query.registryNodes @C.ConwayEra
      >>= void . Test.expectN 2 "registry outputs"
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . Test.expectN 1 "programmable logic outputs"

{-| Register the 'alwaysSucceedsScript' stake validator
-}
registerAlwaysSucceedsStakingCert :: (MonadUtxoQuery m, MonadError (TestError C.ConwayEra) m, MonadFail m, MonadMockchain C.ConwayEra m) =>  m ()
registerAlwaysSucceedsStakingCert = do
  let script = C.PlutusScript C.plutusScriptVersion $ Scripts.alwaysSucceedsScript Production
      hsh = C.hashScript script
      cred = C.StakeCredentialByScript hsh
  txBody <- BuildTx.execBuildTxT $ do
    cert <- BuildTx.mkConwayStakeCredentialRegistrationCertificate cred
    BuildTx.addStakeScriptWitness cert cred (Scripts.alwaysSucceedsScript Production) ()
  void (mapError BalancingError $ tryBalanceAndSubmit mempty Wallet.w1 txBody TrailingChange [])
