{-# LANGUAGE OverloadedStrings #-}
module Wst.Test.UnitTest(
  tests
) where

import Cardano.Api qualified as C
import Control.Monad (void)
import Convex.Class (MonadBlockchain (sendTx), MonadUtxoQuery)
import Convex.MockChain.Utils (mockchainSucceeds)
import Convex.Utils (failOnError)
import Convex.Wallet.Operator (signTxOperator)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Wst.Offchain.Endpoints.Deployment qualified as Endpoints
import Wst.Test.Env (admin, asAdmin)

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "deploy directory and global params" (mockchainSucceeds deployDirectorySet)
  ]

deployDirectorySet :: (MonadUtxoQuery m, MonadBlockchain C.ConwayEra m, MonadFail m) => m ()
deployDirectorySet = failOnError $ asAdmin @C.ConwayEra $ do
  (tx, _txI) <- Endpoints.deployTx
  void $ sendTx $ signTxOperator admin tx
