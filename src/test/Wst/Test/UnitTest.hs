{-# LANGUAGE OverloadedStrings #-}
module Wst.Test.UnitTest(
  tests
) where

import Cardano.Api qualified as C
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Convex.Class (MonadBlockchain (sendTx), MonadUtxoQuery)
import Convex.MockChain.Utils (mockchainSucceeds)
import Convex.Utils (failOnError)
import Convex.Wallet.Operator (signTxOperator)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Wst.Offchain.Endpoints.Deployment qualified as Endpoints
import Wst.Offchain.Endpoints.Query qualified as Query
import Wst.Offchain.Env qualified as Env
import Wst.Test.Env (admin, asAdmin)

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "deploy directory and global params" (mockchainSucceeds deployDirectorySet)
  ]

deployDirectorySet :: (MonadUtxoQuery m, MonadBlockchain C.ConwayEra m, MonadFail m) => m C.TxIn
deployDirectorySet = failOnError $ asAdmin @C.ConwayEra $ do
  (tx, txI) <- Endpoints.deployTx
  void $ sendTx $ signTxOperator admin tx
  flip runReaderT (Env.directoryEnv txI) $ do
    Query.registryNodes @C.ConwayEra
      >>= void . expectSingleton "registry output"
    Query.globalParamsNode @C.ConwayEra
      >>= void . expectSingleton "global params output"
  pure txI

expectSingleton :: MonadFail m => String -> [a] -> m a
expectSingleton msg = \case
  [a] -> pure a
  ls  -> fail $ "Expected a single " ++ msg ++ " but found " ++ show (length ls)
