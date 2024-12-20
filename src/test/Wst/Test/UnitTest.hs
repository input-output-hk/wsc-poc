{-# LANGUAGE OverloadedStrings #-}
module Wst.Test.UnitTest(
  tests
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad (void)
import Convex.Class (MonadBlockchain (sendTx), MonadUtxoQuery)
import Convex.MockChain.Utils (mockchainSucceeds)
import Convex.Utils (failOnError)
import Convex.Wallet.Operator (signTxOperator)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Wst.Offchain.BuildTx.DirectorySet (InsertNodeArgs (..))
import Wst.Offchain.Endpoints.Deployment qualified as Endpoints
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query qualified as Query
import Wst.Test.Env (admin, asAdmin)

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "deploy directory and global params" (mockchainSucceeds deployDirectorySet)
  , testCase "insert directory node" (mockchainSucceeds insertDirectoryNode)
  ]

deployDirectorySet :: (MonadUtxoQuery m, MonadBlockchain C.ConwayEra m, MonadFail m) => m C.TxIn
deployDirectorySet = failOnError $ asAdmin @C.ConwayEra $ do
  (tx, txI) <- Endpoints.deployTx
  void $ sendTx $ signTxOperator admin tx
  Env.withDirectoryFor txI $ do
    Query.registryNodes @C.ConwayEra
      >>= void . expectSingleton "registry output"
    Query.globalParamsNode @C.ConwayEra
      >>= void . expectSingleton "global params output"
  pure txI

insertDirectoryNode :: (MonadUtxoQuery m, MonadBlockchain C.ConwayEra m, MonadFail m) => m ()
insertDirectoryNode = failOnError $ do
  txI <- deployDirectorySet
  asAdmin @C.ConwayEra $ Env.withDirectoryFor txI $ do
    Endpoints.insertNodeTx dummyNodeArgs >>= void . sendTx . signTxOperator admin
    Query.registryNodes @C.ConwayEra
      >>= void . expectN 2 "registry outputs"

dummyNodeArgs :: InsertNodeArgs
dummyNodeArgs =
  InsertNodeArgs
    { inaNewKey = "e165610232235bbbbeff5b998b23e165610232235bbbbeff5b998b23"
    , inaTransferLogic = C.StakeCredentialByScript "e165610232235bbbbeff5b998b23e165610232235bbbbeff5b998b23"
    , inaIssuerLogic = C.StakeCredentialByScript "e165610232235bbbbeff5b998b23e165610232235bbbbeff5b998b23"
    }

expectSingleton :: MonadFail m => String -> [a] -> m a
expectSingleton msg = \case
  [a] -> pure a
  ls  -> fail $ "Expected a single " ++ msg ++ " but found " ++ show (length ls)

expectN :: MonadFail m => Int -> String -> [a] -> m ()
expectN n msg lst
  | length lst == n = pure ()
  | otherwise       = fail $ "Expected " ++ show n ++ " " ++ msg ++ " but found " ++ show (length lst)
