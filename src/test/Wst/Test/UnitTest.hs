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
import PlutusLedgerApi.V1.Credential (Credential (..))
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Wst.Offchain.Endpoints.DirectorySet qualified as Endpoints
import Wst.Offchain.Endpoints.ProtocolParams qualified as Endpoints
import Wst.Test.Env (admin, asAdmin)

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "deploy protocol params" (mockchainSucceeds deployProtocolParams)
  , testCase "deploy directory set" (mockchainSucceeds deployDirectorySet)
  ]

deployProtocolParams :: (MonadUtxoQuery m, MonadBlockchain C.ConwayEra m, MonadFail m) => m ()
deployProtocolParams = failOnError $ asAdmin @C.ConwayEra $ do
  let params =
        ProgrammableLogicGlobalParams
          { directoryNodeCS = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          , progLogicCred   = ScriptCredential "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          }
  Endpoints.deployParamsTx params >>= void . sendTx . signTxOperator admin

deployDirectorySet :: (MonadUtxoQuery m, MonadBlockchain C.ConwayEra m, MonadFail m) => m ()
deployDirectorySet = failOnError $ asAdmin @C.ConwayEra $ do
  (tx, _txI) <- Endpoints.initDirectoryTx
  void $ sendTx $ signTxOperator admin tx
