{-# LANGUAGE OverloadedStrings #-}
module Wst.Test.UnitTest(
  tests
) where

import Cardano.Api qualified as C
import Convex.Class (MonadBlockchain, MonadUtxoQuery)
import Convex.MockChain.Utils (mockchainSucceeds)
import Convex.Utils (failOnError)
import PlutusLedgerApi.V1.Credential (Credential (..))
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Wst.Offchain.Endpoints.ProtocolParams qualified as Endpoints
import Wst.Test.Env (asAdmin)

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "deploy protocol params" (mockchainSucceeds deployProtocolParams)
  ]

deployProtocolParams :: (MonadUtxoQuery m, MonadBlockchain C.ConwayEra m, MonadFail m) => m ()
deployProtocolParams = failOnError $ asAdmin @C.ConwayEra $ do
  let params =
        ProgrammableLogicGlobalParams
          { directoryNodeCS = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          , progLogicCred   = ScriptCredential "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          }
  _ <- Endpoints.deployParamsTx params
  pure ()
