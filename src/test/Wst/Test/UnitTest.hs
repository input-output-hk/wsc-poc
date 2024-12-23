{-# LANGUAGE OverloadedStrings #-}
module Wst.Test.UnitTest(
  tests
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Core qualified as Ledger
import Control.Lens ((^.))
import Control.Monad (void, (<=<))
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (MonadReader)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain (queryNetworkId, queryProtocolParameters, sendTx),
                     MonadMockchain, MonadUtxoQuery, nextSlot,
                     utxosByPaymentCredential)
import Convex.CoinSelection (ChangeOutputPosition (TrailingChange))
import Convex.MockChain.CoinSelection (tryBalanceAndSubmit)
import Convex.MockChain.Utils (mockchainSucceeds)
import Convex.Utils (failOnError)
import Convex.Wallet (paymentCredential)
import Convex.Wallet qualified as BuildTx
import Convex.Wallet.MockWallet qualified as Wallet
import Convex.Wallet.Operator (signTxOperator)
import Convex.Wallet.Operator qualified as Env
import Debug.Trace (trace)
import GHC.IsList (IsList (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Wst.Offchain.BuildTx.DirectorySet (InsertNodeArgs (..))
import Wst.Offchain.BuildTx.ProgrammableLogic (alwaysSucceedsArgs)
import Wst.Offchain.Endpoints.Deployment qualified as Endpoints
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query qualified as Query
import Wst.Offchain.Scripts qualified as Scripts
import Wst.Test.Env (admin, asAdmin)

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "deploy directory and global params" (mockchainSucceeds deployDirectorySet)
  , testCase "insert directory node" (mockchainSucceeds insertDirectoryNode)
  , testGroup "issue programmable tokens"
      [ testCase "always succeeds validator" (mockchainSucceeds issueAlwaysSucceedsValidator)
      , testCase "transfer logic issuance" (mockchainSucceeds issueTransferLogicProgrammableToken)
      -- TODO: Add test for the seize/freeze validator
      ]
  ]

deployDirectorySet :: (MonadUtxoQuery m, MonadBlockchain C.ConwayEra m, MonadFail m) => m C.TxIn
deployDirectorySet = failOnError $ Env.withEnv $ asAdmin @C.ConwayEra $ do
  (tx, txI) <- Endpoints.deployTx
  let id = C.getTxId $ C.getTxBody tx
  void $ sendTx $ signTxOperator admin tx
  Env.withDirectoryFor txI $ do
    Query.registryNodes @C.ConwayEra
      >>= void . expectSingleton "registry output"
    void $ Query.globalParamsNode @C.ConwayEra
  pure txI

insertDirectoryNode :: (MonadUtxoQuery m, MonadBlockchain C.ConwayEra m, MonadFail m) => m ()
insertDirectoryNode = failOnError $ Env.withEnv $ do
  txI <- deployDirectorySet
  asAdmin @C.ConwayEra $ Env.withDirectoryFor txI $ do
    Endpoints.insertNodeTx dummyNodeArgs >>= void . sendTx . signTxOperator admin
    Query.registryNodes @C.ConwayEra
      >>= void . expectN 2 "registry outputs"

{-| Issue some tokens with the "always succeeds" validator
-}
issueAlwaysSucceedsValidator :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => m ()
issueAlwaysSucceedsValidator = failOnError $ Env.withEnv $ do

  -- Register the stake validator
  -- Oddly, the tests passes even if we don't do this.
  -- But I'll leave it in because it seems right.
  registerAlwaysSucceedsStakingCert

  txI <- deployDirectorySet
  asAdmin @C.ConwayEra $ Env.withDirectoryFor txI $ do
    Endpoints.issueProgrammableTokenTx alwaysSucceedsArgs "dummy asset" 100
      >>= void . sendTx . signTxOperator admin
    Query.registryNodes @C.ConwayEra
      >>= void . expectN 2 "registry outputs"
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . expectN 1 "programmable logic outputs"
  pure ()

{-| Issue some tokens with the smart stabelcoin transfer logic validator
-}
issueTransferLogicProgrammableToken :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => m ()
issueTransferLogicProgrammableToken = failOnError $ Env.withEnv $ do

  -- register transfer minting script
  -- register transfer spending script
  -- register issuer spending script

  txI <- deployDirectorySet

  asAdmin @C.ConwayEra $ Env.withDirectoryFor txI $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
    -- register programmable global stake script
    void $ registerTransferScripts opPkh

  asAdmin @C.ConwayEra $ Env.withDirectoryFor txI $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)

    Endpoints.issueSmartTokensTx "dummy asset" 100 (C.PaymentCredentialByKey opPkh)
      >>= void . sendTx . signTxOperator admin
    Query.registryNodes @C.ConwayEra
      >>= void . expectN 2 " registry outputs"
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . expectN 1 "programmable logic outputs"
  pure ()

{-| Issue some tokens with the smart stabelcoin transfer logic validator
-}
transferTransferLogicProgrammableToken :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => m ()
transferTransferLogicProgrammableToken = failOnError $ Env.withEnv $ do

  txI <- deployDirectorySet

  asAdmin @C.ConwayEra $ Env.withDirectoryFor txI $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
    -- register programmable global stake script
    void $ registerTransferScripts opPkh

  asAdmin @C.ConwayEra $ Env.withDirectoryFor txI $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)

    Endpoints.issueSmartTokensTx "dummy asset" 100 (C.PaymentCredentialByKey opPkh)
      >>= void . sendTx . signTxOperator admin
    Query.registryNodes @C.ConwayEra
      >>= void . expectN 2 " registry outputs"
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . expectN 1 "programmable logic outputs"
  pure ()


dummyNodeArgs :: InsertNodeArgs
dummyNodeArgs =
  InsertNodeArgs
    { inaNewKey = "e165610232235bbbbeff5b998b23e165610232235bbbbeff5b998b23"
    , inaTransferLogic = C.StakeCredentialByScript "e165610232235bbbbeff5b998b23e165610232235bbbbeff5b998b23"
    , inaIssuerLogic = C.StakeCredentialByScript "e165610232235bbbbeff5b998b23e165610232235bbbbeff5b998b23"
    }

{-| Register the 'alwaysSucceedsScript' stake validator
-}
registerAlwaysSucceedsStakingCert :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) =>  m ()
registerAlwaysSucceedsStakingCert = failOnError $ do
  pp <- fmap C.unLedgerProtocolParameters queryProtocolParameters
  let script = C.PlutusScript C.plutusScriptVersion Scripts.alwaysSucceedsScript
      hsh = C.hashScript script
      cred = C.StakeCredentialByScript hsh
  txBody <- BuildTx.execBuildTxT $ do
    BuildTx.addStakeScriptWitness cred Scripts.alwaysSucceedsScript ()
    BuildTx.addConwayStakeCredentialRegistrationCertificate cred (pp ^. Ledger.ppKeyDepositL)
  void (tryBalanceAndSubmit mempty Wallet.w1 txBody TrailingChange [])

registerTransferScripts :: (MonadFail m, MonadReader env m, Env.HasTransferLogicEnv env, MonadMockchain C.ConwayEra m) => C.Hash C.PaymentKey -> m C.TxId
registerTransferScripts pkh = failOnError $ do
  pp <- fmap C.unLedgerProtocolParameters queryProtocolParameters
  mintingScript <- asks (Env.tleMintingScript . Env.transferLogicEnv)
  spendingScript <- asks (Env.tleTransferScript . Env.transferLogicEnv)
  -- issuerScript <- asks (Env.tleIssuerScript . Env.transferLogicEnv)
  let
      hshMinting = C.hashScript $ C.PlutusScript C.plutusScriptVersion mintingScript
      credMinting = C.StakeCredentialByScript hshMinting

      hshSpending = C.hashScript $ C.PlutusScript C.plutusScriptVersion spendingScript
      credSpending = C.StakeCredentialByScript hshSpending

      -- hshIssuer = C.hashScript $ C.PlutusScript C.plutusScriptVersion issuerScript
      -- credIssuer = C.StakeCredentialByScript hshIssuer

  txBody <- BuildTx.execBuildTxT $ do
    BuildTx.addStakeScriptWitness credMinting mintingScript ()
    BuildTx.addConwayStakeCredentialRegistrationCertificate credMinting (pp ^. Ledger.ppKeyDepositL)

    BuildTx.addStakeScriptWitness credSpending spendingScript ()
    BuildTx.addConwayStakeCredentialRegistrationCertificate credSpending (pp ^. Ledger.ppKeyDepositL)

    -- BuildTx.addStakeScriptWitness credIssuer issuerScript ()
    -- BuildTx.addConwayStakeCredentialRegistrationCertificate credIssuer (pp ^. Ledger.ppKeyDepositL)

    BuildTx.addRequiredSignature pkh

  x <- tryBalanceAndSubmit mempty Wallet.w1 txBody TrailingChange []
  pure $ C.getTxId $ C.getTxBody x


expectSingleton :: MonadFail m => String -> [a] -> m a
expectSingleton msg = \case
  [a] -> pure a
  ls  -> fail $ "Expected a single " ++ msg ++ " but found " ++ show (length ls)

expectN :: MonadFail m => Int -> String -> [a] -> m ()
expectN n msg lst
  | length lst == n = pure ()
  | otherwise       = fail $ "Expected " ++ show n ++ " " ++ msg ++ " but found " ++ show (length lst)
