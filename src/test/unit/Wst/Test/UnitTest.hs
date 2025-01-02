{-# LANGUAGE OverloadedStrings #-}
module Wst.Test.UnitTest(
  tests
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Plutus.ExUnits qualified as Ledger
import Cardano.Ledger.Shelley.TxCert qualified as TxCert
import Control.Exception (try)
import Control.Lens (set, (%~), (&), (^.))
import Control.Monad (void)
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (MonadReader)
import Convex.BuildTx (MonadBuildTx, addCertificate)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain (queryProtocolParameters, sendTx),
                     MonadMockchain, MonadUtxoQuery)
import Convex.CoinSelection (ChangeOutputPosition (TrailingChange))
import Convex.MockChain
import Convex.MockChain.CoinSelection (tryBalanceAndSubmit)
import Convex.MockChain.Defaults qualified as Defaults
import Convex.MockChain.Utils (mockchainFails, mockchainSucceeds)
import Convex.NodeParams (NodeParams, ledgerProtocolParameters,
                          protocolParameters)
import Convex.Utils (failOnError)
import Convex.Wallet.MockWallet qualified as Wallet
import Convex.Wallet.Operator (signTxOperator)
import Data.List (isPrefixOf)
import Data.Word (Word32)
import GHC.Exception (SomeException, throw)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import Wst.Offchain.BuildTx.DirectorySet (InsertNodeArgs (..))
import Wst.Offchain.BuildTx.ProgrammableLogic (alwaysSucceedsArgs)
import Wst.Offchain.Endpoints.Deployment qualified as Endpoints
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query qualified as Query
import Wst.Offchain.Scripts qualified as Scripts
import Wst.Test.Env (admin, asAdmin, asWallet, user)

testTxSize :: Word32
testTxSize = 16384

testNodeParams :: NodeParams C.ConwayEra
testNodeParams =
  -- restrict script bugdet to current value on mainnet
  let newExUnits = Ledger.ExUnits {Ledger.exUnitsSteps = 10_000_000_000, Ledger.exUnitsMem = 14_000_000}
      npsTx = Defaults.nodeParams & set (ledgerProtocolParameters . protocolParameters . Ledger.ppMaxTxSizeL) testTxSize
  in npsTx & set (ledgerProtocolParameters . protocolParameters . Ledger.ppMaxTxExUnitsL) newExUnits

-- | Run the 'Mockchain' action with modified node parameters to allow larger-than-usual
-- transactions. This is useful for showing debug output from the scripts and fail if there is an error
mockchainSucceedsWithLargeTx :: MockchainIO C.ConwayEra a -> Assertion
mockchainSucceedsWithLargeTx action =
  let params' = testNodeParams & ledgerProtocolParameters . protocolParameters . Ledger.ppMaxTxSizeL %~ (*10)
  in try @SomeException (runMockchain0IOWith Wallet.initialUTxOs params' action) >>= \case
      Right{} -> pure ()
      Left err -> fail (show err)

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "deploy directory and global params" (mockchainSucceedsWithLargeTx deployDirectorySet)
  , testCase "insert directory node" (mockchainSucceeds insertDirectoryNode)
  , testGroup "issue programmable tokens"
      [ testCase "always succeeds validator" (mockchainSucceeds issueAlwaysSucceedsValidator)
      , testCase "smart token issuance" (mockchainSucceeds issueSmartTokensScenario)
      , testCase "smart token transfer" (mockchainSucceeds transferSmartTokens)
      , testCase "blacklist credential" (mockchainSucceeds (void blacklistCredential))
      , testCase "blacklisted transfer" (mockchainFails blacklistTransfer assertBlacklistedAddressException)
      , testCase "seize user output" (mockchainSucceeds seizeUserOutput)
      ]
  ]

deployDirectorySet :: (MonadUtxoQuery m, MonadBlockchain C.ConwayEra m, MonadFail m) => m C.TxIn
deployDirectorySet = failOnError $ Env.withEnv $ asAdmin @C.ConwayEra $ do
  (tx, txI) <- Endpoints.deployTx
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

issueSmartTokensScenario :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => m C.AssetId
issueSmartTokensScenario = deployDirectorySet >>= issueTransferLogicProgrammableToken

{-| Issue some tokens with the smart stabelcoin transfer logic validator
-}
issueTransferLogicProgrammableToken :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => C.TxIn -> m C.AssetId
issueTransferLogicProgrammableToken txI = failOnError $ Env.withEnv $ do

  asAdmin @C.ConwayEra $ Env.withDirectoryFor txI $ Env.withTransferFromOperator $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
    -- register programmable global stake script
    void $ registerTransferScripts opPkh

  asAdmin @C.ConwayEra $ Env.withDirectoryFor txI $ Env.withTransferFromOperator $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)

    (balTx, aid) <- Endpoints.issueSmartTokensTx "dummy asset" 100 (C.PaymentCredentialByKey opPkh)
    void $ sendTx $ signTxOperator admin balTx

    Query.registryNodes @C.ConwayEra
      >>= void . expectN 2 " registry outputs"
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . expectN 1 "programmable logic outputs"
    pure aid

{-| Issue some tokens with the smart stabelcoin transfer logic validator
-}
transferSmartTokens :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => m ()
transferSmartTokens = failOnError $ Env.withEnv $ do
  userPkh <- asWallet Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv)
  txI <- deployDirectorySet

  asAdmin @C.ConwayEra $ Env.withDirectoryFor txI $ Env.withTransferFromOperator $ do
    Endpoints.deployBlacklistTx
      >>= void . sendTx . signTxOperator admin
    Query.blacklistNodes @C.ConwayEra
      >>= void . expectSingleton "blacklist output"

  aid <- issueTransferLogicProgrammableToken txI

  asAdmin @C.ConwayEra $ Env.withDirectoryFor txI $ Env.withTransferFromOperator $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)

    Endpoints.transferSmartTokensTx aid 80 (C.PaymentCredentialByKey userPkh)
      >>= void . sendTx . signTxOperator admin

    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . expectN 2 "programmable logic outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey userPkh)
      >>= void . expectN 1 "user programmable outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey opPkh)
      >>= void . expectN 1 "user programmable outputs"

blacklistCredential :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => m C.PaymentCredential
blacklistCredential = failOnError $ Env.withEnv $ do
  userPkh <- asWallet Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv)
  let paymentCred = C.PaymentCredentialByKey userPkh

  txIn <- deployDirectorySet

  asAdmin @C.ConwayEra $ Env.withDirectoryFor txIn $ Env.withTransferFromOperator $ do
    Endpoints.deployBlacklistTx
      >>= void . sendTx . signTxOperator admin
    Query.blacklistNodes @C.ConwayEra
      >>= void . expectSingleton "blacklist output"

  asAdmin @C.ConwayEra $ Env.withDirectoryFor txIn $ Env.withTransferFromOperator $ do
    Endpoints.blacklistCredentialTx paymentCred
      >>= void . sendTx . signTxOperator admin

    Query.blacklistNodes @C.ConwayEra
      >>= void . expectN 2 "blacklist output"

  pure paymentCred

blacklistTransfer :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => m ()
blacklistTransfer = failOnError $ Env.withEnv $ do
  userPkh <- asWallet Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv)
  let userPaymentCred = C.PaymentCredentialByKey userPkh

  txIn <- deployDirectorySet
  aid <- issueTransferLogicProgrammableToken txIn

  asAdmin @C.ConwayEra $ Env.withDirectoryFor txIn $ Env.withTransferFromOperator $ do
    Endpoints.deployBlacklistTx
      >>= void . sendTx . signTxOperator admin

  opPkh <- asAdmin @C.ConwayEra $ Env.withDirectoryFor txIn $ Env.withTransferFromOperator $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
    Endpoints.transferSmartTokensTx aid 50 (C.PaymentCredentialByKey userPkh)
      >>= void . sendTx . signTxOperator admin
    pure opPkh

  progLogicCred <- asAdmin @C.ConwayEra $ Env.withDirectoryFor txIn $ Env.withTransferFromOperator $ do
    cred <- asks Env.directoryEnv
    pure $ Env.programmableLogicBaseCredential cred

  asAdmin @C.ConwayEra $ Env.withDirectoryFor txIn $ Env.withTransferFromOperator $ do
    Endpoints.blacklistCredentialTx userPaymentCred
      >>= void . sendTx . signTxOperator admin

  asWallet Wallet.w2 $ Env.withDirectoryFor txIn $ Env.withTransferFor progLogicCred opPkh $ do
    Endpoints.transferSmartTokensTx aid 30 (C.PaymentCredentialByKey opPkh)
      >>= void . sendTx . signTxOperator (user Wallet.w2)

seizeUserOutput :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => m ()
seizeUserOutput = failOnError $ Env.withEnv $ do
  userPkh <- asWallet Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv)
  let userPaymentCred = C.PaymentCredentialByKey userPkh

  txIn <- deployDirectorySet
  aid <- issueTransferLogicProgrammableToken txIn

  asAdmin @C.ConwayEra $ Env.withDirectoryFor txIn $ Env.withTransferFromOperator $ do
    Endpoints.deployBlacklistTx
      >>= void . sendTx . signTxOperator admin

  asAdmin @C.ConwayEra $ Env.withDirectoryFor txIn $ Env.withTransferFromOperator $ do
    Endpoints.transferSmartTokensTx aid 50 (C.PaymentCredentialByKey userPkh)
      >>= void . sendTx . signTxOperator admin
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . expectN 2 "programmable logic outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey userPkh)
      >>= void . expectN 1 "user programmable outputs"

  asAdmin @C.ConwayEra $ Env.withDirectoryFor txIn $ Env.withTransferFromOperator $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
    Endpoints.seizeCredentialAssetsTx userPaymentCred
      >>= void . sendTx . signTxOperator admin
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . expectN 3 "programmable logic outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey userPkh)
      >>= void . expectN 1 "user programmable outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey opPkh)
      >>= void . expectN 2 "user programmable outputs"


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

registerTransferScripts :: (MonadFail m, MonadReader env m, Env.HasDirectoryEnv env, Env.HasTransferLogicEnv env, MonadMockchain C.ConwayEra m) => C.Hash C.PaymentKey -> m C.TxId
registerTransferScripts pkh = failOnError $ do
  pp <- fmap C.unLedgerProtocolParameters queryProtocolParameters
  transferMintingScript <- asks (Env.tleMintingScript . Env.transferLogicEnv)
  transferSpendingScript <- asks (Env.tleTransferScript . Env.transferLogicEnv)
  transferGlobalScript <- asks (Env.dsProgrammableLogicGlobalScript . Env.directoryEnv)
  let
      hshMinting = C.hashScript $ C.PlutusScript C.plutusScriptVersion transferMintingScript
      credMinting = C.StakeCredentialByScript hshMinting

      hshSpending = C.hashScript $ C.PlutusScript C.plutusScriptVersion transferSpendingScript
      credSpending = C.StakeCredentialByScript hshSpending

      hshGlobal = C.hashScript $ C.PlutusScript C.plutusScriptVersion transferGlobalScript
      credGlobal = C.StakeCredentialByScript hshGlobal

  txBody <- BuildTx.execBuildTxT $ do
    BuildTx.addStakeScriptWitness credMinting transferMintingScript ()
    BuildTx.addConwayStakeCredentialRegistrationCertificate credMinting (pp ^. Ledger.ppKeyDepositL)

    addStakeCredentialCertificate credSpending
    addStakeCredentialCertificate credGlobal

    BuildTx.addRequiredSignature pkh

  x <- tryBalanceAndSubmit mempty Wallet.w1 txBody TrailingChange []
  pure $ C.getTxId $ C.getTxBody x

{-| Add a 'C.StakeCredential' as a certificate to the transaction
-}
addStakeCredentialCertificate :: forall era m. C.IsConwayBasedEra era => MonadBuildTx era m => C.StakeCredential -> m ()
addStakeCredentialCertificate stk =
  C.conwayEraOnwardsConstraints @era C.conwayBasedEra $
  addCertificate $ C.ConwayCertificate C.conwayBasedEra $ TxCert.RegTxCert $ C.toShelleyStakeCredential stk

expectSingleton :: MonadFail m => String -> [a] -> m a
expectSingleton msg = \case
  [a] -> pure a
  ls  -> fail $ "Expected a single " ++ msg ++ " but found " ++ show (length ls)

expectN :: MonadFail m => Int -> String -> [a] -> m ()
expectN n msg lst
  | length lst == n = pure ()
  | otherwise       = fail $ "Expected " ++ show n ++ " " ++ msg ++ " but found " ++ show (length lst)

_expectLeft :: (MonadFail m, Show b) => String -> Either a b -> m ()
_expectLeft msg = \case
  Left _ -> pure ()
  (Right r) -> fail $ "Expected " ++ msg ++ " but found Right " ++ show r

-- TODO: Need to make this nicer
{-| Make sure that the exception is a failure due to blacklisted address
-}
assertBlacklistedAddressException :: SomeException -> Assertion
assertBlacklistedAddressException ex
  | "user error (TransferBlacklistedCredential (PubKeyCredential" `isPrefixOf` show ex = pure ()
  | otherwise = throw ex
