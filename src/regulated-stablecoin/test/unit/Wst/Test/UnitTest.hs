{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Wst.Test.UnitTest(
  tests
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad (void)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain (sendTx), MonadMockchain, MonadUtxoQuery,
                     ValidationError)
import Convex.CoinSelection (ChangeOutputPosition (TrailingChange))
import Convex.MockChain.CoinSelection (tryBalanceAndSubmit)
import Convex.MockChain.Utils (mockchainFails)
import Convex.Utils (failOnError)
import Convex.Wallet.MockWallet qualified as Wallet
import Convex.Wallet.Operator (signTxOperator)
import Convex.Wallet.Operator qualified as Operator
import Data.String (IsString (..))
import PlutusLedgerApi.V3 (CurrencySymbol (..), ScriptHash (..))
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import SmartTokens.Core.Scripts (ScriptTarget (Debug, Production))
import SmartTokens.Test qualified as Test
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Wst.AppError (AppError)
import Wst.Offchain.BuildTx.DirectorySet (InsertNodeArgs (..))
import Wst.Offchain.BuildTx.Failing (BlacklistedTransferPolicy (..))
import Wst.Offchain.BuildTx.Utils (addConwayStakeCredentialCertificate)
import Wst.Offchain.Endpoints.Deployment qualified as Endpoints
import Wst.Offchain.Env (DirectoryScriptRoot)
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query qualified as Query
import Wst.Offchain.Scripts qualified as Scripts
import Wst.Test.Env (admin, asAdmin, asWallet, user)

tests :: TestTree
tests = testGroup "unit tests"
  [ scriptTargetTests Debug
  , scriptTargetTests Production
  ]

scriptTargetTests :: ScriptTarget -> TestTree
scriptTargetTests target =
  testGroup (fromString $ show target)
    [ testCase "deploy directory and global params" (Test.mockchainSucceedsWithTarget target deployDirectorySet)
    , testGroup "issue programmable tokens"
        [ testCase "always succeeds validator" (Test.mockchainSucceedsWithTarget target $ deployDirectorySet >>= issueAlwaysSucceedsValidator)
        , testCase "smart token issuance" (Test.mockchainSucceedsWithTarget target issueSmartTokensScenario)
        , testCase "smart token transfer" (Test.mockchainSucceedsWithTarget target $ deployDirectorySet >>= transferSmartTokens)
        , testCase "blacklist credential" (Test.mockchainSucceedsWithTarget target $ void $ deployDirectorySet >>= blacklistCredential)
        , testCase "unblacklist credential" (Test.mockchainSucceedsWithTarget target $ void $ deployDirectorySet >>= unblacklistCredential)
        , testCase "blacklisted transfer" (mockchainFails (blacklistTransfer DontSubmitFailingTx) Test.assertBlacklistedAddressException)
        , testCase "blacklisted transfer (failing tx)" (Test.mockchainSucceedsWithTarget target (blacklistTransfer SubmitFailingTx >>= Test.assertFailingTx))
        , testCase "seize user output" (Test.mockchainSucceedsWithTarget target $ deployDirectorySet >>= seizeUserOutput)
        , testCase "deploy all" (Test.mockchainSucceedsWithTarget target deployAll)
        ]
    ]

deployAll :: (MonadReader ScriptTarget m, MonadUtxoQuery m, MonadBlockchain C.ConwayEra m, MonadFail m) => m ()
deployAll = do
  target <- ask
  failOnError @_ @(AppError C.ConwayEra) $ Env.withEnv $ do
    asAdmin @C.ConwayEra $ Endpoints.frackUtxosTx
      >>= void . sendTx . signTxOperator admin

    asAdmin @C.ConwayEra $ do
      (tx, scriptRoot) <- Endpoints.deployFullTx target
      void $ sendTx $ signTxOperator admin tx
      Env.withDirectoryFor scriptRoot $ do
        Query.registryNodes @C.ConwayEra
          >>= void . Test.expectSingleton "registry output"
        void $ Query.globalParamsNode @C.ConwayEra

deployDirectorySet :: (MonadReader ScriptTarget m, MonadUtxoQuery m, MonadBlockchain C.ConwayEra m, MonadFail m) => m DirectoryScriptRoot
deployDirectorySet = do
  target <- ask
  failOnError @_ @(AppError C.ConwayEra) $ Env.withEnv $ do
    asAdmin @C.ConwayEra $ Endpoints.frackUtxosTx
      >>= void . sendTx . signTxOperator admin

    dirScriptRoot <- asAdmin @C.ConwayEra $ do
      (tx, scriptRoot) <- Endpoints.deployTx target
      void $ sendTx $ signTxOperator admin tx
      Env.withDirectoryFor scriptRoot $ do
        Query.registryNodes @C.ConwayEra
          >>= void . Test.expectSingleton "registry output"
        void $ Query.globalParamsNode @C.ConwayEra
      pure scriptRoot

    asAdmin @C.ConwayEra $ Env.withDirectoryFor dirScriptRoot $ do
      Endpoints.deployIssuanceCborHex
        >>= void . sendTx . signTxOperator admin
      void $ Query.issuanceCborHexUTxO @C.ConwayEra
      pure dirScriptRoot

_insertDirectoryNode :: (MonadUtxoQuery m, MonadBlockchain C.ConwayEra m, MonadFail m) => DirectoryScriptRoot -> m ()
_insertDirectoryNode scriptRoot = failOnError @_ @(AppError C.ConwayEra) $ Env.withEnv $ asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ do
  Endpoints.insertNodeTx _dummyNodeArgs >>= void . sendTx . signTxOperator admin
  Query.registryNodes @C.ConwayEra
    >>= void . Test.expectN 2 "registry outputs"

{-| Issue some tokens with the "always succeeds" validator
-}
issueAlwaysSucceedsValidator :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => DirectoryScriptRoot -> m ()
issueAlwaysSucceedsValidator scriptRoot = failOnError @_ @(AppError C.ConwayEra) $ Env.withEnv $ do

  -- Register the stake validator
  -- Oddly, the tests passes even if we don't do this.
  -- But I'll leave it in because it seems right.
  registerAlwaysSucceedsStakingCert

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransfer (Env.alwaysSucceedsTransferLogic Production) $ do
    Endpoints.issueProgrammableTokenTx "dummy asset" 100
      >>= void . sendTx . signTxOperator admin
    Query.registryNodes @C.ConwayEra
      >>= void . Test.expectN 2 "registry outputs"
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . Test.expectN 1 "programmable logic outputs"

issueSmartTokensScenario :: (MonadReader ScriptTarget m, MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => m C.AssetId
issueSmartTokensScenario = deployDirectorySet >>= issueTransferLogicProgrammableToken

{-| Issue some tokens with the smart stablecoin transfer logic validator
-}
issueTransferLogicProgrammableToken :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => DirectoryScriptRoot -> m C.AssetId
issueTransferLogicProgrammableToken scriptRoot = failOnError @_ @(AppError C.ConwayEra) $ Env.withEnv $ do

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
    -- register programmable global stake script
    void $ registerTransferScripts opPkh

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)

    (balTx, aid) <- Endpoints.issueSmartTokensTx "dummy asset" 100 (C.PaymentCredentialByKey opPkh)
    void $ sendTx $ signTxOperator admin balTx

    Query.registryNodes @C.ConwayEra
      >>= void . Test.expectN 2 " registry outputs"
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . Test.expectN 1 "programmable logic outputs"
    pure aid

{-| Issue some tokens with the smart stablecoin transfer logic validator
-}
transferSmartTokens :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => DirectoryScriptRoot -> m ()
transferSmartTokens scriptRoot = failOnError @_ @(AppError C.ConwayEra) $ Env.withEnv $ do
  userPkh <- asWallet Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv)

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    Endpoints.deployBlacklistTx
      >>= void . sendTx . signTxOperator admin
    Query.blacklistNodes @C.ConwayEra
      >>= void . Test.expectSingleton "blacklist output"

  aid <- issueTransferLogicProgrammableToken scriptRoot

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)

    Endpoints.transferSmartTokensTx DontSubmitFailingTx aid 80 (C.PaymentCredentialByKey userPkh)
      >>= void . sendTx . signTxOperator admin

    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . Test.expectN 2 "programmable logic outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey userPkh)
      >>= void . Test.expectN 1 "user programmable outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey opPkh)
      >>= void . Test.expectN 1 "user programmable outputs"

blacklistCredential :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => DirectoryScriptRoot -> m C.PaymentCredential
blacklistCredential scriptRoot = failOnError @_ @(AppError C.ConwayEra) $ Env.withEnv $ do
  userPkh <- asWallet Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv)
  let paymentCred = C.PaymentCredentialByKey userPkh

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    Endpoints.deployBlacklistTx
      >>= void . sendTx . signTxOperator admin
    Query.blacklistNodes @C.ConwayEra
      >>= void . Test.expectSingleton "blacklist output"

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    Endpoints.insertBlacklistNodeTx "" paymentCred
      >>= void . sendTx . signTxOperator admin

    Query.blacklistNodes @C.ConwayEra
      >>= void . Test.expectN 2 "blacklist output"

  pure paymentCred

unblacklistCredential :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => DirectoryScriptRoot -> m C.PaymentCredential
unblacklistCredential scriptRoot = failOnError @_ @(AppError C.ConwayEra) $ Env.withEnv $ do
  userPkh <- asWallet Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv)
  let paymentCred = C.PaymentCredentialByKey userPkh

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    Endpoints.deployBlacklistTx
      >>= void . sendTx . signTxOperator admin
    Query.blacklistNodes @C.ConwayEra
      >>= void . Test.expectSingleton "blacklist output"

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    Endpoints.insertBlacklistNodeTx "" paymentCred
      >>= void . sendTx . signTxOperator admin

    Query.blacklistNodes @C.ConwayEra
      >>= void . Test.expectN 2 "blacklist output"

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    Endpoints.removeBlacklistNodeTx paymentCred
      >>= void . sendTx . signTxOperator admin
    Query.blacklistNodes @C.ConwayEra
      >>= void . Test.expectSingleton "blacklist output"

  pure paymentCred

blacklistTransfer :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => BlacklistedTransferPolicy -> m (Either (ValidationError C.ConwayEra) C.TxId)
blacklistTransfer policy = failOnError @_ @(AppError C.ConwayEra) $ Env.withEnv $ do
  scriptRoot <- runReaderT deployDirectorySet Production
  userPkh <- asWallet Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv)
  let userPaymentCred = C.PaymentCredentialByKey userPkh

  aid <- issueTransferLogicProgrammableToken scriptRoot

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ Endpoints.deployBlacklistTx
    >>= void . sendTx . signTxOperator admin

  opPkh <- asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
    Endpoints.transferSmartTokensTx policy aid 50 (C.PaymentCredentialByKey userPkh)
      >>= void . sendTx . signTxOperator admin
    pure opPkh

  transferLogic <- Env.withDirectoryFor scriptRoot $ Env.transferLogicForDirectory (C.verificationKeyHash . Operator.verificationKey . Operator.oPaymentKey $ admin)

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ Endpoints.insertBlacklistNodeTx "" userPaymentCred
    >>= void . sendTx . signTxOperator admin

  asWallet Wallet.w2 $ Env.withDirectoryFor scriptRoot $ Env.withTransfer transferLogic $ Endpoints.transferSmartTokensTx policy aid 30 (C.PaymentCredentialByKey opPkh)
    >>= sendTx . signTxOperator (user Wallet.w2)

seizeUserOutput :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => DirectoryScriptRoot -> m ()
seizeUserOutput scriptRoot = failOnError @_ @(AppError C.ConwayEra) $ Env.withEnv $ do
  userPkh <- asWallet Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv)
  let userPaymentCred = C.PaymentCredentialByKey userPkh

  aid <- issueTransferLogicProgrammableToken scriptRoot

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ Endpoints.deployBlacklistTx
    >>= void . sendTx . signTxOperator admin

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    Endpoints.transferSmartTokensTx DontSubmitFailingTx aid 50 (C.PaymentCredentialByKey userPkh)
      >>= void . sendTx . signTxOperator admin
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . Test.expectN 2 "programmable logic outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey userPkh)
      >>= void . Test.expectN 1 "user programmable outputs"

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
    Endpoints.seizeCredentialAssetsTx mempty userPaymentCred
      >>= void . sendTx . signTxOperator admin
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . Test.expectN 3 "programmable logic outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey userPkh)
      >>= void . Test.expectN 1 "user programmable outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey opPkh)
      >>= void . Test.expectN 2 "user programmable outputs"

_dummyNodeArgs :: InsertNodeArgs
_dummyNodeArgs =
  InsertNodeArgs
    { inaNewKey = CurrencySymbol (stringToBuiltinByteStringHex "e165610232235bbbbeff5b998b23e165610232235bbbbeff5b998b23")
    , inaHashedParam = ScriptHash (stringToBuiltinByteStringHex "e165610232235bbbbeff5b998b23e165610232235bbbbeff5b998b23")
    , inaTransferLogic = C.StakeCredentialByScript "e165610232235bbbbeff5b998b23e165610232235bbbbeff5b998b23"
    , inaIssuerLogic   = C.StakeCredentialByScript "e165610232235bbbbeff5b998b23e165610232235bbbbeff5b998b23"
    , inaGlobalStateCS = Nothing
    }

{-| Register the 'alwaysSucceedsScript' stake validator
-}
registerAlwaysSucceedsStakingCert :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) =>  m ()
registerAlwaysSucceedsStakingCert = failOnError $ do
  let script = C.PlutusScript C.plutusScriptVersion $ Scripts.alwaysSucceedsScript Production
      hsh = C.hashScript script
      cred = C.StakeCredentialByScript hsh
  txBody <- BuildTx.execBuildTxT $ do
    cert <- BuildTx.mkConwayStakeCredentialRegistrationCertificate cred
    BuildTx.addStakeScriptWitness cert cred (Scripts.alwaysSucceedsScript Production) ()
  void (tryBalanceAndSubmit mempty Wallet.w1 txBody TrailingChange [])

-- TODO: registration to be moved to the endpoints
registerTransferScripts :: (MonadFail m, MonadReader env m, Env.HasTransferLogicEnv env, MonadMockchain C.ConwayEra m) => C.Hash C.PaymentKey -> m C.TxId
registerTransferScripts pkh = failOnError $ do
  transferMintingScript <- asks (Env.tleMintingScript . Env.transferLogicEnv)
  transferSpendingScript <- asks (Env.tleTransferScript . Env.transferLogicEnv)
  transferSeizeSpendingScript <- asks (Env.tleIssuerScript . Env.transferLogicEnv)

  let
      hshMinting = C.hashScript $ C.PlutusScript C.plutusScriptVersion transferMintingScript
      credMinting = C.StakeCredentialByScript hshMinting

      hshSpending = C.hashScript $ C.PlutusScript C.plutusScriptVersion transferSpendingScript
      credSpending = C.StakeCredentialByScript hshSpending

      hshSeizeSpending = C.hashScript $ C.PlutusScript C.plutusScriptVersion transferSeizeSpendingScript
      credSeizeSpending = C.StakeCredentialByScript hshSeizeSpending


  txBody <- BuildTx.execBuildTxT $ do

    addConwayStakeCredentialCertificate credSpending
    addConwayStakeCredentialCertificate credMinting
    addConwayStakeCredentialCertificate credSeizeSpending

    BuildTx.addRequiredSignature pkh

  x <- tryBalanceAndSubmit mempty Wallet.w1 txBody TrailingChange []
  pure $ C.getTxId $ C.getTxBody x
