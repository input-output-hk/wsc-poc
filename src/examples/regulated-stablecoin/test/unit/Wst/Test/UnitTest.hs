{-# LANGUAGE OverloadedStrings #-}
module Wst.Test.UnitTest(
  tests
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad (void, when)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain (sendTx), MonadMockchain, MonadUtxoQuery,
                     ValidationError)
import Convex.CoinSelection (ChangeOutputPosition (TrailingChange))
import Convex.MockChain.CoinSelection (tryBalanceAndSubmit)
import Convex.MockChain.Utils (mockchainFails)
import Convex.Utils (failOnError, mapError)
import Convex.Wallet.MockWallet qualified as Wallet
import Convex.Wallet.Operator (signTxOperator)
import Convex.Wallet.Operator qualified as Operator
import Data.List (isPrefixOf)
import Data.String (IsString (..))
import GHC.Exception (SomeException, throw)
import ProgrammableTokens.OffChain.Endpoints qualified as Endpoints
import ProgrammableTokens.OffChain.Env (programmableTokenPolicyId)
import ProgrammableTokens.OffChain.Env.Operator qualified as Env
import ProgrammableTokens.OffChain.Query qualified as Query
import ProgrammableTokens.Test (deployDirectorySet)
import ProgrammableTokens.Test qualified as Test
import SmartTokens.Core.Scripts (ScriptTarget (Debug, Production))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import Wst.AppError (AppError (BalancingError))
import Wst.Offchain.BuildTx.Failing (BlacklistedTransferPolicy (..))
import Wst.Offchain.BuildTx.Utils (addConwayStakeCredentialCertificate)
import Wst.Offchain.Endpoints.Deployment qualified as Endpoints
import Wst.Offchain.Env (DirectoryScriptRoot)
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query qualified as Query
import Wst.Test.Env (admin, asAdmin, asWallet, user)

tests :: TestTree
tests = testGroup "unit tests"
  [ scriptTargetTests Debug
  , scriptTargetTests Production
  ]

scriptTargetTests :: ScriptTarget -> TestTree
scriptTargetTests target =
  testGroup (fromString $ show target)
    [ testGroup "issue programmable tokens"
        [ testCase "smart token issuance" (Test.mockchainSucceedsWithTarget @(AppError C.ConwayEra) target issueSmartTokensScenario)
        , testCase "smart token transfer" (Test.mockchainSucceedsWithTarget @(AppError C.ConwayEra) target $ deployDirectorySet admin >>= transferSmartTokens)
        , testCase "blacklist credential" (Test.mockchainSucceedsWithTarget @(AppError C.ConwayEra) target $ void $ deployDirectorySet admin >>= blacklistCredential)
        , testCase "unblacklist credential" (Test.mockchainSucceedsWithTarget @(AppError C.ConwayEra) target $ void $ deployDirectorySet admin >>= unblacklistCredential)
        , testCase "blacklisted transfer" (mockchainFails (blacklistTransfer DontSubmitFailingTx) assertBlacklistedAddressException)
        , testCase "blacklisted transfer (failing tx)" (Test.mockchainSucceedsWithTarget @(AppError C.ConwayEra) target (blacklistTransfer SubmitFailingTx >>= Test.assertFailingTx))
        , testCase "seize user output" (Test.mockchainSucceedsWithTarget @(AppError C.ConwayEra) target $ deployDirectorySet admin >>= seizeUserOutput)
        , testCase "seize multi user outputs" (Test.mockchainSucceedsWithTarget @(AppError C.ConwayEra) target $ deployDirectorySet admin >>= seizeMultiUserOutputs)
        , testCase "deploy all" (Test.mockchainSucceedsWithTarget @(AppError C.ConwayEra) target deployAll)
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

issueSmartTokensScenario :: (MonadFail m, MonadReader ScriptTarget m, MonadUtxoQuery m, MonadError (AppError C.ConwayEra) m, MonadMockchain C.ConwayEra m) => m C.AssetId
issueSmartTokensScenario = deployDirectorySet admin >>= issueTransferLogicProgrammableToken

{-| Issue some tokens with the smart stablecoin transfer logic validator
-}
issueTransferLogicProgrammableToken :: (MonadUtxoQuery m, MonadError (AppError C.ConwayEra) m, MonadMockchain C.ConwayEra m, MonadFail m) => DirectoryScriptRoot -> m C.AssetId
issueTransferLogicProgrammableToken scriptRoot = Env.withEnv $ do

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv @C.ConwayEra)
    -- register programmable global stake script
    void $ registerTransferScripts opPkh

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv @C.ConwayEra)

    (balTx, aid) <- Endpoints.issueSmartTokensTx "dummy asset" 100 (C.PaymentCredentialByKey opPkh)
    void $ sendTx $ signTxOperator admin balTx

    Query.registryNodes @C.ConwayEra
      >>= void . Test.expectN 2 " registry outputs"
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . Test.expectN 1 "programmable logic outputs"
    pure aid

{-| Issue some tokens with the smart stablecoin transfer logic validator
-}
transferSmartTokens :: (MonadUtxoQuery m, MonadError (AppError C.ConwayEra) m, MonadFail m, MonadMockchain C.ConwayEra m) => DirectoryScriptRoot -> m ()
transferSmartTokens scriptRoot = Env.withEnv $ do
  userPkh <- asWallet @C.ConwayEra Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv @C.ConwayEra)

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ do
    Endpoints.deployBlacklistTx
      >>= void . sendTx . signTxOperator admin
    Query.blacklistNodes @C.ConwayEra
      >>= void . Test.expectSingleton "blacklist output"

  aid <- issueTransferLogicProgrammableToken scriptRoot

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv @C.ConwayEra)

    Endpoints.transferSmartTokensTx DontSubmitFailingTx aid 80 (C.PaymentCredentialByKey userPkh)
      >>= void . sendTx . signTxOperator admin

    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . Test.expectN 2 "programmable logic outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey userPkh, Nothing)
      >>= void . Test.expectN 1 "user programmable outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey opPkh, Nothing)
      >>= void . Test.expectN 1 "user programmable outputs"

blacklistCredential :: (MonadUtxoQuery m, MonadFail m, MonadError (AppError C.ConwayEra) m, MonadMockchain C.ConwayEra m) => DirectoryScriptRoot -> m C.PaymentCredential
blacklistCredential scriptRoot = Env.withEnv $ do
  userPkh <- asWallet @C.ConwayEra Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv @C.ConwayEra)
  let paymentCred = C.PaymentCredentialByKey userPkh

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ do
    Endpoints.deployBlacklistTx
      >>= void . sendTx . signTxOperator admin
    Query.blacklistNodes @C.ConwayEra
      >>= void . Test.expectSingleton "blacklist output"

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ do
    Endpoints.insertBlacklistNodeTx "" paymentCred
      >>= void . sendTx . signTxOperator admin

    Query.blacklistNodes @C.ConwayEra
      >>= void . Test.expectN 2 "blacklist output"

  pure paymentCred

unblacklistCredential :: (MonadUtxoQuery m, MonadFail m, MonadError (AppError C.ConwayEra) m, MonadMockchain C.ConwayEra m) => DirectoryScriptRoot -> m C.PaymentCredential
unblacklistCredential scriptRoot = Env.withEnv $ do
  userPkh <- asWallet @C.ConwayEra Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv @C.ConwayEra)
  let paymentCred = C.PaymentCredentialByKey userPkh

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ do
    Endpoints.deployBlacklistTx
      >>= void . sendTx . signTxOperator admin
    Query.blacklistNodes @C.ConwayEra
      >>= void . Test.expectSingleton "blacklist output"

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ do
    Endpoints.insertBlacklistNodeTx "" paymentCred
      >>= void . sendTx . signTxOperator admin

    Query.blacklistNodes @C.ConwayEra
      >>= void . Test.expectN 2 "blacklist output"

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ do
    Endpoints.removeBlacklistNodeTx paymentCred
      >>= void . sendTx . signTxOperator admin
    Query.blacklistNodes @C.ConwayEra
      >>= void . Test.expectSingleton "blacklist output"

  pure paymentCred

blacklistTransfer :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => BlacklistedTransferPolicy -> m (Either (ValidationError C.ConwayEra) C.TxId)
blacklistTransfer policy = failOnError @_ @(AppError C.ConwayEra) $ Env.withEnv $ do
  scriptRoot <- runReaderT (deployDirectorySet admin) Production
  userPkh <- asWallet @C.ConwayEra Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv @C.ConwayEra)
  let userPaymentCred = C.PaymentCredentialByKey userPkh

  aid <- issueTransferLogicProgrammableToken scriptRoot

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ Endpoints.deployBlacklistTx
    >>= void . sendTx . signTxOperator admin

  opPkh <- asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv @C.ConwayEra)
    Endpoints.transferSmartTokensTx policy aid 50 (C.PaymentCredentialByKey userPkh)
      >>= void . sendTx . signTxOperator admin
    pure opPkh

  (transferLogic, ble) <- Env.withDirectoryFor scriptRoot $ Env.transferLogicForDirectory (C.verificationKeyHash . Operator.verificationKey . Operator.oPaymentKey $ admin) Nothing

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ Endpoints.insertBlacklistNodeTx "" userPaymentCred
    >>= void . sendTx . signTxOperator admin

  asWallet @C.ConwayEra Wallet.w2 $ Env.withDirectoryFor scriptRoot $ Env.withBlacklist ble $ Env.withTransfer transferLogic $ Endpoints.transferSmartTokensTx policy aid 30 (C.PaymentCredentialByKey opPkh)
    >>= sendTx . signTxOperator (user Wallet.w2)

seizeUserOutput :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m, MonadError (AppError C.ConwayEra) m) => DirectoryScriptRoot -> m ()
seizeUserOutput scriptRoot = Env.withEnv $ do
  userPkh <- asWallet @C.ConwayEra Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv @C.ConwayEra)
  let userPaymentCred = C.PaymentCredentialByKey userPkh

  aid <- issueTransferLogicProgrammableToken scriptRoot

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ Endpoints.deployBlacklistTx
    >>= void . sendTx . signTxOperator admin

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ do
    Endpoints.transferSmartTokensTx DontSubmitFailingTx aid 50 (C.PaymentCredentialByKey userPkh)
      >>= void . sendTx . signTxOperator admin
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . Test.expectN 2 "programmable logic outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey userPkh, Nothing)
      >>= void . Test.expectN 1 "user programmable outputs"

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv @C.ConwayEra)
    Endpoints.seizeCredentialAssetsTx mempty userPaymentCred
      >>= void . sendTx . signTxOperator admin
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . Test.expectN 3 "programmable logic outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey userPkh, Nothing)
      >>= void . Test.expectN 1 "user programmable outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey opPkh, Nothing)
      >>= void . Test.expectN 2 "operator programmable outputs"

seizeMultiUserOutputs ::  (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m, MonadError (AppError C.ConwayEra) m) => DirectoryScriptRoot -> m ()
seizeMultiUserOutputs scriptRoot = Env.withEnv $ do
  userPkh <- asWallet @C.ConwayEra Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv @C.ConwayEra)
  let userPaymentCred = C.PaymentCredentialByKey userPkh

  aid <- issueTransferLogicProgrammableToken scriptRoot

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ Endpoints.deployBlacklistTx
    >>= void . sendTx . signTxOperator admin

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ do
    Endpoints.transferSmartTokensTx DontSubmitFailingTx aid 50 (C.PaymentCredentialByKey userPkh)
      >>= void . sendTx . signTxOperator admin
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . Test.expectN 2 "programmable logic outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey userPkh, Nothing)
      >>= void . Test.expectN 1 "user programmable outputs"

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ do
    Endpoints.transferSmartTokensTx DontSubmitFailingTx aid 50 (C.PaymentCredentialByKey userPkh)
      >>= void . sendTx . signTxOperator admin
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . Test.expectN 3 "programmable logic outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey userPkh, Nothing)
      >>= void . Test.expectN 2 "user programmable outputs"

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv @C.ConwayEra)
    toSeizePolicyId <- asks programmableTokenPolicyId
    Endpoints.seizeMultiCredentialAssetsTx mempty 2 [userPaymentCred]
      >>= void . sendTx . signTxOperator admin
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . Test.expectN 4 "programmable logic outputs"
    userOutputs <- Query.userProgrammableOutputs (C.PaymentCredentialByKey userPkh, Nothing)
    Test.expectN 2 "user programmable outputs" userOutputs
    mapM_ (\utxo -> when (Query.utxoHasPolicyId toSeizePolicyId utxo) $ fail "User should not have any UTxOs with the programmable token policy ID") userOutputs

    Query.userProgrammableOutputs (C.PaymentCredentialByKey opPkh, Nothing)
      >>= void . Test.expectN 2 "operator programmable outputs"

-- TODO: registration to be moved to the endpoints
registerTransferScripts :: (MonadFail m, MonadError (AppError C.ConwayEra) m, MonadReader env m, Env.HasTransferLogicEnv env, MonadMockchain C.ConwayEra m) => C.Hash C.PaymentKey -> m C.TxId
registerTransferScripts pkh = do
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

  x <- mapError BalancingError (tryBalanceAndSubmit mempty Wallet.w1 txBody TrailingChange [])
  pure $ C.getTxId $ C.getTxBody x

-- TODO: Need to make this nicer
{-| Make sure that the exception is a failure due to blacklisted address
-}
assertBlacklistedAddressException :: SomeException -> Assertion
assertBlacklistedAddressException ex
  | "user error (RegStablecoinError (TransferBlacklistedCredential (PubKeyCredential" `isPrefixOf` show ex = pure ()
  | otherwise = throw ex
