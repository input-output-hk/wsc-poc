{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Wst.Test.UnitTest(
  tests
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Control.Lens ((%~), (&), (^.))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain (queryProtocolParameters, sendTx),
                     MonadMockchain, MonadUtxoQuery, ValidationError, getTxById)
import Convex.CoinSelection (ChangeOutputPosition (TrailingChange))
import Convex.MockChain (MockchainT)
import Convex.MockChain.CoinSelection (tryBalanceAndSubmit)
import Convex.MockChain.Defaults qualified as Defaults
import Convex.MockChain.Utils (mockchainFails, mockchainSucceedsWith)
import Convex.NodeParams (NodeParams, ledgerProtocolParameters,
                          protocolParameters)
import Convex.Utils (failOnError)
import Convex.Wallet.MockWallet qualified as Wallet
import Convex.Wallet.Operator (signTxOperator)
import Convex.Wallet.Operator qualified as Operator
import Data.List (isPrefixOf)
import Data.String (IsString (..))
import GHC.Exception (SomeException, throw)
import PlutusLedgerApi.V3 (CurrencySymbol (..))
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import SmartTokens.Core.Scripts (ScriptTarget (Debug, Production))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
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
    [ testCase "deploy directory and global params" (mockchainSucceedsWithTarget target deployDirectorySet)
    , testCase "insert directory node" (mockchainSucceedsWithTarget target $ deployDirectorySet >>= insertDirectoryNode)
    , testGroup "issue programmable tokens"
        [ testCase "always succeeds validator" (mockchainSucceedsWithTarget target $ deployDirectorySet >>= issueAlwaysSucceedsValidator)
        , testCase "smart token issuance" (mockchainSucceedsWithTarget target issueSmartTokensScenario)
        , testCase "smart token transfer" (mockchainSucceedsWithTarget target $ deployDirectorySet >>= transferSmartTokens)
        , testCase "blacklist credential" (mockchainSucceedsWithTarget target $ void $ deployDirectorySet >>= blacklistCredential)
        , testCase "unblacklist credential" (mockchainSucceedsWithTarget target $ void $ deployDirectorySet >>= unblacklistCredential)
        , testCase "blacklisted transfer" (mockchainFails (blacklistTransfer DontSubmitFailingTx) assertBlacklistedAddressException)
        , testCase "blacklisted transfer (failing tx)" (mockchainSucceedsWithTarget target (blacklistTransfer SubmitFailingTx >>= assertFailingTx))
        , testCase "seize user output" (mockchainSucceedsWithTarget target $ deployDirectorySet >>= seizeUserOutput)
        , testCase "deploy all" (mockchainSucceedsWithTarget target deployAll)
        ]
    ]

deployAll :: (MonadReader ScriptTarget m, MonadUtxoQuery m, MonadBlockchain C.ConwayEra m, MonadFail m) => m ()
deployAll = do
  target <- ask
  failOnError $ Env.withEnv $ asAdmin @C.ConwayEra $ do
    (tx, scriptRoot) <- Endpoints.deployFullTx target
    void $ sendTx $ signTxOperator admin tx
    Env.withDirectoryFor scriptRoot $ do
      Query.registryNodes @C.ConwayEra
        >>= void . expectSingleton "registry output"
      void $ Query.globalParamsNode @C.ConwayEra


deployDirectorySet :: (MonadReader ScriptTarget m, MonadUtxoQuery m, MonadBlockchain C.ConwayEra m, MonadFail m) => m DirectoryScriptRoot
deployDirectorySet = do
  target <- ask
  failOnError $ Env.withEnv $ asAdmin @C.ConwayEra $ do
    (tx, scriptRoot) <- Endpoints.deployTx target
    void $ sendTx $ signTxOperator admin tx
    Env.withDirectoryFor scriptRoot $ do
      Query.registryNodes @C.ConwayEra
        >>= void . expectSingleton "registry output"
      void $ Query.globalParamsNode @C.ConwayEra
    pure scriptRoot

insertDirectoryNode :: (MonadUtxoQuery m, MonadBlockchain C.ConwayEra m, MonadFail m) => DirectoryScriptRoot -> m ()
insertDirectoryNode scriptRoot = failOnError $ Env.withEnv $ asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ do
  Endpoints.insertNodeTx dummyNodeArgs >>= void . sendTx . signTxOperator admin
  Query.registryNodes @C.ConwayEra
    >>= void . expectN 2 "registry outputs"

{-| Issue some tokens with the "always succeeds" validator
-}
issueAlwaysSucceedsValidator :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => DirectoryScriptRoot -> m ()
issueAlwaysSucceedsValidator scriptRoot = failOnError $ Env.withEnv $ do

  -- Register the stake validator
  -- Oddly, the tests passes even if we don't do this.
  -- But I'll leave it in because it seems right.
  registerAlwaysSucceedsStakingCert

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransfer (Env.alwaysSucceedsTransferLogic Production) $ do
    Endpoints.issueProgrammableTokenTx "dummy asset" 100
      >>= void . sendTx . signTxOperator admin
    Query.registryNodes @C.ConwayEra
      >>= void . expectN 2 "registry outputs"
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . expectN 1 "programmable logic outputs"

issueSmartTokensScenario :: (MonadReader ScriptTarget m, MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => m C.AssetId
issueSmartTokensScenario = deployDirectorySet >>= issueTransferLogicProgrammableToken

{-| Issue some tokens with the smart stabelcoin transfer logic validator
-}
issueTransferLogicProgrammableToken :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => DirectoryScriptRoot -> m C.AssetId
issueTransferLogicProgrammableToken scriptRoot = failOnError $ Env.withEnv $ do

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
    -- register programmable global stake script
    void $ registerTransferScripts opPkh

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
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
transferSmartTokens :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => DirectoryScriptRoot -> m ()
transferSmartTokens scriptRoot = failOnError $ Env.withEnv $ do
  userPkh <- asWallet Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv)

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    Endpoints.deployBlacklistTx
      >>= void . sendTx . signTxOperator admin
    Query.blacklistNodes @C.ConwayEra
      >>= void . expectSingleton "blacklist output"

  aid <- issueTransferLogicProgrammableToken scriptRoot

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)

    Endpoints.transferSmartTokensTx DontSubmitFailingTx aid 80 (C.PaymentCredentialByKey userPkh)
      >>= void . sendTx . signTxOperator admin

    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . expectN 2 "programmable logic outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey userPkh)
      >>= void . expectN 1 "user programmable outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey opPkh)
      >>= void . expectN 1 "user programmable outputs"

blacklistCredential :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => DirectoryScriptRoot -> m C.PaymentCredential
blacklistCredential scriptRoot = failOnError $ Env.withEnv $ do
  userPkh <- asWallet Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv)
  let paymentCred = C.PaymentCredentialByKey userPkh

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    Endpoints.deployBlacklistTx
      >>= void . sendTx . signTxOperator admin
    Query.blacklistNodes @C.ConwayEra
      >>= void . expectSingleton "blacklist output"

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    Endpoints.insertBlacklistNodeTx "" paymentCred
      >>= void . sendTx . signTxOperator admin

    Query.blacklistNodes @C.ConwayEra
      >>= void . expectN 2 "blacklist output"

  pure paymentCred

unblacklistCredential :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => DirectoryScriptRoot -> m C.PaymentCredential
unblacklistCredential scriptRoot = failOnError $ Env.withEnv $ do
  userPkh <- asWallet Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv)
  let paymentCred = C.PaymentCredentialByKey userPkh

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    Endpoints.deployBlacklistTx
      >>= void . sendTx . signTxOperator admin
    Query.blacklistNodes @C.ConwayEra
      >>= void . expectSingleton "blacklist output"

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    Endpoints.insertBlacklistNodeTx "" paymentCred
      >>= void . sendTx . signTxOperator admin

    Query.blacklistNodes @C.ConwayEra
      >>= void . expectN 2 "blacklist output"

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    Endpoints.removeBlacklistNodeTx paymentCred
      >>= void . sendTx . signTxOperator admin
    Query.blacklistNodes @C.ConwayEra
      >>= void . expectSingleton "blacklist output"

  pure paymentCred

blacklistTransfer :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) => BlacklistedTransferPolicy -> m (Either (ValidationError C.ConwayEra) C.TxId)
blacklistTransfer policy = failOnError $ Env.withEnv $ do
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
seizeUserOutput scriptRoot = failOnError $ Env.withEnv $ do
  userPkh <- asWallet Wallet.w2 $ asks (fst . Env.bteOperator . Env.operatorEnv)
  let userPaymentCred = C.PaymentCredentialByKey userPkh

  aid <- issueTransferLogicProgrammableToken scriptRoot

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ Endpoints.deployBlacklistTx
    >>= void . sendTx . signTxOperator admin

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    Endpoints.transferSmartTokensTx DontSubmitFailingTx aid 50 (C.PaymentCredentialByKey userPkh)
      >>= void . sendTx . signTxOperator admin
    Query.programmableLogicOutputs @C.ConwayEra
      >>= void . expectN 2 "programmable logic outputs"
    Query.userProgrammableOutputs (C.PaymentCredentialByKey userPkh)
      >>= void . expectN 1 "user programmable outputs"

  asAdmin @C.ConwayEra $ Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator $ do
    opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
    Endpoints.seizeCredentialAssetsTx mempty userPaymentCred
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
    { inaNewKey = CurrencySymbol (stringToBuiltinByteStringHex "e165610232235bbbbeff5b998b23e165610232235bbbbeff5b998b23")
    , inaTransferLogic = C.StakeCredentialByScript "e165610232235bbbbeff5b998b23e165610232235bbbbeff5b998b23"
    , inaIssuerLogic = C.StakeCredentialByScript "e165610232235bbbbeff5b998b23e165610232235bbbbeff5b998b23"
    , inaGlobalStateCS = CurrencySymbol ""
    }

{-| Register the 'alwaysSucceedsScript' stake validator
-}
registerAlwaysSucceedsStakingCert :: (MonadUtxoQuery m, MonadFail m, MonadMockchain C.ConwayEra m) =>  m ()
registerAlwaysSucceedsStakingCert = failOnError $ do
  pp <- fmap C.unLedgerProtocolParameters queryProtocolParameters
  let script = C.PlutusScript C.plutusScriptVersion $ Scripts.alwaysSucceedsScript Production
      hsh = C.hashScript script
      cred = C.StakeCredentialByScript hsh
  txBody <- BuildTx.execBuildTxT $ do
    BuildTx.addStakeScriptWitness cred (Scripts.alwaysSucceedsScript Production) ()
    BuildTx.addConwayStakeCredentialRegistrationCertificate cred (pp ^. Ledger.ppKeyDepositL)
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

nodeParamsFor :: ScriptTarget -> NodeParams C.ConwayEra
nodeParamsFor = \case
  -- Run the 'Mockchain' action with modified node parameters to allow larger-than-usual
  -- transactions. This is useful for showing debug output from the scripts and fail if there is an error
  Debug ->
    let tenX ExUnits{exUnitsSteps=steps, exUnitsMem=mem} =
          ExUnits{exUnitsSteps = 10 * steps, exUnitsMem = 10 * mem}
    in Defaults.nodeParams
        & ledgerProtocolParameters . protocolParameters . Ledger.ppMaxTxSizeL %~ (*10)
        & ledgerProtocolParameters . protocolParameters . Ledger.ppMaxTxExUnitsL %~ tenX
  Production -> Defaults.nodeParams

mockchainSucceedsWithTarget :: ScriptTarget -> ReaderT ScriptTarget (MockchainT C.ConwayEra IO) a -> Assertion
mockchainSucceedsWithTarget target =
  mockchainSucceedsWith (nodeParamsFor target) . flip runReaderT target

{-| Assert that the transaction exists on the mockchain and that its script validity flag
is set to 'C.ScriptInvalid'
-}
assertFailingTx :: (MonadMockchain era m, C.IsAlonzoBasedEra era, MonadFail m, MonadIO m) => Either (ValidationError era) C.TxId -> m ()
assertFailingTx = \case
  Left err  -> fail $ "Expected TxId, got: " <> show err
  Right txId -> do
    C.TxBody C.TxBodyContent{C.txScriptValidity} <- getTxById txId >>= maybe (fail $ "Tx not found: " <> show txId) (pure . C.getTxBody)
    liftIO (assertEqual "Tx validity" (C.TxScriptValidity C.alonzoBasedEra C.ScriptInvalid) txScriptValidity)
