{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cardano.Api qualified as C
import Control.Exception (SomeException, displayException, try)
import Control.Monad (void)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (
    MonadBlockchain (queryNetworkId, sendTx),
    MonadMockchain,
    askNodeParams,
    getUtxo,
 )
import Convex.CoinSelection (ChangeOutputPosition (TrailingChange))
import Convex.MockChain (MockchainT, fromLedgerUTxO, getTxExUnits, runMockchain0IOWith)
import Convex.MockChain.CoinSelection (tryBalanceAndSubmit)
import Convex.Utils (mapError)
import Convex.Wallet qualified as ConvexWallet
import Convex.Wallet.MockWallet qualified as MockWallet
import Convex.Wallet.Operator (Operator, Signing, signTxOperator)
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Numeric (showFFloat)
import PlutusLedgerApi.V3 (ExBudget (ExBudget), ExCPU (ExCPU), ExMemory (ExMemory))
import ProgrammableTokens.OffChain.Endpoints qualified as ProgEndpoints
import ProgrammableTokens.OffChain.Env qualified as ProgEnv
import ProgrammableTokens.OffChain.Env.Operator qualified as ProgOperatorEnv
import ProgrammableTokens.Test (
    admin,
    deployDirectorySet,
    nodeParamsFor,
    productionMaxTxExBudget,
 )
import SmartTokens.Core.Scripts (ScriptTarget (Production))
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stdout)
import Wst.AppError (AppError (BalancingError, SubmitError))
import Wst.Offchain.BuildTx.Failing (BlacklistedTransferPolicy (DontSubmitFailingTx))
import Wst.Offchain.BuildTx.Utils (addConwayStakeCredentialCertificate)
import Wst.Offchain.Endpoints.Deployment qualified as Endpoints
import Wst.Offchain.Env (DirectoryScriptRoot)
import Wst.Offchain.Env qualified as Env
import Wst.Test.Env (asAdmin, asWallet)

type BenchM = ExceptT (AppError C.ConwayEra) (ReaderT ScriptTarget (MockchainT C.ConwayEra IO))

data BenchCase = BenchCase
    { bcName :: String
    , bcRun :: BenchM ScenarioReport
    }

data ScenarioResult
    = ScenarioPassed ScenarioReport
    | ScenarioFailed String String

newtype RenderStyle = RenderStyle Bool

data ScenarioReport = ScenarioReport
    { srName :: String
    , srTxId :: C.TxId
    , srRows :: [ScriptRow]
    }

data BenchLabels = BenchLabels
    { blPaymentCreds :: Map.Map C.PaymentCredential String
    , blStakeAddresses :: Map.Map C.StakeAddress String
    , blPolicies :: Map.Map C.PolicyId String
    }

data ScriptRow = ScriptRow
    { rowWitness :: C.ScriptWitnessIndex
    , rowContract :: String
    , rowPurpose :: String
    , rowDetail :: String
    , rowCpu :: Integer
    , rowMem :: Integer
    }

instance Semigroup BenchLabels where
    left <> right =
        BenchLabels
            { blPaymentCreds = Map.union (blPaymentCreds left) (blPaymentCreds right)
            , blStakeAddresses = Map.union (blStakeAddresses left) (blStakeAddresses right)
            , blPolicies = Map.union (blPolicies left) (blPolicies right)
            }

instance Monoid BenchLabels where
    mempty =
        BenchLabels
            { blPaymentCreds = Map.empty
            , blStakeAddresses = Map.empty
            , blPolicies = Map.empty
            }

main :: IO ()
main = do
    renderStyle <- detectRenderStyle
    putStrLn "Regulated stablecoin full-transaction benchmarks (mockchain, Production)"
    let maxCpu = exBudgetCpuInt productionMaxTxExBudget
        maxMem = exBudgetMemInt productionMaxTxExBudget
    putStrLn $
        "Max tx ex units: CPU "
            <> formatUnits maxCpu
            <> " | Mem "
            <> formatUnits maxMem
    putStrLn ""
    results <- traverse runBenchCase benchCases
    mapM_ putStrLn (renderScenarioResults renderStyle results)

runBenchCase :: BenchCase -> IO ScenarioResult
runBenchCase BenchCase{bcName, bcRun} = do
    result <-
        try @SomeException $
            runMockchain0IOWith MockWallet.initialUTxOs (nodeParamsFor Production) $
                runReaderT (runExceptT bcRun) Production
    pure $ case result of
        Left ex -> ScenarioFailed bcName (displayException ex)
        Right (Left err, _state) -> ScenarioFailed bcName (show err)
        Right (Right report, _state) -> ScenarioPassed report

benchCases :: [BenchCase]
benchCases =
    [ BenchCase "deployFullTx" deployFullTxCase
    , BenchCase "issueSmartTokensTx" issueSmartTokensCase
    , BenchCase "deployBlacklistTx" deployBlacklistCase
    , BenchCase "insertBlacklistNodeTx" insertBlacklistNodeCase
    , BenchCase "removeBlacklistNodeTx" removeBlacklistNodeCase
    , BenchCase "transferSmartTokensTx.Spend1Utxo" transferSmartTokensCase
    , BenchCase "transferSmartTokensTx.Spend5Utxos" (transferSmartTokensSpendNUtxosCase 5)
    , BenchCase "transferSmartTokensTx.Spend10Utxos" (transferSmartTokensSpendNUtxosCase 10)
    , BenchCase "transferSmartTokensTx.Spend15Utxos" (transferSmartTokensSpendNUtxosCase 15)
    , BenchCase "seizeCredentialAssetsTx.Seize1Utxo" seizeCredentialAssetsCase
    , BenchCase "seizeMultiCredentialAssetsTx.Seize2Utxos" seizeMultiCredentialAssetsCase
    , BenchCase "seizeMultiCredentialAssetsTx.Seize5Utxos" (seizeMultiCredentialAssetsNUtxosCase 5)
    , BenchCase "seizeMultiCredentialAssetsTx.Seize10Utxos" (seizeMultiCredentialAssetsNUtxosCase 10)
    , BenchCase "seizeMultiCredentialAssetsTx.Seize20Utxos" (seizeMultiCredentialAssetsNUtxosCase 20)
    ]

deployFullTxCase :: BenchM ScenarioReport
deployFullTxCase = do
    target <- ask
    Env.withEnv $ do
        asAdmin @C.ConwayEra $ do
            frackTx <- ProgEndpoints.frackUtxosTx
            void $ submitTxOrThrow (signTxOperator admin frackTx)
        asAdmin @C.ConwayEra $ do
            (tx, scriptRoot) <- Endpoints.deployFullTx target
            labels <- Env.withDirectoryFor scriptRoot $ Env.withTransferFromOperator @C.ConwayEra currentBenchLabels
            benchmarkAndSubmitSignedTx labels "deployFullTx" admin tx

issueSmartTokensCase :: BenchM ScenarioReport
issueSmartTokensCase = do
    scriptRoot <- deployDirectorySet admin
    registerTransferScriptsForDirectory scriptRoot
    Env.withEnv $
        asAdmin @C.ConwayEra $
            Env.withDirectoryFor scriptRoot $
                Env.withTransferFromOperator @C.ConwayEra $ do
                    opPkh <- asks (fst . ProgOperatorEnv.bteOperator . ProgOperatorEnv.operatorEnv @C.ConwayEra)
                    (tx, _aid) <- Endpoints.issueSmartTokensTx (C.UnsafeAssetName "dummy asset") 100 (C.PaymentCredentialByKey opPkh)
                    labels <- currentBenchLabels
                    benchmarkAndSubmitSignedTx labels "issueSmartTokensTx" admin tx

deployBlacklistCase :: BenchM ScenarioReport
deployBlacklistCase = do
    scriptRoot <- deployDirectorySet admin
    Env.withEnv $
        asAdmin @C.ConwayEra $
            Env.withDirectoryFor scriptRoot $
                Env.withTransferFromOperator @C.ConwayEra $ do
                    tx <- Endpoints.deployBlacklistTx
                    labels <- currentBenchLabels
                    benchmarkAndSubmitSignedTx labels "deployBlacklistTx" admin tx

insertBlacklistNodeCase :: BenchM ScenarioReport
insertBlacklistNodeCase = do
    scriptRoot <- deployDirectorySet admin
    userPkh <- walletPaymentKeyHash MockWallet.w2
    submitBlacklistDeployment scriptRoot
    Env.withEnv $
        asAdmin @C.ConwayEra $
            Env.withDirectoryFor scriptRoot $
                Env.withTransferFromOperator @C.ConwayEra $ do
                    tx <- Endpoints.insertBlacklistNodeTx "" (C.PaymentCredentialByKey userPkh)
                    labels <- currentBenchLabels
                    benchmarkAndSubmitSignedTx labels "insertBlacklistNodeTx" admin tx

removeBlacklistNodeCase :: BenchM ScenarioReport
removeBlacklistNodeCase = do
    scriptRoot <- deployDirectorySet admin
    userPkh <- walletPaymentKeyHash MockWallet.w2
    submitBlacklistDeployment scriptRoot
    submitBlacklistInsertion scriptRoot (C.PaymentCredentialByKey userPkh)
    Env.withEnv $
        asAdmin @C.ConwayEra $
            Env.withDirectoryFor scriptRoot $
                Env.withTransferFromOperator @C.ConwayEra $ do
                    tx <- Endpoints.removeBlacklistNodeTx (C.PaymentCredentialByKey userPkh)
                    labels <- currentBenchLabels
                    benchmarkAndSubmitSignedTx labels "removeBlacklistNodeTx" admin tx

transferSmartTokensCase :: BenchM ScenarioReport
transferSmartTokensCase = do
    scriptRoot <- deployDirectorySet admin
    userPkh <- walletPaymentKeyHash MockWallet.w2
    submitBlacklistDeployment scriptRoot
    aid <- issueTransferLogicProgrammableToken scriptRoot
    Env.withEnv $
        asAdmin @C.ConwayEra $
            Env.withDirectoryFor scriptRoot $
                Env.withTransferFromOperator @C.ConwayEra $ do
                    tx <- Endpoints.transferSmartTokensTx DontSubmitFailingTx aid 80 (C.PaymentCredentialByKey userPkh)
                    labels <- currentBenchLabels
                    benchmarkAndSubmitSignedTx labels "transferSmartTokensTx.Spend1Utxo" admin tx

transferSmartTokensSpendNUtxosCase :: Int -> BenchM ScenarioReport
transferSmartTokensSpendNUtxosCase utxoCount = do
    scriptRoot <- deployDirectorySet admin
    userPkh <- walletPaymentKeyHash MockWallet.w2
    opPkh <- walletPaymentKeyHash MockWallet.w1
    submitBlacklistDeployment scriptRoot
    aid <- issueTransferLogicProgrammableTokenQuantity scriptRoot (powerOfTwoQuantity utxoCount)
    submitTransfersToUser scriptRoot aid (splitQuantitiesForOutputCount utxoCount) (C.PaymentCredentialByKey opPkh)
    Env.withEnv $
        asAdmin @C.ConwayEra $
            Env.withDirectoryFor scriptRoot $
                Env.withTransferFromOperator @C.ConwayEra $ do
                    let transferQuantity = powerOfTwoQuantity utxoCount - 1
                        benchName = "transferSmartTokensTx.Spend" <> show utxoCount <> "Utxos"
                    tx <- Endpoints.transferSmartTokensTx DontSubmitFailingTx aid transferQuantity (C.PaymentCredentialByKey userPkh)
                    labels <- currentBenchLabels
                    benchmarkAndSubmitSignedTx labels benchName admin tx

seizeCredentialAssetsCase :: BenchM ScenarioReport
seizeCredentialAssetsCase = do
    scriptRoot <- deployDirectorySet admin
    userPkh <- walletPaymentKeyHash MockWallet.w2
    aid <- issueTransferLogicProgrammableToken scriptRoot
    submitBlacklistDeployment scriptRoot
    submitTransferToUser scriptRoot aid 50 (C.PaymentCredentialByKey userPkh)
    Env.withEnv $
        asAdmin @C.ConwayEra $
            Env.withDirectoryFor scriptRoot $
                Env.withTransferFromOperator @C.ConwayEra $ do
                    tx <- Endpoints.seizeCredentialAssetsTx mempty (C.PaymentCredentialByKey userPkh)
                    labels <- currentBenchLabels
                    benchmarkAndSubmitSignedTx labels "seizeCredentialAssetsTx.Seize1Utxo" admin tx

seizeMultiCredentialAssetsCase :: BenchM ScenarioReport
seizeMultiCredentialAssetsCase = do
    scriptRoot <- deployDirectorySet admin
    userPkh <- walletPaymentKeyHash MockWallet.w2
    aid <- issueTransferLogicProgrammableToken scriptRoot
    submitBlacklistDeployment scriptRoot
    submitTransferToUser scriptRoot aid 50 (C.PaymentCredentialByKey userPkh)
    submitTransferToUser scriptRoot aid 50 (C.PaymentCredentialByKey userPkh)
    Env.withEnv $
        asAdmin @C.ConwayEra $
            Env.withDirectoryFor scriptRoot $
                Env.withTransferFromOperator @C.ConwayEra $ do
                    tx <- Endpoints.seizeMultiCredentialAssetsTx mempty 2 [C.PaymentCredentialByKey userPkh]
                    labels <- currentBenchLabels
                    benchmarkAndSubmitSignedTx labels "seizeMultiCredentialAssetsTx.Seize2Utxos" admin tx

seizeMultiCredentialAssetsNUtxosCase :: Int -> BenchM ScenarioReport
seizeMultiCredentialAssetsNUtxosCase utxoCount = do
    scriptRoot <- deployDirectorySet admin
    userPkh <- walletPaymentKeyHash MockWallet.w2
    let userCred = C.PaymentCredentialByKey userPkh
    aid <- issueTransferLogicProgrammableTokenQuantity scriptRoot (powerOfTwoQuantity (utxoCount + 1))
    submitBlacklistDeployment scriptRoot
    submitTransfersToUser scriptRoot aid (splitQuantitiesForOutputCount (utxoCount + 1)) userCred
    Env.withEnv $
        asAdmin @C.ConwayEra $
            Env.withDirectoryFor scriptRoot $
                Env.withTransferFromOperator @C.ConwayEra $ do
                    let benchName = "seizeMultiCredentialAssetsTx.Seize" <> show utxoCount <> "Utxos"
                    tx <- Endpoints.seizeMultiCredentialAssetsTx mempty utxoCount [userCred]
                    labels <- currentBenchLabels
                    benchmarkAndSubmitSignedTx labels benchName admin tx

walletPaymentKeyHash :: ConvexWallet.Wallet -> BenchM (C.Hash C.PaymentKey)
walletPaymentKeyHash wallet =
    Env.withEnv $
        asWallet @C.ConwayEra wallet $
            asks (fst . ProgOperatorEnv.bteOperator . ProgOperatorEnv.operatorEnv @C.ConwayEra)

submitBlacklistDeployment :: DirectoryScriptRoot -> BenchM ()
submitBlacklistDeployment scriptRoot =
    Env.withEnv $
        asAdmin @C.ConwayEra $
            Env.withDirectoryFor scriptRoot $
                Env.withTransferFromOperator @C.ConwayEra $ do
                    tx <- Endpoints.deployBlacklistTx
                    void $ submitTxOrThrow (signTxOperator admin tx)

submitBlacklistInsertion :: DirectoryScriptRoot -> C.PaymentCredential -> BenchM ()
submitBlacklistInsertion scriptRoot paymentCred =
    Env.withEnv $
        asAdmin @C.ConwayEra $
            Env.withDirectoryFor scriptRoot $
                Env.withTransferFromOperator @C.ConwayEra $ do
                    tx <- Endpoints.insertBlacklistNodeTx "" paymentCred
                    void $ submitTxOrThrow (signTxOperator admin tx)

submitTransferToUser :: DirectoryScriptRoot -> C.AssetId -> C.Quantity -> C.PaymentCredential -> BenchM ()
submitTransferToUser scriptRoot aid quantity destCred =
    Env.withEnv $
        asAdmin @C.ConwayEra $
            Env.withDirectoryFor scriptRoot $
                Env.withTransferFromOperator @C.ConwayEra $ do
                    tx <- Endpoints.transferSmartTokensTx DontSubmitFailingTx aid quantity destCred
                    void $ submitTxOrThrow (signTxOperator admin tx)

issueTransferLogicProgrammableToken :: DirectoryScriptRoot -> BenchM C.AssetId
issueTransferLogicProgrammableToken scriptRoot =
    issueTransferLogicProgrammableTokenQuantity scriptRoot 100

issueTransferLogicProgrammableTokenQuantity :: DirectoryScriptRoot -> C.Quantity -> BenchM C.AssetId
issueTransferLogicProgrammableTokenQuantity scriptRoot quantity = do
    registerTransferScriptsForDirectory scriptRoot
    Env.withEnv $
        asAdmin @C.ConwayEra $
            Env.withDirectoryFor scriptRoot $
                Env.withTransferFromOperator @C.ConwayEra $ do
                    opPkh <- asks (fst . ProgOperatorEnv.bteOperator . ProgOperatorEnv.operatorEnv @C.ConwayEra)
                    (tx, aid) <- Endpoints.issueSmartTokensTx (C.UnsafeAssetName "dummy asset") quantity (C.PaymentCredentialByKey opPkh)
                    void $ submitTxOrThrow (signTxOperator admin tx)
                    pure aid

submitTransfersToUser :: DirectoryScriptRoot -> C.AssetId -> [C.Quantity] -> C.PaymentCredential -> BenchM ()
submitTransfersToUser scriptRoot aid quantities destCred =
    go quantities
  where
    go [] = pure ()
    go (quantity : remaining) = do
        submitTransferToUser scriptRoot aid quantity destCred
        go remaining

splitQuantitiesForOutputCount :: Int -> [C.Quantity]
splitQuantitiesForOutputCount outputCount =
    fmap powerOfTwoQuantity [1 .. max 0 (outputCount - 1)]

powerOfTwoQuantity :: Int -> C.Quantity
powerOfTwoQuantity power =
    fromIntegral ((2 :: Integer) ^ power)

registerTransferScriptsForDirectory :: DirectoryScriptRoot -> BenchM ()
registerTransferScriptsForDirectory scriptRoot =
    Env.withEnv $
        asAdmin @C.ConwayEra $
            Env.withDirectoryFor scriptRoot $
                Env.withTransferFromOperator @C.ConwayEra $ do
                    opPkh <- asks (fst . ProgOperatorEnv.bteOperator . ProgOperatorEnv.operatorEnv @C.ConwayEra)
                    void $ registerTransferScripts opPkh

registerTransferScripts ::
    ( MonadError (AppError C.ConwayEra) m
    , MonadFail m
    , MonadMockchain C.ConwayEra m
    , ProgEnv.HasTransferLogicEnv env
    , MonadReader env m
    ) =>
    C.Hash C.PaymentKey ->
    m C.TxId
registerTransferScripts pkh = do
    transferMintingScript <- asks (Env.tleMintingScript . Env.transferLogicEnv)
    transferSpendingScript <- asks (Env.tleTransferScript . Env.transferLogicEnv)
    transferSeizeSpendingScript <- asks (Env.tleIssuerScript . Env.transferLogicEnv)

    let hshMinting = C.hashScript $ C.PlutusScript C.plutusScriptVersion transferMintingScript
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

    tx <- mapError BalancingError (tryBalanceAndSubmit mempty MockWallet.w1 txBody TrailingChange [])
    pure (C.getTxId (C.getTxBody tx))

submitTxOrThrow ::
    (MonadBlockchain C.ConwayEra m, MonadError (AppError C.ConwayEra) m) =>
    C.Tx C.ConwayEra ->
    m C.TxId
submitTxOrThrow tx =
    sendTx tx >>= \case
        Left err -> throwError (SubmitError err)
        Right txId -> pure txId

benchmarkAndSubmitSignedTx ::
    ( MonadMockchain C.ConwayEra m
    , MonadError (AppError C.ConwayEra) m
    , MonadFail m
    ) =>
    BenchLabels ->
    String ->
    Operator Signing ->
    C.Tx C.ConwayEra ->
    m ScenarioReport
benchmarkAndSubmitSignedTx labels name signer tx =
    benchmarkAndSubmitTx labels name (signTxOperator signer tx)

benchmarkAndSubmitTx ::
    ( MonadMockchain C.ConwayEra m
    , MonadError (AppError C.ConwayEra) m
    , MonadFail m
    ) =>
    BenchLabels ->
    String ->
    C.Tx C.ConwayEra ->
    m ScenarioReport
benchmarkAndSubmitTx labels name tx = do
    ledgerUtxo <- getUtxo
    params <- askNodeParams
    exUnitsByIndex <-
        case getTxExUnits params ledgerUtxo tx of
            Left err -> fail ("getTxExUnits failed for " <> name <> ": " <> show err)
            Right units -> pure units
    void $ submitTxOrThrow tx
    let utxo = fromLedgerUTxO C.shelleyBasedEra ledgerUtxo
    pure
        ScenarioReport
            { srName = name
            , srTxId = C.getTxId (C.getTxBody tx)
            , srRows = mkScriptRows labels utxo tx exUnitsByIndex
            }

mkScriptRows :: BenchLabels -> C.UTxO C.ConwayEra -> C.Tx C.ConwayEra -> Map.Map C.ScriptWitnessIndex C.ExecutionUnits -> [ScriptRow]
mkScriptRows labels utxo tx exUnitsByIndex =
    fmap mkRow (Map.toAscList exUnitsByIndex)
  where
    mkRow (witnessIndex, units) =
        let (contract, purpose, detail) = scriptLabel labels utxo tx witnessIndex
         in ScriptRow
                { rowWitness = witnessIndex
                , rowContract = contract
                , rowPurpose = purpose
                , rowDetail = detail
                , rowCpu = fromIntegral (C.executionSteps units)
                , rowMem = fromIntegral (C.executionMemory units)
                }

scriptLabel :: BenchLabels -> C.UTxO C.ConwayEra -> C.Tx C.ConwayEra -> C.ScriptWitnessIndex -> (String, String, String)
scriptLabel labels utxo tx = \case
    C.ScriptWitnessIndexTxIn idx ->
        let inputs = C.txIns body
         in case atMay (fromIntegral idx) inputs of
                Nothing ->
                    ("unknown spend", "spend", "tx-in #" <> show idx)
                Just (txIn, _) ->
                    case Map.lookup txIn (C.unUTxO utxo) of
                        Nothing ->
                            ("unknown spend", "spend", abbrevTxIn txIn)
                        Just resolved ->
                            let paymentCred = paymentCredentialFromTxOut resolved
                                contract = Map.findWithDefault "unknown spend" paymentCred (blPaymentCreds labels)
                             in (contract, "spend", abbrevTxIn txIn)
    C.ScriptWitnessIndexMint idx ->
        let policies = case C.txMintValue body of
                C.TxMintNone -> []
                C.TxMintValue _ policyMap -> Map.toAscList policyMap
         in case atMay (fromIntegral idx) policies of
                Nothing ->
                    ("unknown mint", "mint", "policy #" <> show idx)
                Just (policyId, _) ->
                    let contract = Map.findWithDefault "unknown mint" policyId (blPolicies labels)
                     in (contract, "mint", abbrevPolicyId policyId)
    C.ScriptWitnessIndexWithdrawal idx ->
        let withdrawals = case C.txWithdrawals body of
                C.TxWithdrawalsNone -> []
                C.TxWithdrawals _ entries -> entries
         in case atMay (fromIntegral idx) withdrawals of
                Nothing ->
                    ("unknown withdrawal", "withdrawal", "withdrawal #" <> show idx)
                Just (stakeAddr, _, _) ->
                    let contract = Map.findWithDefault "unknown withdrawal" stakeAddr (blStakeAddresses labels)
                     in (contract, "withdrawal", abbrevStakeAddress stakeAddr)
    C.ScriptWitnessIndexCertificate idx ->
        ("certificate", "certificate", "certificate #" <> show idx)
    C.ScriptWitnessIndexVoting idx ->
        ("voting", "voting", "vote #" <> show idx)
    C.ScriptWitnessIndexProposing idx ->
        ("proposal", "proposal", "proposal #" <> show idx)
  where
    body = C.getTxBodyContent (C.getTxBody tx)

currentBenchLabels ::
    ( MonadReader env m
    , MonadBlockchain C.ConwayEra m
    , Env.HasDirectoryEnv env
    , Env.HasTransferLogicEnv env
    , Env.HasBlacklistEnv env
    ) =>
    m BenchLabels
currentBenchLabels = do
    directoryLabels <- directoryBenchLabels
    transferLabels <- transferBenchLabels
    blacklistLabels <- blacklistBenchLabels
    pure (directoryLabels <> transferLabels <> blacklistLabels)

directoryBenchLabels ::
    (MonadReader env m, MonadBlockchain C.ConwayEra m, Env.HasDirectoryEnv env) =>
    m BenchLabels
directoryBenchLabels = do
    networkId <- queryNetworkId
    dirEnv <- asks Env.directoryEnv
    let policyNames =
            Map.fromList
                [ (Env.directoryNodePolicyId dirEnv, "directoryNodeMinting")
                , (Env.protocolParamsPolicyId dirEnv, "protocolParamsMinting")
                , (Env.issuanceCborHexPolicyId dirEnv, "issuanceCborHexMinting")
                ]
        paymentNames =
            Map.fromList
                [ (scriptPaymentCredential (Env.dsDirectorySpendingScript dirEnv), "directoryNodeSpending")
                , (scriptPaymentCredential (Env.dsProtocolParamsSpendingScript dirEnv), "protocolParamsSpending")
                , (scriptPaymentCredential (Env.dsIssuanceCborHexSpendingScript dirEnv), "issuanceCborHexSpending")
                , (Env.programmableLogicBaseCredential dirEnv, "programmableLogicBase")
                ]
        stakeNames =
            Map.fromList
                [ (C.makeStakeAddress networkId (Env.programmableLogicStakeCredential dirEnv), "programmableLogicGlobal")
                ]
    pure BenchLabels{blPaymentCreds = paymentNames, blStakeAddresses = stakeNames, blPolicies = policyNames}

transferBenchLabels ::
    ( MonadReader env m
    , MonadBlockchain C.ConwayEra m
    , Env.HasDirectoryEnv env
    , Env.HasTransferLogicEnv env
    ) =>
    m BenchLabels
transferBenchLabels = do
    networkId <- queryNetworkId
    transferEnv <- asks Env.transferLogicEnv
    programmableTokenPolicy <- ProgEnv.programmableTokenPolicyId
    let issuanceStake = scriptStakeCredential (Env.tleMintingScript transferEnv)
        transferStake = scriptStakeCredential (Env.tleTransferScript transferEnv)
        thirdPartyStake = scriptStakeCredential (Env.tleIssuerScript transferEnv)
        stakeNames =
            Map.fromList
                [ (C.makeStakeAddress networkId issuanceStake, "issuanceLogicScript")
                , (C.makeStakeAddress networkId transferStake, "transferLogicScript")
                , (C.makeStakeAddress networkId thirdPartyStake, "thirdPartyTransferLogicScript")
                ]
        policyNames =
            Map.fromList
                [ (programmableTokenPolicy, "programmableTokenMinting")
                ]
    pure BenchLabels{blPaymentCreds = Map.empty, blStakeAddresses = stakeNames, blPolicies = policyNames}

blacklistBenchLabels ::
    (MonadReader env m, Env.HasBlacklistEnv env) =>
    m BenchLabels
blacklistBenchLabels = do
    blacklistEnv <- asks Env.blacklistEnv
    let policyNames =
            Map.fromList
                [ (Env.blacklistNodePolicyId blacklistEnv, "blacklistNodeMinting")
                ]
        paymentNames =
            Map.fromList
                [ (scriptPaymentCredential (Env.bleSpendingScript blacklistEnv), "blacklistNodeSpending")
                ]
    pure BenchLabels{blPaymentCreds = paymentNames, blStakeAddresses = Map.empty, blPolicies = policyNames}

scriptPaymentCredential :: C.PlutusScript C.PlutusScriptV3 -> C.PaymentCredential
scriptPaymentCredential =
    C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3

scriptStakeCredential :: C.PlutusScript C.PlutusScriptV3 -> C.StakeCredential
scriptStakeCredential =
    C.StakeCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3

paymentCredentialFromTxOut :: C.TxOut C.CtxUTxO C.ConwayEra -> C.PaymentCredential
paymentCredentialFromTxOut (C.TxOut (C.AddressInEra _ addr) _ _ _) =
    case addr of
        C.ShelleyAddress _ cred _ -> C.fromShelleyPaymentCredential cred
        C.ByronAddress _ -> error "unexpected Byron address in script input"

detectRenderStyle :: IO RenderStyle
detectRenderStyle = do
    noColor <- lookupEnv "NO_COLOR"
    isTty <- hIsTerminalDevice stdout
    pure (RenderStyle (isTty && isNothing noColor))

renderScenarioResults :: RenderStyle -> [ScenarioResult] -> [String]
renderScenarioResults renderStyle results =
    intercalate [""] (fmap (renderScenarioResult renderStyle) results)

renderScenarioResult :: RenderStyle -> ScenarioResult -> [String]
renderScenarioResult renderStyle = \case
    ScenarioFailed name err ->
        [ styleBanner renderStyle scenarioSeparator
        , styleFail renderStyle ("Scenario: " <> name <> " [FAIL]")
        , styleMeta renderStyle ("Error: " <> err)
        ]
    ScenarioPassed ScenarioReport{srName, srTxId, srRows} ->
        let totalCpu = sum (fmap rowCpu srRows)
            totalMem = sum (fmap rowMem srRows)
            contracts = uniquePreservingOrder (fmap rowContract srRows)
         in [ styleBanner renderStyle scenarioSeparator
            , stylePass renderStyle ("Scenario: " <> srName <> " [PASS]")
            , styleMeta renderStyle ("TxId: " <> abbrevTxId srTxId)
            , styleMeta renderStyle ("Scripts: " <> show (length srRows))
            ]
                <> renderContractSummary renderStyle contracts
                <> [ styleMeta renderStyle "Totals:"
                   , styleMeta renderStyle ("  CPU: " <> formatUnits totalCpu <> " | Mem: " <> formatUnits totalMem)
                   , styleMeta renderStyle ("  CPU %: " <> formatPercent (cpuPct totalCpu) <> " | Mem %: " <> formatPercent (memPct totalMem))
                   , ""
                   ]
                <> if null srRows
                    then [styleMeta renderStyle "No Plutus script witnesses"]
                    else indentLines 2 (renderScriptBlocks renderStyle srRows)

renderContractSummary :: RenderStyle -> [String] -> [String]
renderContractSummary renderStyle [] = [styleMeta renderStyle "Contracts: none"]
renderContractSummary renderStyle contracts =
    styleMeta renderStyle "Contracts:" : fmap (styleMeta renderStyle . ("  - " <>)) contracts

renderScriptBlocks :: RenderStyle -> [ScriptRow] -> [String]
renderScriptBlocks renderStyle rows =
    intercalate [""] (fmap (renderScriptBlock renderStyle) rows)

renderScriptBlock :: RenderStyle -> ScriptRow -> [String]
renderScriptBlock renderStyle ScriptRow{rowWitness, rowContract, rowPurpose, rowDetail, rowCpu, rowMem} =
    [ styleTableHeader renderStyle (rowContract <> ":")
    , "  witness: " <> renderWitnessIndex rowWitness
    , "  purpose: " <> rowPurpose <> " | detail: " <> rowDetail
    , "  CPU: "
        <> formatUnits rowCpu
        <> " | Mem: "
        <> formatUnits rowMem
        <> " | CPU %: "
        <> formatPercent (cpuPct rowCpu)
        <> " | Mem %: "
        <> formatPercent (memPct rowMem)
    ]

renderWitnessIndex :: C.ScriptWitnessIndex -> String
renderWitnessIndex = \case
    C.ScriptWitnessIndexTxIn n -> "TxIn[" <> show n <> "]"
    C.ScriptWitnessIndexMint n -> "Mint[" <> show n <> "]"
    C.ScriptWitnessIndexCertificate n -> "Cert[" <> show n <> "]"
    C.ScriptWitnessIndexWithdrawal n -> "Wdrl[" <> show n <> "]"
    C.ScriptWitnessIndexVoting n -> "Vote[" <> show n <> "]"
    C.ScriptWitnessIndexProposing n -> "Prop[" <> show n <> "]"

formatUnits :: (Show a) => a -> String
formatUnits = formatThousands . show

formatThousands :: String -> String
formatThousands ('-' : rest) = '-' : formatThousands rest
formatThousands digits =
    intercalate "," . reverse . fmap reverse . chunksOf 3 . reverse $ digits

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
    let (prefix, suffix) = splitAt n xs
     in prefix : chunksOf n suffix

formatPercent :: Double -> String
formatPercent pct = showFFloat (Just 2) pct "%"

cpuPct :: Integer -> Double
cpuPct cpu = percentage cpu (exBudgetCpuInt productionMaxTxExBudget)

memPct :: Integer -> Double
memPct mem = percentage mem (exBudgetMemInt productionMaxTxExBudget)

percentage :: Integer -> Integer -> Double
percentage numerator denominator =
    (fromIntegral numerator * 100) / fromIntegral denominator

exBudgetCpuInt :: ExBudget -> Integer
exBudgetCpuInt (ExBudget (ExCPU cpu) _) = read (show cpu)

exBudgetMemInt :: ExBudget -> Integer
exBudgetMemInt (ExBudget _ (ExMemory mem)) = read (show mem)

scenarioSeparator :: String
scenarioSeparator = replicate 80 '='

stylePass :: RenderStyle -> String -> String
stylePass (RenderStyle False) s = s
stylePass (RenderStyle True) s = "\ESC[1;32m" <> s <> "\ESC[0m"

styleFail :: RenderStyle -> String -> String
styleFail (RenderStyle False) s = s
styleFail (RenderStyle True) s = "\ESC[1;31m" <> s <> "\ESC[0m"

styleMeta :: RenderStyle -> String -> String
styleMeta (RenderStyle False) s = s
styleMeta (RenderStyle True) s = "\ESC[36m" <> s <> "\ESC[0m"

styleBanner :: RenderStyle -> String -> String
styleBanner (RenderStyle False) s = s
styleBanner (RenderStyle True) s = "\ESC[2;37m" <> s <> "\ESC[0m"

styleTableHeader :: RenderStyle -> String -> String
styleTableHeader (RenderStyle False) s = s
styleTableHeader (RenderStyle True) s = "\ESC[1;34m" <> s <> "\ESC[0m"

indentLines :: Int -> [String] -> [String]
indentLines width = fmap ((replicate width ' ') <>)

atMay :: Int -> [a] -> Maybe a
atMay idx _
    | idx < 0 = Nothing
atMay 0 (x : _) = Just x
atMay n (_ : xs) = atMay (n - 1) xs
atMay _ [] = Nothing

abbrevTo :: Int -> String -> String
abbrevTo limit s
    | length s <= limit = s
    | limit <= 8 = take limit s
    | otherwise =
        let suffixLen = min 10 (max 4 (limit `div` 4))
            prefixLen = max 1 (limit - suffixLen - 3)
         in take prefixLen s <> "..." <> drop (length s - suffixLen) s

abbrev :: String -> String
abbrev = abbrevTo 44

abbrevTxId :: C.TxId -> String
abbrevTxId = abbrevTo 52 . showNoQuotes

abbrevTxIn :: C.TxIn -> String
abbrevTxIn = abbrev . show

abbrevPolicyId :: C.PolicyId -> String
abbrevPolicyId = abbrev . showNoQuotes

abbrevStakeAddress :: C.StakeAddress -> String
abbrevStakeAddress = abbrev . show

showNoQuotes :: (Show a) => a -> String
showNoQuotes = stripQuotes . show

stripQuotes :: String -> String
stripQuotes s =
    case s of
        '"' : rest -> reverse (dropWhile (== '"') (reverse rest))
        _ -> s

uniquePreservingOrder :: (Ord a) => [a] -> [a]
uniquePreservingOrder = go Map.empty
  where
    go _ [] = []
    go seen (x : xs)
        | Map.member x seen = go seen xs
        | otherwise = x : go (Map.insert x () seen) xs
