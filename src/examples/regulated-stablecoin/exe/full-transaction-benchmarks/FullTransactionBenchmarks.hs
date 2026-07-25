{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cardano.Api qualified as C
import Control.Exception (SomeException, displayException, try)
import Control.Monad (replicateM_, void, when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.Class (lift)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (
    MonadBlockchain (queryNetworkId, queryProtocolParameters, sendTx),
    MonadMockchain,
    MonadUtxoQuery,
    askNodeParams,
    getUtxo,
 )
import Convex.CoinSelection (ChangeOutputPosition (TrailingChange))
import Convex.CoinSelection qualified as CoinSelection
import Convex.MockChain (MockchainT, fromLedgerUTxO, getTxExUnits, runMockchain0IOWith)
import Convex.MockChain.CoinSelection (tryBalanceAndSubmit)
import Convex.PlutusLedger.V1 (transCredential, transPolicyId)
import Convex.Utils (mapError)
import Convex.Utxos qualified as Utxos
import Convex.Wallet qualified as ConvexWallet
import Convex.Wallet.MockWallet qualified as MockWallet
import Convex.Wallet.Operator (Operator, Signing, returnOutputFor, signTxOperator)
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List (intercalate, isInfixOf, maximumBy, nub)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Numeric (showFFloat)
import PlutusLedgerApi.V3 (
    BuiltinByteString,
    Credential (PubKeyCredential, ScriptCredential),
    CurrencySymbol,
    ExBudget (ExBudget),
    ExCPU (ExCPU),
    ExMemory (ExMemory),
    PubKeyHash (PubKeyHash),
    ScriptHash (ScriptHash),
 )
import ProgrammableTokens.OffChain.BuildTx qualified as ProgBuildTx
import ProgrammableTokens.OffChain.Endpoints qualified as ProgEndpoints
import ProgrammableTokens.OffChain.Env qualified as ProgEnv
import ProgrammableTokens.OffChain.Env.Operator qualified as ProgOperatorEnv
import ProgrammableTokens.OffChain.Error (AsProgrammableTokensError)
import ProgrammableTokens.OffChain.Query qualified as ProgQuery
import ProgrammableTokens.Test (
    admin,
    deployDirectorySet,
    nodeParamsFor,
    productionMaxTxExBudget,
 )
import SmartTokens.Contracts.ExampleTransferLogic (BlacklistProof (NonmembershipProof))
import SmartTokens.Contracts.ProgrammableLogicBase (
    ProgrammableLogicGlobalRedeemer (TransferAct, plgrMintProofs, plgrParamsRefIdx, plgrTransferProofs, plgrTransferWdrlIdxs),
 )
import SmartTokens.Core.Scripts (ScriptTarget (Debug, Production))
import SmartTokens.Types.PTokenDirectory (BlacklistNode (..), DirectorySetNode (..))
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stdout)
import Wst.AppError (AppError (BalancingError, SubmitError))
import Wst.Offchain.BuildTx.Failing (BlacklistedTransferPolicy (DontSubmitFailingTx))
import Wst.Offchain.BuildTx.ReferenceScripts qualified as RefScripts
import Wst.Offchain.BuildTx.TransferLogic qualified as TransferBuildTx
import Wst.Offchain.BuildTx.Utils (addConwayStakeCredentialCertificate)
import Wst.Offchain.Endpoints.Deployment qualified as Endpoints
import Wst.Offchain.Env (DirectoryScriptRoot)
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query qualified as WstQuery
import Wst.Test.Env (asAdmin, asWallet)
import Wst.Test.Env qualified as TestEnv

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
    , srNotes :: [String]
    , srRows :: [ScriptRow]
    }

data TxBudgetStatus
    = TxWithinBudget ScenarioReport Integer Integer
    | TxOverBudget ScenarioReport Integer Integer
    | TxEvaluationFailed String

data SearchResult = SearchResult
    { sCap :: Int
    , sMaxFit :: Int
    , sMaxReport :: ScenarioReport
    , sMaxCpu :: Integer
    , sMaxMem :: Integer
    , sFirstOver :: Maybe TxBudgetStatus
    }

data BenchReferenceScripts = BenchReferenceScripts
    { brsBase :: C.TxIn
    , brsGlobal :: C.TxIn
    , brsTransfer :: C.TxIn
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
    noColor <- lookupEnv "NO_COLOR"
    isTty <- hIsTerminalDevice stdout
    let renderStyle = RenderStyle (isTty && isNothing noColor)
    benchOnly <- lookupEnv "BENCH_ONLY"
    benchTarget <- lookupEnv "BENCH_TARGET"
    let target = case fmap (map toLower) benchTarget of
            Just "debug" -> Debug
            Just "tracing" -> Debug
            _ -> Production
    putStrLn $ "Regulated stablecoin full-transaction benchmarks (mockchain, " <> show target <> ")"
    let maxCpu = exBudgetCpuInt productionMaxTxExBudget
        maxMem = exBudgetMemInt productionMaxTxExBudget
    putStrLn $
        "Max tx ex units: CPU "
            <> formatUnits maxCpu
            <> " | Mem "
            <> formatUnits maxMem
    putStrLn ""
    let selectedBenchCases =
            case benchOnly of
                Nothing -> benchCases
                Just needle -> filter (\benchCase -> needle == bcName benchCase || needle `isInfixOf` bcName benchCase) benchCases
    results <- traverse (runBenchCase target) selectedBenchCases
    mapM_ putStrLn (intercalate [""] (fmap (renderScenarioResult renderStyle) results))

runBenchCase :: ScriptTarget -> BenchCase -> IO ScenarioResult
runBenchCase target BenchCase{bcName, bcRun} = do
    result <-
        try @SomeException $
            runMockchain0IOWith MockWallet.initialUTxOs (nodeParamsFor target) $
                runReaderT (runExceptT bcRun) target
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
    , BenchCase "transferSmartTokensTx.MaxOneTokenUtxosByExUnits" transferSmartTokensMaxOneTokenUtxosCase
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
    aid <- issueTransferLogicProgrammableTokenQuantity scriptRoot 100
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

transferSmartTokensMaxOneTokenUtxosCase :: BenchM ScenarioReport
transferSmartTokensMaxOneTokenUtxosCase = do
    target <- ask
    initialCap <- liftIO (lookupPositiveIntEnv "BENCH_INITIAL_ONE_TOKEN_CAP" 16)
    maxCap <- liftIO (lookupPositiveIntEnv "BENCH_MAX_ONE_TOKEN_CAP" 256)
    result <- liftIO (searchTransferMaxOneTokenUtxos target initialCap maxCap)
    case result of
        Left err -> fail err
        Right report -> pure report

seizeCredentialAssetsCase :: BenchM ScenarioReport
seizeCredentialAssetsCase = do
    scriptRoot <- deployDirectorySet admin
    userPkh <- walletPaymentKeyHash MockWallet.w2
    aid <- issueTransferLogicProgrammableTokenQuantity scriptRoot 100
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
    aid <- issueTransferLogicProgrammableTokenQuantity scriptRoot 100
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
                    debugDumpNamedTxIfRequested ("submitTransferToUser quantity=" <> show quantity) tx
                    void $ submitTxOrThrow (signTxOperator admin tx)

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
    traverse_ (\quantity -> submitTransferToUser scriptRoot aid quantity destCred) quantities

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
benchmarkAndSubmitSignedTx labels name signer tx = do
    let signedTx = signTxOperator signer tx
    report <- evaluateTx labels name signedTx
    void $ submitTxOrThrow signedTx
    pure report

evaluateTx ::
    ( MonadMockchain C.ConwayEra m
    , MonadError (AppError C.ConwayEra) m
    , MonadFail m
    ) =>
    BenchLabels ->
    String ->
    C.Tx C.ConwayEra ->
    m ScenarioReport
evaluateTx labels name tx = do
    ledgerUtxo <- getUtxo
    params <- askNodeParams
    exUnitsByIndex <-
        case getTxExUnits params ledgerUtxo tx of
            Left err -> fail ("getTxExUnits failed for " <> name <> ": " <> show err)
            Right units -> pure units
    let utxo = fromLedgerUTxO C.shelleyBasedEra ledgerUtxo
        txSizeBytes = BS.length (C.serialiseToCBOR tx)
    pure
        ScenarioReport
            { srName = name
            , srTxId = C.getTxId (C.getTxBody tx)
            , srNotes = ["Tx size: " <> formatUnits txSizeBytes <> " bytes"]
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
                , (C.makeStakeAddress networkId (Env.programmableSeizeStakeCredential dirEnv), "programmableSeize")
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

paymentCredentialFromTxOut :: C.TxOut C.CtxUTxO era -> C.PaymentCredential
paymentCredentialFromTxOut (C.TxOut (C.AddressInEra _ addr) _ _ _) =
    case addr of
        C.ShelleyAddress _ cred _ -> C.fromShelleyPaymentCredential cred
        C.ByronAddress _ -> error "unexpected Byron address in script input"

renderScenarioResult :: RenderStyle -> ScenarioResult -> [String]
renderScenarioResult renderStyle = \case
    ScenarioFailed name err ->
        [ styleBanner renderStyle scenarioSeparator
        , styleFail renderStyle ("Scenario: " <> name <> " [FAIL]")
        , styleMeta renderStyle ("Error: " <> err)
        ]
    ScenarioPassed ScenarioReport{srName, srTxId, srNotes, srRows} ->
        let totalCpu = sum (fmap rowCpu srRows)
            totalMem = sum (fmap rowMem srRows)
            contracts = uniquePreservingOrder (fmap rowContract srRows)
            renderContractSummary =
                case contracts of
                    [] -> [styleMeta renderStyle "Contracts: none"]
                    _ -> styleMeta renderStyle "Contracts:" : fmap (styleMeta renderStyle . ("  - " <>)) contracts
            renderNotes =
                case srNotes of
                    [] -> []
                    notes -> styleMeta renderStyle "Notes:" : fmap (styleMeta renderStyle . ("  - " <>)) notes
            renderContractLine (contract, witnessCount, contractCpu, contractMem) =
                styleMeta renderStyle $
                    "  - "
                        <> contract
                        <> " ("
                        <> show witnessCount
                        <> "): CPU "
                        <> formatUnits contractCpu
                        <> " | Mem "
                        <> formatUnits contractMem
                        <> " | CPU % "
                        <> formatPercent (cpuPct contractCpu)
                        <> " | Mem % "
                        <> formatPercent (memPct contractMem)
            renderWitness = \case
                C.ScriptWitnessIndexTxIn n -> "TxIn[" <> show n <> "]"
                C.ScriptWitnessIndexMint n -> "Mint[" <> show n <> "]"
                C.ScriptWitnessIndexCertificate n -> "Cert[" <> show n <> "]"
                C.ScriptWitnessIndexWithdrawal n -> "Wdrl[" <> show n <> "]"
                C.ScriptWitnessIndexVoting n -> "Vote[" <> show n <> "]"
                C.ScriptWitnessIndexProposing n -> "Prop[" <> show n <> "]"
            renderScriptBlock ScriptRow{rowWitness, rowContract, rowPurpose, rowDetail, rowCpu, rowMem} =
                [ styleTableHeader renderStyle (rowContract <> ":")
                , "  witness: " <> renderWitness rowWitness
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
         in [ styleBanner renderStyle scenarioSeparator
            , stylePass renderStyle ("Scenario: " <> srName <> " [PASS]")
            , styleMeta renderStyle ("TxId: " <> abbrevTxId srTxId)
            , styleMeta renderStyle ("Scripts: " <> show (length srRows))
            ]
                <> renderContractSummary
                <> renderNotes
                <> ( case contractTotals srRows of
                        [] -> []
                        totals ->
                            styleMeta renderStyle "Contract totals:"
                                : fmap renderContractLine totals
                   )
                <> [ styleMeta renderStyle "Totals:"
                   , styleMeta renderStyle ("  CPU: " <> formatUnits totalCpu <> " | Mem: " <> formatUnits totalMem)
                   , styleMeta renderStyle ("  CPU %: " <> formatPercent (cpuPct totalCpu) <> " | Mem %: " <> formatPercent (memPct totalMem))
                   , ""
                   ]
                <> if null srRows
                    then [styleMeta renderStyle "No Plutus script witnesses"]
                    else indentLines 2 (intercalate [""] (fmap renderScriptBlock srRows))

contractTotals :: [ScriptRow] -> [(String, Int, Integer, Integer)]
contractTotals rows =
    fmap toTotal (uniquePreservingOrder (fmap rowContract rows))
  where
    toTotal contract =
        let matchingRows = filter ((== contract) . rowContract) rows
         in (contract, length matchingRows, sum (fmap rowCpu matchingRows), sum (fmap rowMem matchingRows))

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

lookupPositiveIntEnv :: String -> Int -> IO Int
lookupPositiveIntEnv name fallback = do
    value <- lookupEnv name
    pure $
        case value >>= readPositiveInt of
            Just parsed -> parsed
            Nothing -> fallback
  where
    readPositiveInt raw =
        case reads raw of
            [(parsed, "")] | parsed > 0 -> Just parsed
            _ -> Nothing

searchTransferMaxOneTokenUtxos :: ScriptTarget -> Int -> Int -> IO (Either String ScenarioReport)
searchTransferMaxOneTokenUtxos target initialCap maxCap =
    go initialCap
  where
    go cap = do
        outcome <-
            runMockchain0IOWith MockWallet.initialUTxOs (nodeParamsFor target) $
                runReaderT (runExceptT (prepareTransferSearchState cap >>= searchPreparedTransferState cap)) target
        case outcome of
            (Left err, _state) -> pure (Left (show err))
            (Right result, _state)
                | sMaxFit result == sCap result && sCap result < maxCap ->
                    go (min maxCap (sCap result * 2))
                | otherwise ->
                    pure (Right (attachSearchNotes initialCap maxCap result))

attachSearchNotes :: Int -> Int -> SearchResult -> ScenarioReport
attachSearchNotes initialCap maxCap result =
    let maxReport = sMaxReport result
        fittedCpuPct = cpuPct (sMaxCpu result)
        fittedMemPct = memPct (sMaxMem result)
        boundaryNotes =
            case sFirstOver result of
                Nothing ->
                    if sCap result >= maxCap && sMaxFit result == sCap result
                        then ["Search cap reached at " <> show maxCap <> " one-token UTxOs; increase the cap to probe further."]
                        else ["No over-budget boundary was encountered inside the current search range."]
                Just status ->
                    case status of
                        TxWithinBudget _ _ _ ->
                            []
                        TxOverBudget overReport overCpu overMem ->
                            [ "First over-budget candidate: "
                                <> case reverse (words (srName overReport)) of
                                    suffix : _ -> suffix
                                    [] -> srName overReport
                                <> " | CPU "
                                <> formatUnits overCpu
                                <> " ("
                                <> formatPercent (cpuPct overCpu)
                                <> ")"
                                <> " | Mem "
                                <> formatUnits overMem
                                <> " ("
                                <> formatPercent (memPct overMem)
                                <> ")"
                            ]
                        TxEvaluationFailed err ->
                            ["First non-fitting candidate could not be evaluated: " <> err]
        notes =
            srNotes maxReport
                <> [ "Search range: " <> show initialCap <> " to " <> show maxCap <> " one-token UTxOs"
                   , "Maximum programmable-token UTxOs transferable within tx ex-unit limits: " <> show (sMaxFit result)
                   , "Winning candidate shape: "
                        <> show (sMaxFit result)
                        <> " programmableLogicBase inputs -> "
                        <> show (sMaxFit result)
                        <> " programmableLogicBase outputs, plus one wallet change output"
                   , "Winning candidate total: CPU " <> formatUnits (sMaxCpu result) <> " (" <> formatPercent fittedCpuPct <> "), Mem " <> formatUnits (sMaxMem result) <> " (" <> formatPercent fittedMemPct <> ")"
                   ]
                <> boundaryNotes
     in maxReport{srName = "transferSmartTokensTx.MaxOneTokenUtxosByExUnits", srNotes = notes}

prepareTransferSearchState ::
    Int ->
    BenchM (DirectoryScriptRoot, BenchLabels, C.AssetId, C.Hash C.PaymentKey, C.Hash C.PaymentKey, BenchReferenceScripts)
prepareTransferSearchState utxoCount = do
    debugBenchLog ("prepareTransferSearchState start: utxoCount=" <> show utxoCount)
    scriptRoot <- deployDirectorySet admin
    debugBenchLog "deployed directory set"
    userPkh <- walletPaymentKeyHash MockWallet.w2
    adminPkh <- walletPaymentKeyHash MockWallet.w1
    submitBlacklistDeployment scriptRoot
    debugBenchLog "submitted blacklist deployment"
    aid <- issueTransferLogicProgrammableTokenQuantity scriptRoot (fromIntegral utxoCount)
    debugBenchLog ("issued programmable token quantity=" <> show utxoCount)
    let userCred = C.PaymentCredentialByKey userPkh
    submitTransfersToUser scriptRoot aid (replicate utxoCount 1) userCred
    debugBenchLog "submitted transfers to user"
    refScripts <- createTransferReferenceScripts scriptRoot
    debugBenchLog
        ( "created transfer reference scripts: base="
            <> show (brsBase refScripts)
            <> " global="
            <> show (brsGlobal refScripts)
            <> " transfer="
            <> show (brsTransfer refScripts)
        )
    (transferLogic, ble) <-
        Env.withEnv $
            Env.withDirectoryFor scriptRoot $
                Env.transferLogicForDirectory adminPkh Nothing
    userOutputs <-
        Env.withEnv $
            asWallet @C.ConwayEra MockWallet.w2 $
                Env.withDirectoryFor scriptRoot $
                    ProgQuery.userProgrammableOutputs (userCred, Nothing)
    if length userOutputs /= utxoCount
        then fail ("Expected " <> show utxoCount <> " one-token user UTxOs, got " <> show (length userOutputs))
        else do
            labels <-
                Env.withEnv $
                    asWallet @C.ConwayEra MockWallet.w2 $
                        Env.withDirectoryFor scriptRoot $
                            Env.withBlacklist ble $
                                Env.withTransfer transferLogic $
                                    currentBenchLabels
            pure (scriptRoot, labels, aid, adminPkh, userPkh, refScripts)

createTransferReferenceScripts :: DirectoryScriptRoot -> BenchM BenchReferenceScripts
createTransferReferenceScripts scriptRoot = do
    target <- ask
    Env.withEnv $
        asAdmin @C.ConwayEra $
            Env.withDirectoryFor scriptRoot $
                Env.withTransferFromOperator @C.ConwayEra $ do
                    networkId <- queryNetworkId
                    directoryEnv <- asks Env.directoryEnv
                    transferEnv <- asks Env.transferLogicEnv
                    let refScriptAddr =
                            C.makeShelleyAddressInEra
                                C.shelleyBasedEra
                                networkId
                                (C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 $ Env.dsIssuanceCborHexSpendingScript directoryEnv)
                                C.NoStakeAddress
                        refScriptLovelace = case target of
                            Debug -> 250_000_000
                            Production -> 40_000_000
                        refScriptValue = C.lovelaceToValue refScriptLovelace
                    txBody <- BuildTx.execBuildTxT $ do
                        BuildTx.createRefScriptNoDatum
                            refScriptAddr
                            (C.PlutusScript C.PlutusScriptV3 (Env.dsProgrammableLogicBaseScript directoryEnv))
                            refScriptValue
                        BuildTx.createRefScriptNoDatum
                            refScriptAddr
                            (C.PlutusScript C.PlutusScriptV3 (Env.dsProgrammableLogicGlobalScript directoryEnv))
                            refScriptValue
                        BuildTx.createRefScriptNoDatum
                            refScriptAddr
                            (C.PlutusScript C.PlutusScriptV3 (Env.tleTransferScript transferEnv))
                            refScriptValue
                    tx <- mapError BalancingError (tryBalanceAndSubmit mempty MockWallet.w1 txBody TrailingChange [])
                    let txId = C.getTxId (C.getTxBody tx)
                    pure
                        BenchReferenceScripts
                            { brsBase = C.TxIn txId (C.TxIx 0)
                            , brsGlobal = C.TxIn txId (C.TxIx 1)
                            , brsTransfer = C.TxIn txId (C.TxIx 2)
                            }

searchPreparedTransferState ::
    Int ->
    (DirectoryScriptRoot, BenchLabels, C.AssetId, C.Hash C.PaymentKey, C.Hash C.PaymentKey, BenchReferenceScripts) ->
    BenchM SearchResult
searchPreparedTransferState cap prepared@(_scriptRoot, _labels, _aid, _adminPkh, _userPkh, _refScripts) = do
    fitted <- binarySearchMaxFit prepared 1 cap
    maxStatus <- evaluateTransferCandidate prepared fitted
    maxReport <- case maxStatus of
        TxWithinBudget report totalCpu totalMem -> pure (report, totalCpu, totalMem)
        TxOverBudget{} -> fail ("Internal error: fitted candidate " <> show fitted <> " evaluated as over budget")
        TxEvaluationFailed err -> fail ("Internal error evaluating fitted candidate " <> show fitted <> ": " <> err)
    firstOver <-
        if fitted < cap
            then Just <$> evaluateTransferCandidate prepared (fitted + 1)
            else pure Nothing
    let (report, totalCpu, totalMem) = maxReport
    pure
        SearchResult
            { sCap = cap
            , sMaxFit = fitted
            , sMaxReport = report
            , sMaxCpu = totalCpu
            , sMaxMem = totalMem
            , sFirstOver = firstOver
            }

binarySearchMaxFit ::
    (DirectoryScriptRoot, BenchLabels, C.AssetId, C.Hash C.PaymentKey, C.Hash C.PaymentKey, BenchReferenceScripts) ->
    Int ->
    Int ->
    BenchM Int
binarySearchMaxFit prepared low high
    | low >= high = pure low
    | otherwise = do
        let mid = (low + high + 1) `div` 2
        status <- evaluateTransferCandidate prepared mid
        case status of
            TxWithinBudget{} -> binarySearchMaxFit prepared mid high
            TxOverBudget{} -> binarySearchMaxFit prepared low (mid - 1)
            TxEvaluationFailed{} -> binarySearchMaxFit prepared low (mid - 1)

evaluateTransferCandidate ::
    (DirectoryScriptRoot, BenchLabels, C.AssetId, C.Hash C.PaymentKey, C.Hash C.PaymentKey, BenchReferenceScripts) ->
    Int ->
    BenchM TxBudgetStatus
evaluateTransferCandidate (scriptRoot, labels, aid, adminPkh, _userPkh, refScripts) utxoCount = do
    debugBenchLog ("evaluateTransferCandidate start: utxoCount=" <> show utxoCount)
    let adminCred = C.PaymentCredentialByKey adminPkh
    (transferLogic, ble) <-
        Env.withEnv $
            Env.withDirectoryFor scriptRoot $
                Env.transferLogicForDirectory adminPkh Nothing
    txResult <-
        lift $
            runExceptT
                ( Env.withEnv $
                    asWallet @C.ConwayEra MockWallet.w2 $
                        Env.withDirectoryFor scriptRoot $
                            Env.withBlacklist ble $
                                Env.withTransfer transferLogic $ do
                                    debugBenchLog "building one-output-per-input candidate"
                                    (balancedTx, tx) <- oneOutputPerInputTransferTx aid utxoCount adminCred refScripts
                                    debugBenchLog "built candidate tx"
                                    debugDumpTransferCandidateIfRequested utxoCount refScripts balancedTx tx
                                    evaluateTx labels ("transferSmartTokensTx.OneTokenUtxos " <> show utxoCount) (signTxOperator (TestEnv.user MockWallet.w2) tx)
                )
    pure $ case txResult of
        Left err -> TxEvaluationFailed (show err)
        Right report ->
            let totalCpu = sum (fmap rowCpu (srRows report))
                totalMem = sum (fmap rowMem (srRows report))
             in if totalCpu <= exBudgetCpuInt productionMaxTxExBudget
                    && totalMem <= exBudgetMemInt productionMaxTxExBudget
                    then TxWithinBudget report totalCpu totalMem
                    else TxOverBudget report totalCpu totalMem

oneOutputPerInputTransferTx ::
    forall env err m.
    ( MonadReader env m
    , ProgEnv.HasOperatorEnv C.ConwayEra env
    , Env.HasDirectoryEnv env
    , Env.HasTransferLogicEnv env
    , Env.HasBlacklistEnv env
    , MonadMockchain C.ConwayEra m
    , MonadError err m
    , MonadFail m
    , MonadIO m
    , MonadUtxoQuery m
    , AsProgrammableTokensError err
    , CoinSelection.AsCoinSelectionError err
    , CoinSelection.AsBalancingError err C.ConwayEra
    ) =>
    C.AssetId ->
    Int ->
    C.PaymentCredential ->
    BenchReferenceScripts ->
    m (C.BalancedTxBody C.ConwayEra, C.Tx C.ConwayEra)
oneOutputPerInputTransferTx assetId utxoCount destCred refScripts = do
    debugBenchLog ("oneOutputPerInputTransferTx start: utxoCount=" <> show utxoCount)
    directory <- ProgQuery.registryNodes @C.ConwayEra
    blacklist <- WstQuery.blacklistNodes @C.ConwayEra
    opPkh <- asks (fst . ProgOperatorEnv.bteOperator . ProgOperatorEnv.operatorEnv @C.ConwayEra)
    networkId <- queryNetworkId
    directoryEnv <- asks Env.directoryEnv
    transferEnv <- asks Env.transferLogicEnv
    let ownerCred = C.PaymentCredentialByKey opPkh
        proofResult = blacklistProofResult blacklist (transCredential ownerCred)
    userOutputsAtProgrammable <- ProgQuery.userProgrammableOutputs (ownerCred, Nothing)
    paramsTxIn <- ProgQuery.globalParamsNode @C.ConwayEra
    let selectedOutputs = take utxoCount userOutputsAtProgrammable
    when (length selectedOutputs /= utxoCount) $
        fail ("Expected at least " <> show utxoCount <> " programmable inputs, got " <> show (length selectedOutputs))
    case assetId of
        C.AdaAssetId ->
            fail "Ada is not a programmable token"
        C.AssetId policyId assetName -> do
            operatorEnv <- asks (ProgOperatorEnv.operatorEnv @C.ConwayEra)
            protocolParams <- queryProtocolParameters
            let txIns = fmap WstQuery.uIn selectedOutputs
                programmableTokenSymbol = transPolicyId policyId
                globalStakeScript = Env.dsProgrammableLogicGlobalScript directoryEnv
                globalStakeCred = C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 globalStakeScript
                directoryProofNode = directoryProofNodeForSymbol directory programmableTokenSymbol
                transferProof txBody = directoryProofRefIndex txBody directoryProofNode
                programmableLogicGlobalRedeemer txBody =
                    TransferAct
                        { plgrTransferProofs = [transferProof txBody]
                        , plgrTransferWdrlIdxs =
                            [ fromIntegral @Int @Integer $
                                BuildTx.findIndexWithdrawal (C.makeStakeAddress networkId transferStakeCred) txBody
                            ]
                        , plgrMintProofs = []
                        , plgrParamsRefIdx =
                            fromIntegral @Int @Integer $
                                BuildTx.findIndexReference (WstQuery.uIn paramsTxIn) txBody
                        }
                programmableGlobalWitness txBody =
                    BuildTx.buildRefScriptWitness
                        (brsGlobal refScripts)
                        C.PlutusScriptV3
                        C.NoScriptDatumForStake
                        (programmableLogicGlobalRedeemer txBody)
                transferScript = Env.tleTransferScript transferEnv
                transferStakeCred = C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 transferScript
                requiredWitnesses = replicate utxoCount (transCredential ownerCred)
                proofNodeForCred cred = blacklistProofNodeForCredential blacklist cred
                witnessRefs = nub (fmap (WstQuery.uIn . proofNodeForCred) requiredWitnesses)
                witnessRefIndex txBody cred =
                    fromIntegral @Int @Integer $
                        BuildTx.findIndexReference (WstQuery.uIn (proofNodeForCred cred)) txBody
                transferRedeemer txBody =
                    fmap (NonmembershipProof . witnessRefIndex txBody) requiredWitnesses
                transferStakeWitness txBody =
                    BuildTx.buildRefScriptWitness
                        (brsTransfer refScripts)
                        C.PlutusScriptV3
                        C.NoScriptDatumForStake
                        (transferRedeemer txBody)
            (proofResult', txBuilder) <-
                BuildTx.runBuildTxT $
                    ( do
                        BuildTx.addReference (WstQuery.uIn paramsTxIn)
                        BuildTx.addReference
                            (WstQuery.uIn (case directoryProofNode of DirectoryProofExists node -> node; DirectoryProofDoesNotExist node -> node))
                        BuildTx.addReference (brsBase refScripts)
                        BuildTx.addReference (brsGlobal refScripts)
                        traverse_ (\txIn -> RefScripts.spendPlutusRefWithInlineDatum txIn (brsBase refScripts) C.PlutusScriptV3 ()) txIns
                        BuildTx.addWithdrawalWithTxBody
                            (C.makeStakeAddress networkId globalStakeCred)
                            (C.Quantity 0)
                            (C.ScriptWitness C.ScriptWitnessForStakeAddr . programmableGlobalWitness)
                        BuildTx.addRequiredSignature opPkh
                        traverse_ BuildTx.addReference witnessRefs
                        BuildTx.addReference (brsTransfer refScripts)
                        BuildTx.addWithdrawalWithTxBody
                            (C.makeStakeAddress networkId transferStakeCred)
                            (C.Quantity 0)
                            (C.ScriptWitness C.ScriptWitnessForStakeAddr . transferStakeWitness)
                        replicateM_ utxoCount $
                            ProgBuildTx.paySmartTokensToDestination (assetName, 1) policyId destCred
                        pure proofResult
                    )
                        <* BuildTx.setMinAdaDepositAll protocolParams
            debugBenchLog "assembled tx builder"
            debugDumpTransferBuilderIfRequested utxoCount refScripts txBuilder
            let operatorCred = C.PaymentCredentialByKey (fst (ProgOperatorEnv.bteOperator operatorEnv))
            returnOutput <- returnOutputFor operatorCred
            debugBenchLog "starting balanceTx"
            (balancedTx, _changes) <-
                case proofResult' of
                    TransferBuildTx.CredentialNotBlacklisted{} ->
                        CoinSelection.balanceTx mempty returnOutput (Utxos.fromApiUtxo (ProgOperatorEnv.bteOperatorUtxos operatorEnv)) txBuilder TrailingChange
                    TransferBuildTx.CredentialBlacklisted{} ->
                        fail "unexpected blacklisted credential in benchmark transfer"
                    TransferBuildTx.NoBlacklistNodes ->
                        fail "blacklist not initialized for benchmark transfer"
            debugBenchLog "balanceTx succeeded"
            pure (balancedTx, CoinSelection.signBalancedTxBody [] balancedTx)

debugBenchLog :: (MonadIO m) => String -> m ()
debugBenchLog msg = do
    enabled <- liftIO (lookupEnv "BENCH_DEBUG_TX")
    when (enabled == Just "1") $
        liftIO (putStrLn ("[bench-debug] " <> msg))

debugDumpTransferBuilderIfRequested ::
    forall env m.
    ( MonadReader env m
    , MonadMockchain C.ConwayEra m
    , Env.HasDirectoryEnv env
    , Env.HasTransferLogicEnv env
    , MonadIO m
    ) =>
    Int ->
    BenchReferenceScripts ->
    BuildTx.TxBuilder C.ConwayEra ->
    m ()
debugDumpTransferBuilderIfRequested utxoCount BenchReferenceScripts{brsBase, brsGlobal, brsTransfer} txBuilder = do
    enabled <- liftIO (lookupEnv "BENCH_DEBUG_TX")
    when (enabled == Just "1") $ do
        utxo <- fromLedgerUTxO C.shelleyBasedEra <$> getUtxo
        directoryEnv <- asks Env.directoryEnv
        transferEnv <- asks Env.transferLogicEnv
        let baseHash = C.hashScript $ C.PlutusScript C.PlutusScriptV3 (Env.dsProgrammableLogicBaseScript directoryEnv)
            globalHash = C.hashScript $ C.PlutusScript C.PlutusScriptV3 (Env.dsProgrammableLogicGlobalScript directoryEnv)
            transferHash = C.hashScript $ C.PlutusScript C.PlutusScriptV3 (Env.tleTransferScript transferEnv)
            body = BuildTx.buildTx txBuilder
            referenceInputs =
                case C.txInsReference body of
                    C.TxInsReferenceNone -> []
                    C.TxInsReference _ refs _ -> refs
            resolvedInputs =
                fmap (renderResolvedInput utxo . fst) (C.txIns body)
            rendered =
                unlines $
                    [ ""
                    , "=== transferSmartTokensTx.MaxOneTokenUtxosByExUnits pre-balance debug ==="
                    , "Programmable input count: " <> show utxoCount
                    , "base hash: " <> show baseHash <> " ref=" <> show brsBase
                    , "global hash: " <> show globalHash <> " ref=" <> show brsGlobal
                    , "transfer hash: " <> show transferHash <> " ref=" <> show brsTransfer
                    , "spending inputs: " <> show (length (C.txIns body))
                    , "reference inputs: " <> show referenceInputs
                    , "outputs: " <> show (length (C.txOuts body))
                    , "resolved spending inputs:"
                    ]
                        <> fmap ("  " <>) resolvedInputs
                        <> [ "pre-balance tx body content:"
                           , show body
                           ]
        liftIO (putStrLn rendered)

debugDumpNamedTxIfRequested ::
    forall env m.
    ( MonadReader env m
    , MonadMockchain C.ConwayEra m
    , Env.HasDirectoryEnv env
    , Env.HasTransferLogicEnv env
    , MonadIO m
    ) =>
    String ->
    C.Tx C.ConwayEra ->
    m ()
debugDumpNamedTxIfRequested label tx = do
    enabled <- liftIO (lookupEnv "BENCH_DEBUG_TX")
    when (enabled == Just "1") $ do
        utxo <- fromLedgerUTxO C.shelleyBasedEra <$> getUtxo
        directoryEnv <- asks Env.directoryEnv
        transferEnv <- asks Env.transferLogicEnv
        let baseHash = C.hashScript $ C.PlutusScript C.PlutusScriptV3 (Env.dsProgrammableLogicBaseScript directoryEnv)
            globalHash = C.hashScript $ C.PlutusScript C.PlutusScriptV3 (Env.dsProgrammableLogicGlobalScript directoryEnv)
            transferHash = C.hashScript $ C.PlutusScript C.PlutusScriptV3 (Env.tleTransferScript transferEnv)
            body = C.getTxBodyContent (C.getTxBody tx)
            referenceInputs =
                case C.txInsReference body of
                    C.TxInsReferenceNone -> []
                    C.TxInsReference _ refs _ -> refs
            resolvedInputs =
                fmap (renderResolvedInput utxo . fst) (C.txIns body)
            rendered =
                unlines $
                    [ ""
                    , "=== tx debug: " <> label <> " ==="
                    , "base hash: " <> show baseHash
                    , "global hash: " <> show globalHash
                    , "transfer hash: " <> show transferHash
                    , "spending inputs: " <> show (length (C.txIns body))
                    , "reference inputs: " <> show referenceInputs
                    , "outputs: " <> show (length (C.txOuts body))
                    , "resolved spending inputs:"
                    ]
                        <> fmap ("  " <>) resolvedInputs
                        <> [ "tx body content:"
                           , show body
                           , "signed tx:"
                           , show tx
                           ]
        liftIO (putStrLn rendered)

debugDumpTransferCandidateIfRequested ::
    forall env m.
    ( MonadReader env m
    , MonadMockchain C.ConwayEra m
    , Env.HasDirectoryEnv env
    , Env.HasTransferLogicEnv env
    , MonadIO m
    ) =>
    Int ->
    BenchReferenceScripts ->
    C.BalancedTxBody C.ConwayEra ->
    C.Tx C.ConwayEra ->
    m ()
debugDumpTransferCandidateIfRequested utxoCount BenchReferenceScripts{brsBase, brsGlobal, brsTransfer} balancedTx tx = do
    enabled <- liftIO (lookupEnv "BENCH_DEBUG_TX")
    when (enabled == Just "1") $ do
        utxo <- fromLedgerUTxO C.shelleyBasedEra <$> getUtxo
        directoryEnv <- asks Env.directoryEnv
        transferEnv <- asks Env.transferLogicEnv
        let baseHash = C.hashScript $ C.PlutusScript C.PlutusScriptV3 (Env.dsProgrammableLogicBaseScript directoryEnv)
            globalHash = C.hashScript $ C.PlutusScript C.PlutusScriptV3 (Env.dsProgrammableLogicGlobalScript directoryEnv)
            transferHash = C.hashScript $ C.PlutusScript C.PlutusScriptV3 (Env.tleTransferScript transferEnv)
            body = C.getTxBodyContent (C.getTxBody tx)
            referenceInputs =
                case C.txInsReference body of
                    C.TxInsReferenceNone -> []
                    C.TxInsReference _ refs _ -> refs
            resolvedInputs =
                fmap (renderResolvedInput utxo . fst) (C.txIns body)
            headerLines =
                [ ""
                , "=== transferSmartTokensTx.MaxOneTokenUtxosByExUnits debug ==="
                , "Programmable input count: " <> show utxoCount
                , "base hash: " <> show baseHash <> " ref=" <> show brsBase
                , "global hash: " <> show globalHash <> " ref=" <> show brsGlobal
                , "transfer hash: " <> show transferHash <> " ref=" <> show brsTransfer
                , "spending inputs: " <> show (length (C.txIns body))
                , "reference inputs: " <> show referenceInputs
                , "outputs: " <> show (length (C.txOuts body))
                , "resolved spending inputs:"
                ]
            rendered =
                unlines $
                    headerLines
                        <> fmap ("  " <>) resolvedInputs
                        <> [ "balanced tx:"
                           , show balancedTx
                           , "tx body content:"
                           , show body
                           , "signed tx:"
                           , show tx
                           ]
        liftIO (putStrLn rendered)

renderResolvedInput :: C.UTxO C.ConwayEra -> C.TxIn -> String
renderResolvedInput utxo txIn =
    case Map.lookup txIn (C.unUTxO utxo) of
        Nothing ->
            show txIn <> " -> missing from utxo"
        Just txOut ->
            show txIn
                <> " -> paymentCred="
                <> show (paymentCredentialFromTxOut txOut)
                <> ", txOut="
                <> show txOut

data DirectoryProofNode era
    = DirectoryProofExists (WstQuery.UTxODat era DirectorySetNode)
    | DirectoryProofDoesNotExist (WstQuery.UTxODat era DirectorySetNode)

directoryProofNodeForSymbol :: [WstQuery.UTxODat era DirectorySetNode] -> CurrencySymbol -> DirectoryProofNode era
directoryProofNodeForSymbol directoryNodes targetSymbol =
    case filter ((<= targetSymbol) . key . WstQuery.uDatum) directoryNodes of
        [] ->
            error ("Missing directory coverage for programmable token policy: " <> show targetSymbol)
        nodes ->
            let node = maximumBy (compare `on` (key . WstQuery.uDatum)) nodes
             in if key (WstQuery.uDatum node) == targetSymbol
                    then DirectoryProofExists node
                    else DirectoryProofDoesNotExist node

-- Input-side transfer proofs are now plain reference-input indices; the global
-- validator (mkProgrammableLogicGlobal) derives exact-match vs covering from the
-- referenced directory node's datum.
directoryProofRefIndex :: (C.IsBabbageBasedEra era) => C.TxBodyContent C.BuildTx era -> DirectoryProofNode era -> Integer
directoryProofRefIndex txBody proofNode =
    fromIntegral @Int @Integer $
        BuildTx.findIndexReference
            (WstQuery.uIn (case proofNode of DirectoryProofExists node -> node; DirectoryProofDoesNotExist node -> node))
            txBody

blacklistProofNodeForCredential :: [WstQuery.UTxODat era BlacklistNode] -> Credential -> WstQuery.UTxODat era BlacklistNode
blacklistProofNodeForCredential blacklistNodes cred =
    case filter ((<= unwrapCredential cred) . blnKey . WstQuery.uDatum) blacklistNodes of
        [] ->
            error ("No blacklist proof node found for credential: " <> show cred)
        nodes ->
            maximumBy (compare `on` (blnKey . WstQuery.uDatum)) nodes

blacklistProofResult :: [WstQuery.UTxODat era BlacklistNode] -> Credential -> TransferBuildTx.FindProofResult era
blacklistProofResult [] _ =
    TransferBuildTx.NoBlacklistNodes
blacklistProofResult blacklistNodes cred =
    let node@WstQuery.UTxODat{WstQuery.uDatum = blnNodeDatum} =
            maximumBy (compare `on` (blnKey . WstQuery.uDatum)) $
                filter ((<= unwrapCredential cred) . blnKey . WstQuery.uDatum) blacklistNodes
     in if blnKey blnNodeDatum == unwrapCredential cred
            then TransferBuildTx.CredentialBlacklisted node
            else TransferBuildTx.CredentialNotBlacklisted node

unwrapCredential ::
    Credential ->
    BuiltinByteString
unwrapCredential = \case
    PubKeyCredential (PubKeyHash bytes) -> bytes
    ScriptCredential (ScriptHash bytes) -> bytes
