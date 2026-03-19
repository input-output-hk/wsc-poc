{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Cardano.Api qualified as C
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Either (isRight)
import Data.List (foldl', intercalate, sortOn)
import Data.Maybe (mapMaybe)
import Data.Word (Word8)
import Numeric (showFFloat)
import Plutarch.Evaluate (applyArguments, evalScript)
import Plutarch.Internal.Term (Config (NoTracing), Script, Term, compile)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Value (assetClass, assetClassValue)
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins qualified as BI
import ProgrammableTokens.OffChain.Scripts qualified as OffchainScripts
import ProgrammableTokens.Test (productionMaxTxExBudget)
import ProgrammableTokens.Test.ScriptContext.Builder (ScriptContextBuilder, buildBalancedScriptContext, buildScriptContext, mkAdaValue, withAddress, withFee, withInlineDatum, withInput, withMint, withMintingScript, withOutRef, withOutput, withRedeemer, withReferenceInput, withRewardingScript, withScriptInput, withSigner, withTxOutAddress, withTxOutInlineDatum, withTxOutValue, withValue, withWithdrawal)
import SmartTokens.Contracts.AlwaysYields (palwaysSucceed)
import SmartTokens.Contracts.Issuance (mkProgrammableLogicMinting)
import SmartTokens.Contracts.IssuanceCborHex (IssuanceCborHex (IssuanceCborHex), mkIssuanceCborHexMinting)
import SmartTokens.Contracts.ProgrammableLogicBase (ProgrammableLogicGlobalRedeemer (TransferAct), TokenProof (TokenDoesNotExist, TokenExists), mkProgrammableLogicBase, mkProgrammableLogicGlobal, mkSeizeActRedeemerFromAbsoluteInputIdxs)
import SmartTokens.Contracts.ProtocolParams (mkProtocolParametersMinting)
import SmartTokens.Core.Scripts (ScriptTarget (Production))
import SmartTokens.LinkedList.MintDirectory (DirectoryNodeAction (InitDirectory, InsertDirectoryNode), mkDirectoryNodeMP)
import SmartTokens.LinkedList.SpendDirectory (pmkDirectorySpending)
import SmartTokens.Types.Constants (issuanceCborHexToken, protocolParamsToken)
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (DirectorySetNode))
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (ProgrammableLogicGlobalParams))

main :: IO ()
main = do
    let ExBudget (ExCPU maxCpu) (ExMemory maxMem) = productionMaxTxExBudget
    putStrLn "Onchain script benchmark (NoTracing, one case per redeemer path)"
    rows <- traverse runCase benchCases
    let sorted = sortOn caseName rows
    putStrLn $
        "Max tx ex units: CPU "
            <> formatUnits maxCpu
            <> " | Mem "
            <> formatUnits maxMem
    putStrLn ""
    mapM_ putStrLn (intercalate [""] (fmap renderScenarioResult sorted))
    putStrLn ""
    putStrLn "Transaction totals"
    mapM_ putStrLn (renderBenchTable sorted)
    putStrLn ""
    putStrLn "Per-script breakdown"
    mapM_ putStrLn (renderBreakdownTable sorted)
  where
    caseName BenchRow{brName = name} = name

data BenchCase = BenchCase
    { bcName :: String
    , bcScript :: Script
    , bcArgs :: [Data]
    , bcPrimaryKind :: EvalKind
    , bcScenarioCtx :: ScriptContext
    }

data BenchRow = BenchRow
    { brName :: String
    , brSuccess :: Bool
    , brBudget :: ExBudget
    , brScriptCount :: Int
    , brPrimaryBudget :: ExBudget
    , brRows :: [ScriptWitnessRow]
    , brBreakdown :: [ScriptBreakdown]
    }

data ScriptWitnessRow = ScriptWitnessRow
    { swContract :: String
    , swPurpose :: String
    , swDetail :: String
    , swBudget :: ExBudget
    }

data ScriptBreakdown = ScriptBreakdown
    { sbCaseName :: String
    , sbScriptName :: String
    , sbCount :: Int
    , sbBudget :: ExBudget
    }

runCase :: BenchCase -> IO BenchRow
runCase bench = do
    let primarySpec = EvalSpec (bcPrimaryKind bench) (bcScript bench) (bcArgs bench)
        scenarioSpecs =
            primarySpec
                : filter ((/= bcPrimaryKind bench) . esKind) (scenarioEvalSpecsFromCtx (bcScenarioCtx bench))
    primaryResult <- runEvalSpec primarySpec
    results <- traverse runEvalSpec scenarioSpecs
    let ok = all erSuccess results
        totalBudget = sumBudgets (fmap erBudget results)
        witnessRows = fmap scriptWitnessRowFromEvalResult results
        breakdown = aggregateBreakdown (bcName bench) results
    if ok
        then pure (BenchRow (bcName bench) ok totalBudget (length scenarioSpecs) (erBudget primaryResult) witnessRows breakdown)
        else do
            putStrLn ("failure logs for " <> bcName bench <> ":")
            mapM_
                ( \(idx, result) ->
                    if erSuccess result
                        then pure ()
                        else putStrLn ("  [" <> show idx <> "] " <> renderEvalKind (erKind result) <> ": " <> erLogText result)
                )
                (zip [0 :: Int ..] results)
            pure (BenchRow (bcName bench) ok totalBudget (length scenarioSpecs) (erBudget primaryResult) witnessRows breakdown)

renderScenarioResult :: BenchRow -> [String]
renderScenarioResult BenchRow{brName = name, brSuccess = success, brBudget = budget, brRows = witnessRows} =
    let contracts = uniquePreservingOrder (fmap swContract witnessRows)
        renderContractSummary =
            case contracts of
                [] -> ["Contracts: none"]
                _ -> "Contracts:" : fmap ("  - " <>) contracts
        renderContractLine (contract, witnessCount, contractBudget) =
            "  - "
                <> contract
                <> " ("
                <> show witnessCount
                <> "): CPU "
                <> budgetCpuText contractBudget
                <> " | Mem "
                <> budgetMemText contractBudget
                <> " | CPU % "
                <> formatPercent (budgetCpuPct contractBudget)
                <> " | Mem % "
                <> formatPercent (budgetMemPct contractBudget)
        renderScriptBlock :: Int -> ScriptWitnessRow -> [String]
        renderScriptBlock idx ScriptWitnessRow{swContract = contract, swPurpose = purpose, swDetail = detail, swBudget = rowBudget} =
            [ contract <> ":"
            , "  witness: #" <> show idx <> if idx == 1 then " (primary)" else ""
            , "  purpose: " <> purpose <> " | detail: " <> detail
            , "  CPU: "
                <> budgetCpuText rowBudget
                <> " | Mem: "
                <> budgetMemText rowBudget
                <> " | CPU %: "
                <> formatPercent (budgetCpuPct rowBudget)
                <> " | Mem %: "
                <> formatPercent (budgetMemPct rowBudget)
            ]
     in [scenarioSeparator, "Scenario: " <> name <> if success then " [PASS]" else " [FAIL]", "Scripts: " <> show (length witnessRows)]
            <> renderContractSummary
            <> ( case contractTotals witnessRows of
                    [] -> []
                    totals -> "Contract totals:" : fmap renderContractLine totals
               )
            <> [ "Totals:"
               , "  CPU: " <> budgetCpuText budget <> " | Mem: " <> budgetMemText budget
               , "  CPU %: " <> formatPercent (budgetCpuPct budget) <> " | Mem %: " <> formatPercent (budgetMemPct budget)
               , ""
               ]
            <> if null witnessRows
                then ["No Plutus script witnesses"]
                else indentLines 2 (intercalate [""] (zipWith renderScriptBlock [1 :: Int ..] witnessRows))

renderBenchTable :: [BenchRow] -> [String]
renderBenchTable rows =
    let header =
            [ "Case"
            , "Result"
            , "CPU"
            , "CPU %"
            , "Mem"
            , "Mem %"
            , "Scripts"
            , "Primary CPU"
            , "Primary Mem"
            ]
        renderedRows = fmap renderBenchRow rows
        allRows = header : renderedRows
        widths = fmap maximumColumnWidth (transpose allRows)
        separator = renderSeparator widths
     in renderColumns widths header
            : separator
            : fmap (renderColumns widths) renderedRows

renderBenchRow :: BenchRow -> [String]
renderBenchRow BenchRow{brName = name, brSuccess = success, brBudget = budget, brScriptCount = scriptCount, brPrimaryBudget = primaryBudget} =
    [ name
    , if success then "PASS" else "FAIL"
    , budgetCpuText budget
    , formatPercent (budgetCpuPct budget)
    , budgetMemText budget
    , formatPercent (budgetMemPct budget)
    , show scriptCount
    , budgetCpuText primaryBudget
    , budgetMemText primaryBudget
    ]

renderBreakdownTable :: [BenchRow] -> [String]
renderBreakdownTable rows =
    let header =
            [ "Case"
            , "Script"
            , "Count"
            , "CPU"
            , "Mem"
            ]
        renderedRows = fmap renderBreakdownRow (concatMap brBreakdown rows)
        allRows = header : renderedRows
        widths = fmap maximumColumnWidth (transpose allRows)
        separator = renderSeparator widths
     in renderColumns widths header
            : separator
            : fmap (renderColumns widths) renderedRows

renderBreakdownRow :: ScriptBreakdown -> [String]
renderBreakdownRow ScriptBreakdown{sbCaseName = caseName, sbScriptName = scriptName, sbCount = count, sbBudget = budget} =
    [ caseName
    , scriptName
    , show count
    , budgetCpuText budget
    , budgetMemText budget
    ]

renderColumns :: [Int] -> [String] -> String
renderColumns widths cols =
    intercalate " | " (zipWith renderColumn [0 ..] (zip widths cols))
  where
    renderColumn :: Int -> (Int, String) -> String
    renderColumn idx (width, col)
        | idx <= 1 = padRight width col
        | otherwise = padLeft width col

renderSeparator :: [Int] -> String
renderSeparator widths =
    intercalate "-+-" (fmap (`replicate` '-') widths)

maximumColumnWidth :: [String] -> Int
maximumColumnWidth = maximum . fmap length

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose rows = fmap head rows : transpose (fmap tail rows)

padLeft :: Int -> String -> String
padLeft width s = replicate (width - length s) ' ' <> s

padRight :: Int -> String -> String
padRight width s = s <> replicate (width - length s) ' '

budgetCpuText :: ExBudget -> String
budgetCpuText (ExBudget (ExCPU cpu) _) = formatUnits cpu

budgetMemText :: ExBudget -> String
budgetMemText (ExBudget _ (ExMemory mem)) = formatUnits mem

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

abbrevTo :: Int -> String -> String
abbrevTo limit s
    | length s <= limit = s
    | limit <= 8 = take limit s
    | otherwise =
        let suffixLen = min 10 (max 4 (limit `div` 4))
            prefixLen = max 1 (limit - suffixLen - 3)
         in take prefixLen s <> "..." <> drop (length s - suffixLen) s

abbrev :: String -> String
abbrev = abbrevTo 72

budgetCpuPct :: ExBudget -> Double
budgetCpuPct (ExBudget (ExCPU cpu) _) =
    let ExBudget (ExCPU maxCpu) _ = productionMaxTxExBudget
     in percentage cpu maxCpu

budgetMemPct :: ExBudget -> Double
budgetMemPct (ExBudget _ (ExMemory mem)) =
    let ExBudget _ (ExMemory maxMem) = productionMaxTxExBudget
     in percentage mem maxMem

percentage :: (Show a) => a -> a -> Double
percentage numerator denominator =
    (toDouble numerator * 100) / toDouble denominator
  where
    toDouble :: (Show a) => a -> Double
    toDouble = read . show

formatPercent :: Double -> String
formatPercent pct = showFFloat (Just 2) pct "%"

scenarioSeparator :: String
scenarioSeparator = replicate 80 '='

indentLines :: Int -> [String] -> [String]
indentLines width = fmap ((replicate width ' ') <>)

uniquePreservingOrder :: (Eq a) => [a] -> [a]
uniquePreservingOrder =
    foldl' (\seen x -> if x `elem` seen then seen else seen <> [x]) []

contractTotals :: [ScriptWitnessRow] -> [(String, Int, ExBudget)]
contractTotals witnessRows =
    fmap toTotal (uniquePreservingOrder (fmap swContract witnessRows))
  where
    toTotal contract =
        let matchingRows = filter ((== contract) . swContract) witnessRows
         in (contract, length matchingRows, sumBudgets (fmap swBudget matchingRows))

compileNoTracing :: (forall s. Term s a) -> Script
compileNoTracing t =
    either (error . ("compile failed: " <>) . show) id (compile NoTracing t)

data EvalKind
    = EvalBaseSpend TxOutRef
    | EvalGlobalReward Credential
    | EvalDirectoryMint CurrencySymbol
    | EvalDirectorySpend TxOutRef
    | EvalProgrammableMint CurrencySymbol
    | EvalProtocolParamsMint CurrencySymbol
    | EvalIssuanceMint CurrencySymbol
    | EvalAlwaysSucceedsSpend String TxOutRef
    | EvalAlwaysSucceedsReward String Credential
    deriving stock (Eq, Show)

data EvalSpec = EvalSpec
    { esKind :: EvalKind
    , esScript :: Script
    , esArgs :: [Data]
    }

data EvalResult = EvalResult
    { erKind :: EvalKind
    , erSuccess :: Bool
    , erBudget :: ExBudget
    , erLogText :: String
    }

runEvalSpec :: EvalSpec -> IO EvalResult
runEvalSpec evalSpec = do
    let evalKind = esKind evalSpec
        evalScript' = esScript evalSpec
        evalArgs = esArgs evalSpec
    let (res, budget, logs) = evalScript (applyArguments evalScript' evalArgs)
    pure
        EvalResult
            { erKind = evalKind
            , erSuccess = isRight res
            , erBudget = budget
            , erLogText = show logs
            }

sumBudgets :: [ExBudget] -> ExBudget
sumBudgets =
    foldl' addBudget (ExBudget (ExCPU 0) (ExMemory 0))
  where
    addBudget (ExBudget (ExCPU cpuA) (ExMemory memA)) (ExBudget (ExCPU cpuB) (ExMemory memB)) =
        ExBudget (ExCPU (cpuA + cpuB)) (ExMemory (memA + memB))

renderEvalKind :: EvalKind -> String
renderEvalKind kind =
    case kind of
        EvalBaseSpend outRef -> "programmableLogicBase spending " <> show outRef
        EvalGlobalReward cred -> "programmableLogicGlobal rewarding " <> show cred
        EvalDirectoryMint cs -> "directoryNodeMinting mint " <> show cs
        EvalDirectorySpend outRef -> "directoryNodeSpending spending " <> show outRef
        EvalProgrammableMint cs -> "programmableLogicMinting mint " <> show cs
        EvalProtocolParamsMint cs -> "protocolParamsMinting mint " <> show cs
        EvalIssuanceMint cs -> "issuanceCborHexMinting mint " <> show cs
        EvalAlwaysSucceedsSpend scriptName outRef -> scriptName <> " spending " <> show outRef
        EvalAlwaysSucceedsReward scriptName cred -> scriptName <> " rewarding " <> show cred

aggregateBreakdown :: String -> [EvalResult] -> [ScriptBreakdown]
aggregateBreakdown caseName =
    foldl' step []
  where
    step :: [ScriptBreakdown] -> EvalResult -> [ScriptBreakdown]
    step acc EvalResult{erKind = evalKind, erBudget = budget} =
        let scriptName = scriptNameForEvalKind evalKind
         in case break ((== scriptName) . sbScriptName) acc of
                (prefix, current : suffix) ->
                    prefix
                        <> [ current
                                { sbCount = sbCount current + 1
                                , sbBudget = sumBudgets [sbBudget current, budget]
                                }
                           ]
                        <> suffix
                (prefix, []) ->
                    prefix
                        <> [ ScriptBreakdown
                                { sbCaseName = caseName
                                , sbScriptName = scriptName
                                , sbCount = 1
                                , sbBudget = budget
                                }
                           ]

scriptNameForEvalKind :: EvalKind -> String
scriptNameForEvalKind evalKind =
    case evalKind of
        EvalBaseSpend{} -> "programmableLogicBase"
        EvalGlobalReward{} -> "programmableLogicGlobal"
        EvalDirectoryMint{} -> "directoryNodeMinting"
        EvalDirectorySpend{} -> "directoryNodeSpending"
        EvalProgrammableMint{} -> "programmableLogicMinting"
        EvalProtocolParamsMint{} -> "protocolParamsMinting"
        EvalIssuanceMint{} -> "issuanceCborHexMinting"
        EvalAlwaysSucceedsSpend scriptName _ -> scriptName
        EvalAlwaysSucceedsReward scriptName _ -> scriptName

scriptWitnessRowFromEvalResult :: EvalResult -> ScriptWitnessRow
scriptWitnessRowFromEvalResult EvalResult{erKind = evalKind, erBudget = budget} =
    ScriptWitnessRow
        { swContract = scriptNameForEvalKind evalKind
        , swPurpose = scriptPurposeForEvalKind evalKind
        , swDetail = scriptDetailForEvalKind evalKind
        , swBudget = budget
        }

scriptPurposeForEvalKind :: EvalKind -> String
scriptPurposeForEvalKind evalKind =
    case evalKind of
        EvalBaseSpend{} -> "spending"
        EvalGlobalReward{} -> "rewarding"
        EvalDirectoryMint{} -> "minting"
        EvalDirectorySpend{} -> "spending"
        EvalProgrammableMint{} -> "minting"
        EvalProtocolParamsMint{} -> "minting"
        EvalIssuanceMint{} -> "minting"
        EvalAlwaysSucceedsSpend{} -> "spending"
        EvalAlwaysSucceedsReward{} -> "rewarding"

scriptDetailForEvalKind :: EvalKind -> String
scriptDetailForEvalKind evalKind =
    abbrev $
        case evalKind of
            EvalBaseSpend outRef -> show outRef
            EvalGlobalReward cred -> show cred
            EvalDirectoryMint cs -> show cs
            EvalDirectorySpend outRef -> show outRef
            EvalProgrammableMint cs -> show cs
            EvalProtocolParamsMint cs -> show cs
            EvalIssuanceMint cs -> show cs
            EvalAlwaysSucceedsSpend _ outRef -> show outRef
            EvalAlwaysSucceedsReward _ cred -> show cred

compiledAlwaysSucceedsScript :: Script
compiledAlwaysSucceedsScript =
    compileNoTracing palwaysSucceed

compiledDirectorySpendingScript :: Script
compiledDirectorySpendingScript =
    compileNoTracing pmkDirectorySpending

mkCase :: String -> EvalKind -> (forall s. Term s a) -> [Data] -> ScriptContext -> BenchCase
mkCase name primaryKind term args ctx =
    BenchCase name (compileNoTracing term) args primaryKind ctx

scriptCredentialHash :: Credential -> ScriptHash
scriptCredentialHash (ScriptCredential sh) = sh
scriptCredentialHash cred = error ("expected ScriptCredential, got: " <> show cred)

scriptHashFromCardanoScript :: C.PlutusScript C.PlutusScriptV3 -> ScriptHash
scriptHashFromCardanoScript =
    ScriptHash
        . PV1.toBuiltin
        . C.serialiseToRawBytes
        . C.hashScript
        . C.PlutusScript C.PlutusScriptV3

scenarioEvalSpecsFromCtx :: ScriptContext -> [EvalSpec]
scenarioEvalSpecsFromCtx ctx@(ScriptContext txInfo _ _) =
    mapMaybe inputEvalSpec (txInfoInputs txInfo)
        <> mapMaybe withdrawalEvalSpec (Map.keys (txInfoWdrl txInfo))
        <> mapMaybe mintEvalSpec (Map.keys (mintValueToMap (txInfoMint txInfo)))
  where
    inputEvalSpec :: TxInInfo -> Maybe EvalSpec
    inputEvalSpec input =
        case txOutAddress (txInInfoResolved input) of
            Address (ScriptCredential sh) _
                | sh == progLogicBaseHash ->
                    let outRef = txInInfoOutRef input
                     in Just $
                            EvalSpec
                                (EvalBaseSpend outRef)
                                (compileNoTracing mkProgrammableLogicBase)
                                [toData globalCred, toData (spendingPurposeCtx ctx input)]
                | sh == scriptCredentialHash txD29BaseScriptCred ->
                    let outRef = txInInfoOutRef input
                     in Just $
                            EvalSpec
                                (EvalBaseSpend outRef)
                                (compileNoTracing mkProgrammableLogicBase)
                                [toData txD29GlobalStakeCred, toData (spendingPurposeCtx ctx input)]
                | sh == ScriptHash (bs28 0x44) ->
                    let outRef = txInInfoOutRef input
                     in Just $
                            EvalSpec
                                (EvalDirectorySpend outRef)
                                compiledDirectorySpendingScript
                                [toData protocolParamsCS, toData (spendingPurposeCtx ctx input)]
                | sh == externalAlwaysSucceedsHash ->
                    let outRef = txInInfoOutRef input
                     in Just $
                            EvalSpec
                                (EvalAlwaysSucceedsSpend "alwaysSucceeds" outRef)
                                compiledAlwaysSucceedsScript
                                [toData (spendingPurposeCtx ctx input)]
                | sh == externalAlwaysSucceedsHash2 ->
                    let outRef = txInInfoOutRef input
                     in Just $
                            EvalSpec
                                (EvalAlwaysSucceedsSpend "alwaysSucceeds2" outRef)
                                compiledAlwaysSucceedsScript
                                [toData (spendingPurposeCtx ctx input)]
            _ -> Nothing

    withdrawalEvalSpec :: Credential -> Maybe EvalSpec
    withdrawalEvalSpec cred
        | cred == globalCred =
            Just $
                EvalSpec
                    (EvalGlobalReward cred)
                    (compileNoTracing mkProgrammableLogicGlobal)
                    [toData protocolParamsCS, toData (rewardingPurposeCtx ctx cred)]
        | cred == txD29GlobalStakeCred =
            Just $
                EvalSpec
                    (EvalGlobalReward cred)
                    (compileNoTracing mkProgrammableLogicGlobal)
                    [toData txD29ProtocolParamsCS, toData (rewardingPurposeCtx ctx cred)]
        | cred == txD29TransferLogicStakeCred =
            Just $
                EvalSpec
                    (EvalAlwaysSucceedsReward "transferLogicScript" cred)
                    compiledAlwaysSucceedsScript
                    [toData (rewardingPurposeCtx ctx cred)]
        | otherwise =
            case auxiliaryAlwaysSucceedsScriptName cred of
                Just scriptName ->
                    Just $
                        EvalSpec
                            (EvalAlwaysSucceedsReward scriptName cred)
                            compiledAlwaysSucceedsScript
                            [toData (rewardingPurposeCtx ctx cred)]
                Nothing -> Nothing

    mintEvalSpec :: CurrencySymbol -> Maybe EvalSpec
    mintEvalSpec cs
        | cs == directoryPolicyCS =
            Just $
                EvalSpec
                    (EvalDirectoryMint cs)
                    (compileNoTracing mkDirectoryNodeMP)
                    [toData initRef, toData issuancePolicyCS, toData (mintingPurposeCtx ctx cs)]
        | cs == mintingPolicyCS =
            Just $
                EvalSpec
                    (EvalProgrammableMint cs)
                    (compileNoTracing mkProgrammableLogicMinting)
                    [toData progLogicBaseCred, toData mintingLogicHash, toData (mintingPurposeCtx ctx cs)]
        | cs == protocolParamsCS =
            Just $
                EvalSpec
                    (EvalProtocolParamsMint cs)
                    (compileNoTracing mkProtocolParametersMinting)
                    [toData protocolParamsInitRef, toData (mintingPurposeCtx ctx cs)]
        | cs == issuancePolicyCS =
            Just $
                EvalSpec
                    (EvalIssuanceMint cs)
                    (compileNoTracing mkIssuanceCborHexMinting)
                    [toData issuanceInitRef, toData (mintingPurposeCtx ctx cs)]
        | otherwise = Nothing

    auxiliaryAlwaysSucceedsScriptName :: Credential -> Maybe String
    auxiliaryAlwaysSucceedsScriptName cred
        | cred == ScriptCredential transferLogicHash = Just "transferLogicScript"
        | cred == issuerCred = Just "issuerScript"
        | cred == ScriptCredential mintingLogicHash = Just "mintingLogicScript"
        | cred == ScriptCredential externalAlwaysSucceedsHash = Just "alwaysSucceeds"
        | cred == ScriptCredential externalAlwaysSucceedsHash2 = Just "alwaysSucceeds2"
        | otherwise = Nothing

spendingPurposeCtx :: ScriptContext -> TxInInfo -> ScriptContext
spendingPurposeCtx (ScriptContext txInfo _ _) input =
    let outRef = txInInfoOutRef input
        datum =
            case txOutDatum (txInInfoResolved input) of
                OutputDatum (Datum dat) -> Just (Datum dat)
                _ -> Nothing
     in ScriptContext
            txInfo
            (Redeemer (lookupRedeemerData (Spending outRef) txInfo))
            (SpendingScript outRef datum)

rewardingPurposeCtx :: ScriptContext -> Credential -> ScriptContext
rewardingPurposeCtx (ScriptContext txInfo _ _) cred =
    ScriptContext
        txInfo
        (Redeemer (lookupRedeemerData (Rewarding cred) txInfo))
        (RewardingScript cred)

mintingPurposeCtx :: ScriptContext -> CurrencySymbol -> ScriptContext
mintingPurposeCtx (ScriptContext txInfo _ _) cs =
    ScriptContext
        txInfo
        (Redeemer (lookupRedeemerData (Minting cs) txInfo))
        (MintingScript cs)

lookupRedeemerData :: ScriptPurpose -> TxInfo -> BuiltinData
lookupRedeemerData purpose txInfo =
    case Map.lookup purpose (txInfoRedeemers txInfo) of
        Just (Redeemer dat) -> dat
        Nothing -> PlutusTx.toBuiltinData ()

mkValue :: [(CurrencySymbol, TokenName, Integer)] -> Value
mkValue = foldMap (\(cs, tn, amount) -> assetClassValue (assetClass cs tn) amount)

hexToBytes :: String -> BS.ByteString
hexToBytes = BS.pack . go
  where
    go [] = []
    go [_] = error "hexToBuiltin: odd-length hex string"
    go (a : b : rest) =
        fromIntegral (hexNibble a * 16 + hexNibble b) : go rest

    hexNibble :: Char -> Int
    hexNibble c
        | c >= '0' && c <= '9' = ord c - ord '0'
        | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
        | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
        | otherwise = error ("hexToBuiltin: invalid hex char: " <> [c])

hexToBuiltin :: String -> BuiltinByteString
hexToBuiltin = PV1.toBuiltin . hexToBytes

currencySymbolHex :: String -> CurrencySymbol
currencySymbolHex = CurrencySymbol . hexToBuiltin

tokenNameHex :: String -> TokenName
tokenNameHex = TokenName . hexToBuiltin

scriptHashHex :: String -> ScriptHash
scriptHashHex = ScriptHash . hexToBuiltin

pubKeyHashHex :: String -> PubKeyHash
pubKeyHashHex = PubKeyHash . hexToBuiltin

txIdHex :: String -> TxId
txIdHex = TxId . hexToBuiltin

txId32 :: Word8 -> Word8 -> TxId
txId32 hi lo =
    TxId (PV1.toBuiltin (BS.replicate 31 hi <> BS.singleton lo))

txOutRef32 :: Word8 -> Word8 -> Integer -> TxOutRef
txOutRef32 hi lo idx = TxOutRef (txId32 hi lo) idx

assetUnitHex :: String -> Integer -> (CurrencySymbol, TokenName, Integer)
assetUnitHex unit quantity =
    let policyId = take 56 unit
        tokenName = drop 56 unit
     in (currencySymbolHex policyId, tokenNameHex tokenName, quantity)

bs28 :: Word8 -> BuiltinByteString
bs28 w = PV1.toBuiltin (BS.replicate 28 w)

maxBs28 :: BuiltinByteString
maxBs28 = bs28 0xff

computeRegisteredCs :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> CurrencySymbol
computeRegisteredCs prefix postfix hashedParam =
    CurrencySymbol $
        BI.blake2b_224
            ( versionHeader
                <> prefix
                <> BI.serialiseData (PlutusTx.toBuiltinData hashedParam)
                <> postfix
            )
  where
    versionHeader = PV1.toBuiltin (BS.pack [3])

scriptAddress :: ScriptHash -> Address
scriptAddress sh = Address (ScriptCredential sh) Nothing

scriptAddressWithStakeCredential :: ScriptHash -> Credential -> Address
scriptAddressWithStakeCredential sh cred =
    Address (ScriptCredential sh) (Just (StakingHash cred))

scriptAddressWithSignerStake :: ScriptHash -> PubKeyHash -> Address
scriptAddressWithSignerStake sh pkh =
    Address (ScriptCredential sh) (Just (StakingHash (PubKeyCredential pkh)))

pubKeyAddress :: PubKeyHash -> Address
pubKeyAddress pkh = Address (PubKeyCredential pkh) Nothing

withRefInputDatumValue :: TxOutRef -> Address -> Value -> BuiltinData -> ScriptContextBuilder
withRefInputDatumValue ref addr value dat =
    withReferenceInput
        ( withOutRef ref
            <> withAddress addr
            <> withValue value
            <> withInlineDatum dat
        )

-- Shared constants for mini-ledger scripts
signerPkh :: PubKeyHash
signerPkh = PubKeyHash (bs28 0x01)

recipientPkh :: PubKeyHash
recipientPkh = PubKeyHash (bs28 0x02)

protocolParamsCS :: CurrencySymbol
protocolParamsCS = CurrencySymbol (bs28 0x10)

directoryNodeCS :: CurrencySymbol
directoryNodeCS = CurrencySymbol (bs28 0x11)

progLogicBaseHash :: ScriptHash
progLogicBaseHash = ScriptHash (bs28 0x12)

progLogicBaseCred :: Credential
progLogicBaseCred = ScriptCredential progLogicBaseHash

globalScriptHash :: ScriptHash
globalScriptHash = ScriptHash (bs28 0x13)

globalCred :: Credential
globalCred = ScriptCredential globalScriptHash

issuerLogicHash :: ScriptHash
issuerLogicHash = ScriptHash (bs28 0x14)

issuerCred :: Credential
issuerCred = ScriptCredential issuerLogicHash

transferLogicHash :: ScriptHash
transferLogicHash = ScriptHash (bs28 0x15)

mintingLogicHash :: ScriptHash
mintingLogicHash = ScriptHash (bs28 0x16)

issuancePolicyCS :: CurrencySymbol
issuancePolicyCS = CurrencySymbol (bs28 0x17)

directoryPolicyCS :: CurrencySymbol
directoryPolicyCS = CurrencySymbol (bs28 0x18)

mintingPolicyCS :: CurrencySymbol
mintingPolicyCS = CurrencySymbol (bs28 0x19)

nonProgrammableCS :: CurrencySymbol
nonProgrammableCS = CurrencySymbol (bs28 0x1a)

programmableTransferCS :: CurrencySymbol
programmableTransferCS = CurrencySymbol (bs28 0x1b)

programmableTransferCS2 :: CurrencySymbol
programmableTransferCS2 = CurrencySymbol (bs28 0x1c)

programmableTransferCS3 :: CurrencySymbol
programmableTransferCS3 = CurrencySymbol (bs28 0x1d)

nonProgrammableCS2 :: CurrencySymbol
nonProgrammableCS2 = CurrencySymbol (bs28 0x1e)

externalAlwaysSucceedsHash :: ScriptHash
externalAlwaysSucceedsHash = ScriptHash (bs28 0x21)

externalAlwaysSucceedsHash2 :: ScriptHash
externalAlwaysSucceedsHash2 = ScriptHash (bs28 0x22)

manyPubKeyInputCount :: Integer
manyPubKeyInputCount = 50

tailCS :: CurrencySymbol
tailCS = CurrencySymbol maxBs28

seizeInputTxId :: TxId
seizeInputTxId = txId32 0x5e 0x12

leadingPubKeyInputTxId :: TxId
leadingPubKeyInputTxId = txId32 0x00 0x00

transferManyInputTxId :: TxId
transferManyInputTxId = txId32 0xfa 0x11

directoryProgrammableNodeRef :: TxOutRef
directoryProgrammableNodeRef = txOutRef32 0xbb 0x10 0

directoryProgrammableNode2Ref :: TxOutRef
directoryProgrammableNode2Ref = txOutRef32 0xbb 0x11 0

directoryProgrammableNode3Ref :: TxOutRef
directoryProgrammableNode3Ref = txOutRef32 0xbb 0x12 0

externalScriptInputRef :: TxOutRef
externalScriptInputRef = txOutRef32 0xff 0xff 0

mainnetDexSwapInputTxId :: TxId
mainnetDexSwapInputTxId = txId32 0xc3 0x11

mainnetDexBaseInputRef :: TxOutRef
mainnetDexBaseInputRef = TxOutRef mainnetDexSwapInputTxId 0

mainnetDexPoolInputRef :: TxOutRef
mainnetDexPoolInputRef = txOutRef32 0xc3 0x12 0

mainnetDexFeeInputRef :: TxOutRef
mainnetDexFeeInputRef = txOutRef32 0xc3 0x13 0

progInputRef :: TxOutRef
progInputRef = txOutRef32 0xf0 0x0d 0

paramRef :: TxOutRef
paramRef = txOutRef32 0xaa 0x00 0

dirNodeRef :: TxOutRef
dirNodeRef = txOutRef32 0xbb 0x00 0

initRef :: TxOutRef
initRef = txOutRef32 0x1a 0x1a 0

insertNodeInRef :: TxOutRef
insertNodeInRef = txOutRef32 0xc0 0xde 0

issuanceRef :: TxOutRef
issuanceRef = txOutRef32 0xc0 0xfe 0

protocolParamsInitRef :: TxOutRef
protocolParamsInitRef = txOutRef32 0xaa 0x11 0

issuanceInitRef :: TxOutRef
issuanceInitRef = txOutRef32 0xbb 0x21 0

protocolParamsAlwaysFailHash :: ScriptHash
protocolParamsAlwaysFailHash =
    scriptHashFromCardanoScript (OffchainScripts.protocolParamsSpendingScript Production)

issuanceAlwaysFailHash :: ScriptHash
issuanceAlwaysFailHash =
    scriptHashFromCardanoScript (OffchainScripts.issuanceCborHexSpendingScript Production)

protocolParamsDatum :: ProgrammableLogicGlobalParams
protocolParamsDatum =
    ProgrammableLogicGlobalParams directoryNodeCS progLogicBaseCred

issuanceDatum :: BuiltinData
issuanceDatum =
    PlutusTx.toBuiltinData (IssuanceCborHex "0d" "0e")

directoryCoveringNode :: DirectorySetNode
directoryCoveringNode =
    DirectorySetNode
        (CurrencySymbol "")
        tailCS
        (ScriptCredential transferLogicHash)
        issuerCred
        (CurrencySymbol "")

directoryProgrammableNode :: DirectorySetNode
directoryProgrammableNode =
    DirectorySetNode
        programmableTransferCS
        tailCS
        (ScriptCredential transferLogicHash)
        issuerCred
        (CurrencySymbol "")

directoryProgrammableNode2 :: DirectorySetNode
directoryProgrammableNode2 =
    DirectorySetNode
        programmableTransferCS2
        tailCS
        (ScriptCredential transferLogicHash)
        issuerCred
        (CurrencySymbol "")

directoryProgrammableNode3 :: DirectorySetNode
directoryProgrammableNode3 =
    DirectorySetNode
        programmableTransferCS3
        tailCS
        (ScriptCredential transferLogicHash)
        issuerCred
        (CurrencySymbol "")

seizeInputIdxsFor :: Integer -> [Integer]
seizeInputIdxsFor n = [0 .. (n - 1)]

seizeInputAddr :: Address
seizeInputAddr = scriptAddressWithSignerStake progLogicBaseHash signerPkh

seizeInputValue :: Value
seizeInputValue =
    mkAdaValue 3_000_000
        <> mkValue [(programmableTransferCS, TokenName "0c", 1)]

seizeCorrespondingOutputValue :: Value
seizeCorrespondingOutputValue = mkAdaValue 3_000_000

seizeResidualOutputValueFor :: Integer -> Value
seizeResidualOutputValueFor n =
    mkValue [(programmableTransferCS, TokenName "0c", n)]

seizeInputBuilder :: Integer -> ScriptContextBuilder
seizeInputBuilder idx =
    withScriptInput
        (PlutusTx.toBuiltinData ())
        ( withOutRef (TxOutRef seizeInputTxId idx)
            <> withAddress seizeInputAddr
            <> withValue seizeInputValue
        )

leadingPubKeyInputBuilder :: Integer -> ScriptContextBuilder
leadingPubKeyInputBuilder idx =
    withInput
        ( withOutRef (TxOutRef leadingPubKeyInputTxId idx)
            <> withAddress (pubKeyAddress signerPkh)
            <> withValue (mkAdaValue 2_000_000)
        )

externalScriptInputBuilder :: ScriptContextBuilder
externalScriptInputBuilder =
    withScriptInput
        (PlutusTx.toBuiltinData ())
        ( withOutRef externalScriptInputRef
            <> withAddress (scriptAddress externalAlwaysSucceedsHash)
            <> withValue (mkAdaValue 2_000_000)
        )

seizeCorrespondingOutputBuilder :: ScriptContextBuilder
seizeCorrespondingOutputBuilder =
    withOutput
        ( withTxOutAddress seizeInputAddr
            <> withTxOutValue seizeCorrespondingOutputValue
        )

seizeResidualOutputBuilder :: Integer -> ScriptContextBuilder
seizeResidualOutputBuilder n =
    withOutput
        ( withTxOutAddress seizeInputAddr
            <> withTxOutValue (seizeResidualOutputValueFor n)
        )

-- Contexts / redeemer paths
baseSpendingCtx :: ScriptContext
baseSpendingCtx =
    let ScriptContext txInfo _ _ = globalTransferCtx
     in ScriptContext
            txInfo
            (Redeemer (PlutusTx.toBuiltinData ()))
            (SpendingScript progInputRef Nothing)

globalTransferCtx :: ScriptContext
globalTransferCtx =
    buildBalancedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData $ TransferAct [TokenExists 1] [])
            globalCred
            0
            <> withSigner signerPkh
            <> withWithdrawal (ScriptCredential transferLogicHash) 0
            <> withScriptInput
                (PlutusTx.toBuiltinData ())
                ( withOutRef progInputRef
                    <> withAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                    <> withValue
                        ( mkAdaValue 10_000_000
                            <> mkValue [(programmableTransferCS, TokenName "0c", 5)]
                        )
                )
            <> withOutput
                ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                    <> withTxOutValue (mkAdaValue 3_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 2)])
                )
            <> withOutput
                ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash recipientPkh)
                    <> withTxOutValue (mkAdaValue 3_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 3)])
                )
            <> withOutput
                ( withTxOutAddress (pubKeyAddress signerPkh)
                    <> withTxOutValue (mkAdaValue 2_000_000)
                )
            <> withRefInputDatumValue
                paramRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue
                dirNodeRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                (PlutusTx.toBuiltinData directoryProgrammableNode)
        )

globalTransferDoesNotExistCtx :: ScriptContext
globalTransferDoesNotExistCtx =
    buildBalancedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData $ TransferAct [TokenDoesNotExist 1] [])
            globalCred
            0
            <> withSigner signerPkh
            <> withScriptInput
                (PlutusTx.toBuiltinData ())
                ( withOutRef progInputRef
                    <> withAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                    <> withValue
                        ( mkAdaValue 10_000_000
                            <> mkValue
                                [(nonProgrammableCS, TokenName "np", 4)]
                        )
                )
            <> withOutput
                ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                    <> withTxOutValue (mkAdaValue 3_000_000)
                )
            <> withOutput
                ( withTxOutAddress (pubKeyAddress signerPkh)
                    <> withTxOutValue (mkAdaValue 2_000_000 <> mkValue [(nonProgrammableCS, TokenName "np", 4)])
                )
            <> withRefInputDatumValue
                paramRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue
                dirNodeRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                (PlutusTx.toBuiltinData directoryCoveringNode)
        )

globalTransferMixedManyCtx :: ScriptContext
globalTransferMixedManyCtx =
    buildBalancedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData $ TransferAct [TokenDoesNotExist 1, TokenExists 2, TokenExists 3, TokenExists 4, TokenDoesNotExist 1] [])
            globalCred
            0
            <> withSigner signerPkh
            <> withWithdrawal (ScriptCredential transferLogicHash) 0
            <> withScriptInput
                (PlutusTx.toBuiltinData ())
                ( withOutRef progInputRef
                    <> withAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                    <> withValue
                        ( mkAdaValue 10_000_000
                            <> mkValue
                                [ (nonProgrammableCS, TokenName "np1", 4)
                                , (programmableTransferCS, TokenName "0c", 5)
                                , (programmableTransferCS2, TokenName "1c", 7)
                                , (programmableTransferCS3, TokenName "2c", 9)
                                , (nonProgrammableCS2, TokenName "np2", 6)
                                ]
                        )
                )
            <> withOutput
                ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                    <> withTxOutValue
                        ( mkAdaValue 3_000_000
                            <> mkValue
                                [ (programmableTransferCS, TokenName "0c", 5)
                                , (programmableTransferCS2, TokenName "1c", 7)
                                , (programmableTransferCS3, TokenName "2c", 9)
                                ]
                        )
                )
            <> withOutput
                ( withTxOutAddress (pubKeyAddress signerPkh)
                    <> withTxOutValue
                        ( mkAdaValue 2_000_000
                            <> mkValue
                                [ (nonProgrammableCS, TokenName "np1", 4)
                                , (nonProgrammableCS2, TokenName "np2", 6)
                                ]
                        )
                )
            <> withRefInputDatumValue
                paramRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue
                dirNodeRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                (PlutusTx.toBuiltinData directoryCoveringNode)
            <> withRefInputDatumValue
                directoryProgrammableNodeRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                (PlutusTx.toBuiltinData directoryProgrammableNode)
            <> withRefInputDatumValue
                directoryProgrammableNode2Ref
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                (PlutusTx.toBuiltinData directoryProgrammableNode2)
            <> withRefInputDatumValue
                directoryProgrammableNode3Ref
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                (PlutusTx.toBuiltinData directoryProgrammableNode3)
        )

transferManyInputAddr :: Address
transferManyInputAddr = scriptAddressWithSignerStake progLogicBaseHash signerPkh

transferManyInputValue :: Value
transferManyInputValue =
    mkAdaValue 3_000_000
        <> mkValue
            [ (nonProgrammableCS, TokenName "np", 1)
            , (programmableTransferCS, TokenName "0c", 1)
            ]

transferManyInputBuilder :: Integer -> ScriptContextBuilder
transferManyInputBuilder idx =
    withScriptInput
        (PlutusTx.toBuiltinData ())
        ( withOutRef (TxOutRef transferManyInputTxId idx)
            <> withAddress transferManyInputAddr
            <> withValue transferManyInputValue
        )

mkGlobalTransferManyCtx :: Integer -> ScriptContext
mkGlobalTransferManyCtx inputCount =
    let inputRefs = [0 .. (inputCount - 1)]
        scriptInputsBuilder = mconcat (map transferManyInputBuilder inputRefs)
        qtyInFirstOutput = inputCount `div` 2
        qtyInSecondOutput = inputCount - qtyInFirstOutput
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData $ TransferAct [TokenDoesNotExist 1, TokenExists 2] [])
                globalCred
                0
                <> withSigner signerPkh
                <> withWithdrawal (ScriptCredential transferLogicHash) 0
                <> scriptInputsBuilder
                <> withOutput
                    ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                        <> withTxOutValue (mkAdaValue 3_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", qtyInFirstOutput)])
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash recipientPkh)
                        <> withTxOutValue (mkAdaValue 3_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", qtyInSecondOutput)])
                    )
                <> withOutput
                    ( withTxOutAddress (pubKeyAddress signerPkh)
                        <> withTxOutValue
                            ( mkAdaValue 2_000_000
                                <> mkValue [(nonProgrammableCS, TokenName "np", inputCount)]
                            )
                    )
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    (PlutusTx.toBuiltinData protocolParamsDatum)
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (PlutusTx.toBuiltinData directoryCoveringNode)
                <> withRefInputDatumValue
                    directoryProgrammableNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (PlutusTx.toBuiltinData directoryProgrammableNode)
            )

globalTransfer5Ctx :: ScriptContext
globalTransfer5Ctx = mkGlobalTransferManyCtx 5

globalTransfer10Ctx :: ScriptContext
globalTransfer10Ctx = mkGlobalTransferManyCtx 10

globalTransfer15Ctx :: ScriptContext
globalTransfer15Ctx = mkGlobalTransferManyCtx 15

mkGlobalSeizeCtx :: Integer -> ScriptContext
mkGlobalSeizeCtx seizeInputCount =
    let seizeInputIdxs = seizeInputIdxsFor seizeInputCount
        seizeInputRefs = [0 .. (seizeInputCount - 1)]
        seizeRedeemer = mkSeizeActRedeemerFromAbsoluteInputIdxs 1 seizeInputIdxs 0
        seizeInputsBuilder = mconcat (map seizeInputBuilder seizeInputRefs)
        correspondingOutputsBuilder = mconcat (replicate (fromIntegral seizeInputCount) seizeCorrespondingOutputBuilder)
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData seizeRedeemer)
                globalCred
                0
                <> withWithdrawal issuerCred 0
                <> seizeInputsBuilder
                -- put the residual output first so it is last in tx output order
                -- (withOutput prepends in the builder)
                <> seizeResidualOutputBuilder seizeInputCount
                <> correspondingOutputsBuilder
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    (PlutusTx.toBuiltinData protocolParamsDatum)
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (PlutusTx.toBuiltinData directoryProgrammableNode)
            )

globalSeize1Ctx :: ScriptContext
globalSeize1Ctx =
    mkGlobalSeizeCtx 1

globalSeize5Ctx :: ScriptContext
globalSeize5Ctx =
    mkGlobalSeizeCtx 5

globalSeize10Ctx :: ScriptContext
globalSeize10Ctx =
    mkGlobalSeizeCtx 10

globalSeize20Ctx :: ScriptContext
globalSeize20Ctx =
    mkGlobalSeizeCtx 20

mkGlobalSeizeExternalScriptAndManyPubKeyCtx :: Integer -> ScriptContext
mkGlobalSeizeExternalScriptAndManyPubKeyCtx pubKeyInputCount =
    let pubKeyInputIdxs = [0 .. (pubKeyInputCount - 1)]
        seizeRedeemer =
            mkSeizeActRedeemerFromAbsoluteInputIdxs
                1
                -- The Plutarch validator accounts for every spending redeemer in
                -- the transaction, so the external script spend is listed too.
                [pubKeyInputCount, pubKeyInputCount + 1]
                0
        pubKeyInputsBuilder = mconcat (map leadingPubKeyInputBuilder pubKeyInputIdxs)
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData seizeRedeemer)
                globalCred
                0
                <> withWithdrawal issuerCred 0
                <> pubKeyInputsBuilder
                <> seizeInputBuilder 0
                <> externalScriptInputBuilder
                <> seizeResidualOutputBuilder 1
                <> seizeCorrespondingOutputBuilder
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    (PlutusTx.toBuiltinData protocolParamsDatum)
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (PlutusTx.toBuiltinData directoryProgrammableNode)
            )

globalSeize1ExternalScript50PubKeyCtx :: ScriptContext
globalSeize1ExternalScript50PubKeyCtx =
    mkGlobalSeizeExternalScriptAndManyPubKeyCtx manyPubKeyInputCount

directoryInitCtx :: ScriptContext
directoryInitCtx =
    let mintValue = mkValue [(directoryPolicyCS, TokenName "", 1)]
        emptyNodeDatum =
            PlutusTx.dataToBuiltinData $
                PlutusTx.List
                    [ PlutusTx.B ""
                    , PlutusTx.B (BS.replicate 30 0xff)
                    , PlutusTx.Constr 0 [PlutusTx.B ""]
                    , PlutusTx.Constr 0 [PlutusTx.B ""]
                    , PlutusTx.B ""
                    ]
     in buildScriptContext
            ( withRedeemer (PlutusTx.toBuiltinData InitDirectory)
                <> withMintingScript mintValue (PlutusTx.toBuiltinData InitDirectory)
                <> withInput
                    ( withOutRef initRef
                        <> withAddress (pubKeyAddress signerPkh)
                        <> withValue (mkAdaValue 10_000_000)
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddress (ScriptHash (bs28 0x44)))
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue)
                        <> withTxOutInlineDatum emptyNodeDatum
                    )
            )

directoryInsertCtx :: ScriptContext
directoryInsertCtx =
    let issuancePrefix = "0d"
        issuancePostfix = "0e"
        hashedMintingParam = bs28 0x55
        insertProtocolParamsDatum =
            ProgrammableLogicGlobalParams directoryPolicyCS progLogicBaseCred
        insertedCs = computeRegisteredCs issuancePrefix issuancePostfix hashedMintingParam
        insertedCsBs = case insertedCs of
            CurrencySymbol bs -> bs
        insertedToken = TokenName insertedCsBs
        insertRedeemer = InsertDirectoryNode insertedCs (ScriptHash hashedMintingParam)
        nodeMintValue = mkValue [(directoryPolicyCS, insertedToken, 1)]
        registeredAssetMintValue = mkValue [(insertedCs, TokenName "0b", 1)]
        coveringNode =
            DirectorySetNode
                (CurrencySymbol "")
                tailCS
                (ScriptCredential transferLogicHash)
                issuerCred
                (CurrencySymbol "")
        coveringOutput =
            DirectorySetNode
                (CurrencySymbol "")
                insertedCs
                (ScriptCredential transferLogicHash)
                issuerCred
                (CurrencySymbol "")
        insertedNode =
            DirectorySetNode
                insertedCs
                tailCS
                (ScriptCredential transferLogicHash)
                issuerCred
                (CurrencySymbol "")
     in buildScriptContext
            ( withRedeemer (PlutusTx.toBuiltinData insertRedeemer)
                <> withMintingScript nodeMintValue (PlutusTx.toBuiltinData insertRedeemer)
                <> withMint registeredAssetMintValue (PlutusTx.toBuiltinData ())
                <> withScriptInput
                    (PlutusTx.toBuiltinData ())
                    ( withOutRef insertNodeInRef
                        <> withAddress (scriptAddress (ScriptHash (bs28 0x44)))
                        <> withValue (mkAdaValue 2_000_000 <> mkValue [(directoryPolicyCS, TokenName "", 1)])
                        <> withInlineDatum (PlutusTx.toBuiltinData coveringNode)
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddress (ScriptHash (bs28 0x44)))
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mkValue [(directoryPolicyCS, TokenName "", 1)])
                        <> withTxOutInlineDatum (PlutusTx.toBuiltinData coveringOutput)
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddress (ScriptHash (bs28 0x44)))
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mkValue [(directoryPolicyCS, insertedToken, 1)])
                        <> withTxOutInlineDatum (PlutusTx.toBuiltinData insertedNode)
                    )
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    (PlutusTx.toBuiltinData insertProtocolParamsDatum)
                <> withRefInputDatumValue
                    issuanceRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 2_000_000 <> mkValue [(issuancePolicyCS, issuanceCborHexToken, 1)])
                    (PlutusTx.toBuiltinData $ IssuanceCborHex issuancePrefix issuancePostfix)
            )

programmableMintCtx :: ScriptContext
programmableMintCtx =
    let scriptRedeemer = PlutusTx.toBuiltinData (ScriptCredential mintingLogicHash)
        mintValue = mkValue [(mintingPolicyCS, TokenName "0c", 1)]
     in buildBalancedScriptContext
            ( withFee 0
                <> withRedeemer scriptRedeemer
                <> withMintingScript mintValue scriptRedeemer
                <> withWithdrawal (ScriptCredential mintingLogicHash) 0
                <> withOutput
                    ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue)
                    )
            )

programmableBurnCtx :: ScriptContext
programmableBurnCtx =
    let scriptRedeemer = PlutusTx.toBuiltinData (ScriptCredential mintingLogicHash)
        burnValue = mkValue [(mintingPolicyCS, TokenName "0c", -1)]
     in buildBalancedScriptContext
            ( withRedeemer scriptRedeemer
                <> withMintingScript burnValue scriptRedeemer
                <> withWithdrawal (ScriptCredential mintingLogicHash) 0
            )

protocolParamsMintCtx :: ScriptContext
protocolParamsMintCtx =
    let mintValue = mkValue [(protocolParamsCS, protocolParamsToken, 1)]
     in buildBalancedScriptContext
            ( withMintingScript mintValue (PlutusTx.toBuiltinData ())
                <> withInput
                    ( withOutRef protocolParamsInitRef
                        <> withAddress (pubKeyAddress signerPkh)
                        <> withValue (mkAdaValue 2_000_000)
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddress protocolParamsAlwaysFailHash)
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue)
                        <> withTxOutInlineDatum (PlutusTx.toBuiltinData protocolParamsDatum)
                    )
            )

issuanceMintCtx :: ScriptContext
issuanceMintCtx =
    let mintValue = mkValue [(issuancePolicyCS, issuanceCborHexToken, 1)]
     in buildBalancedScriptContext
            ( withMintingScript mintValue (PlutusTx.toBuiltinData ())
                <> withInput
                    ( withOutRef issuanceInitRef
                        <> withAddress (pubKeyAddress signerPkh)
                        <> withValue (mkAdaValue 2_000_000)
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddress issuanceAlwaysFailHash)
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue)
                        <> withTxOutInlineDatum issuanceDatum
                    )
            )

-- Mainnet-inspired DEX transaction benchmark fixture from:
-- c3111df78f97a8f7ed4934510e04d9c777339a00d5eac26e123471ff4ab954ff
--
-- The original tx processes 16 swap requests against a NIGHT/ADA pool. Here we
-- model the same broad shape on the programmable-token mini-ledger:
-- - 16 request inputs at programmableLogicBase with an always-succeeds stake script
-- - 1 pool input at programmableLogicBase with a second always-succeeds stake script
-- - 1 pubkey input that is consumed entirely as fees
-- - 16 request outputs at programmableLogicBase with pubkey stake credentials
-- - 1 pool continuation output carrying the pool state NFT
mainnetDexTxHash :: String
mainnetDexTxHash = "c3111df78f97a8f7ed4934510e04d9c777339a00d5eac26e123471ff4ab954ff"

mainnetDexSwapInputAda :: Integer
mainnetDexSwapInputAda = 3_280_000

mainnetDexFeeAda :: Integer
mainnetDexFeeAda = 635_743_434

mainnetDexNightToken :: TokenName
mainnetDexNightToken = tokenNameHex "4e49474854"

mainnetDexPoolStateToken :: TokenName
mainnetDexPoolStateToken = tokenNameHex "504f4f4c"

mainnetDexSwapStakeCred :: Credential
mainnetDexSwapStakeCred = ScriptCredential externalAlwaysSucceedsHash

mainnetDexPoolStakeCred :: Credential
mainnetDexPoolStakeCred = ScriptCredential externalAlwaysSucceedsHash2

mainnetDexSwapInputAddr :: Address
mainnetDexSwapInputAddr =
    scriptAddressWithStakeCredential progLogicBaseHash mainnetDexSwapStakeCred

mainnetDexPoolAddr :: Address
mainnetDexPoolAddr =
    scriptAddressWithStakeCredential progLogicBaseHash mainnetDexPoolStakeCred

mainnetDexSwapInputNightQtys :: [Integer]
mainnetDexSwapInputNightQtys =
    [ 10_000_000_000
    , 10_833_333_333
    , 10_833_333_333
    , 7_721_468_569
    , 7_721_468_569
    , 7_721_468_569
    ]
        <> replicate 10 5_447_167_285

mainnetDexSwapOutputAdaQtys :: [Integer]
mainnetDexSwapOutputAdaQtys =
    [ 640_185_917
    , 634_261_663
    , 628_419_934
    , 622_659_204
    , 616_977_977
    , 611_374_795
    , 605_848_231
    , 600_396_893
    , 595_019_417
    , 589_714_474
    , 825_925_825
    , 815_607_230
    , 805_482_200
    , 1_112_392_741
    , 1_093_337_592
    , 992_934_466
    ]

mainnetDexSwapOutputNightQtys :: [Integer]
mainnetDexSwapOutputNightQtys =
    [2, 0, 8, 2, 4, 2, 5, 1, 9, 9, 7, 0, 0, 8, 1, 6]

mainnetDexPoolInputAda :: Integer
mainnetDexPoolInputAda = 139_587_470_341

mainnetDexPoolInputNight :: Integer
mainnetDexPoolInputNight = 1_147_114_273_642

mainnetDexPoolOutputAda :: Integer
mainnetDexPoolOutputAda = 127_849_411_782

mainnetDexPoolOutputNight :: Integer
mainnetDexPoolOutputNight = 1_256_417_018_801

mainnetDexOutputStakePkhAt :: Integer -> PubKeyHash
mainnetDexOutputStakePkhAt idx =
    PubKeyHash (bs28 (fromIntegral (0x30 + idx)))

mainnetDexSwapInputBuilder :: Integer -> Integer -> ScriptContextBuilder
mainnetDexSwapInputBuilder idx nightQty =
    withScriptInput
        (PlutusTx.toBuiltinData ())
        ( withOutRef (TxOutRef mainnetDexSwapInputTxId idx)
            <> withAddress mainnetDexSwapInputAddr
            <> withValue
                ( mkAdaValue (fromIntegral mainnetDexSwapInputAda)
                    <> mkValue [(programmableTransferCS, mainnetDexNightToken, nightQty)]
                )
        )

mainnetDexPoolInputBuilder :: ScriptContextBuilder
mainnetDexPoolInputBuilder =
    withScriptInput
        (PlutusTx.toBuiltinData ())
        ( withOutRef mainnetDexPoolInputRef
            <> withAddress mainnetDexPoolAddr
            <> withValue
                ( mkAdaValue (fromIntegral mainnetDexPoolInputAda)
                    <> mkValue
                        [ (programmableTransferCS, mainnetDexNightToken, mainnetDexPoolInputNight)
                        , (nonProgrammableCS, mainnetDexPoolStateToken, 1)
                        ]
                )
        )

mainnetDexFeeInputBuilder :: ScriptContextBuilder
mainnetDexFeeInputBuilder =
    withInput
        ( withOutRef mainnetDexFeeInputRef
            <> withAddress (pubKeyAddress signerPkh)
            <> withValue (mkAdaValue (fromIntegral mainnetDexFeeAda))
        )

mainnetDexSwapOutputBuilder :: Integer -> Integer -> Integer -> ScriptContextBuilder
mainnetDexSwapOutputBuilder idx adaQty nightQty =
    withOutput $
        withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash (mainnetDexOutputStakePkhAt idx))
            <> withTxOutValue
                ( mkAdaValue (fromIntegral adaQty)
                    <> mkValue [(programmableTransferCS, mainnetDexNightToken, nightQty) | nightQty > 0]
                )

mainnetDexPoolOutputBuilder :: ScriptContextBuilder
mainnetDexPoolOutputBuilder =
    withOutput $
        withTxOutAddress mainnetDexPoolAddr
            <> withTxOutValue
                ( mkAdaValue (fromIntegral mainnetDexPoolOutputAda)
                    <> mkValue
                        [ (programmableTransferCS, mainnetDexNightToken, mainnetDexPoolOutputNight)
                        , (nonProgrammableCS, mainnetDexPoolStateToken, 1)
                        ]
                )

stripZeroChangeOutput :: ScriptContext -> ScriptContext
stripZeroChangeOutput ctx =
    let txInfo = scriptContextTxInfo ctx
     in case reverse (txInfoOutputs txInfo) of
            lastOutput : remainingOutputsReversed
                | txOutValue lastOutput == mempty ->
                    ctx
                        { scriptContextTxInfo =
                            txInfo
                                { txInfoOutputs = reverse remainingOutputsReversed
                                }
                        }
            _ -> ctx

mainnetDexGlobalTransferCtx :: ScriptContext
mainnetDexGlobalTransferCtx =
    let swapInputsBuilder =
            mconcat $
                zipWith
                    mainnetDexSwapInputBuilder
                    [0 .. 15]
                    mainnetDexSwapInputNightQtys
        swapOutputsBuilder =
            mconcat $
                zipWith3
                    mainnetDexSwapOutputBuilder
                    [0 .. 15]
                    mainnetDexSwapOutputAdaQtys
                    mainnetDexSwapOutputNightQtys
     in stripZeroChangeOutput $
            buildBalancedScriptContext
                ( withFee mainnetDexFeeAda
                    <> withRewardingScript
                        (PlutusTx.toBuiltinData $ TransferAct [TokenExists 2, TokenDoesNotExist 1] [])
                        globalCred
                        0
                    <> withSigner signerPkh
                    <> withWithdrawal (ScriptCredential transferLogicHash) 0
                    <> withWithdrawal mainnetDexSwapStakeCred 0
                    <> withWithdrawal mainnetDexPoolStakeCred 0
                    <> swapInputsBuilder
                    <> mainnetDexPoolInputBuilder
                    <> mainnetDexFeeInputBuilder
                    <> swapOutputsBuilder
                    <> mainnetDexPoolOutputBuilder
                    <> withRefInputDatumValue
                        paramRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                        (PlutusTx.toBuiltinData protocolParamsDatum)
                    <> withRefInputDatumValue
                        dirNodeRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                        (PlutusTx.toBuiltinData directoryCoveringNode)
                    <> withRefInputDatumValue
                        directoryProgrammableNodeRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                        (PlutusTx.toBuiltinData directoryProgrammableNode)
                )

mainnetDexBaseSpendingCtx :: ScriptContext
mainnetDexBaseSpendingCtx =
    let ScriptContext txInfo _ _ = mainnetDexGlobalTransferCtx
     in ScriptContext
            txInfo
            (Redeemer (PlutusTx.toBuiltinData ()))
            (SpendingScript mainnetDexBaseInputRef Nothing)

-- Exact transaction benchmark fixture from:
-- https://preview.cexplorer.io/tx/d29ce2a9f79a70a91d83a40e0e1cf346ab94979b6f0ba001de8a89895aa518df?tab=content
-- The context is reconstructed from chain data (inputs/outputs/ref-inputs/redeemers/withdrawals).
txD29Hash :: String
txD29Hash = "d29ce2a9f79a70a91d83a40e0e1cf346ab94979b6f0ba001de8a89895aa518df"

txD29ProtocolParamsCS :: CurrencySymbol
txD29ProtocolParamsCS = currencySymbolHex "c348817600e8cd22ddf01b4fef9437a4d768700c313415fc2ad5ee09"

txD29GlobalStakeCred :: Credential
txD29GlobalStakeCred = ScriptCredential (scriptHashHex "36775ef231d797f8234268d4a7ecb51d5c44c096685dfb2cb881d7d3")

txD29TransferLogicStakeCred :: Credential
txD29TransferLogicStakeCred = ScriptCredential (scriptHashHex "5b7d265e7d862937c6e1a5266bba7dc685b786b1cc33551aa66867c9")

txD29BaseScriptCred :: Credential
txD29BaseScriptCred = ScriptCredential (scriptHashHex "fca77bcce1e5e73c97a0bfa8c90f7cd2faff6fd6ed5b6fec1c04eefa")

txD29InputStakePkh :: PubKeyHash
txD29InputStakePkh = pubKeyHashHex "37cd439e255465bce092323ed5b2e8b6d66581a1e170355c2a69ce62"

txD29RecipientStakePkh :: PubKeyHash
txD29RecipientStakePkh = pubKeyHashHex "586bc2eab5e93fef22664bf8c6db809b5ae90e2d848afb90a38bea8f"

txD29WstUnit :: String
txD29WstUnit = "b34a184f1f2871aa4d33544caecefef5242025f45c3fa5213d7662a9575354"

txD29ScriptInputRef :: TxOutRef
txD29ScriptInputRef = TxOutRef "0f0bef2ec112ff86d062cd5cf8e0172dd1449b740a6677fa348de09ee9a1fef3" 1

txD29PubInputRef :: TxOutRef
txD29PubInputRef = TxOutRef "57e6c1c0c8a19799a15682b5dc89882757c41aba70622a1a7f948996c8f16be7" 2

txD29RefInput0Ref :: TxOutRef
txD29RefInput0Ref = TxOutRef "09dab423b34f670ebb49f62fb3b9c25a4c03c53689cd74ab415df2d7fa90734c" 2

txD29RefInput1Ref :: TxOutRef
txD29RefInput1Ref = TxOutRef "bb16e777bfc4977f922a163ab90bf8415c21e4cd3800ef68a956f494a55193e8" 0

txD29RefInput2Ref :: TxOutRef
txD29RefInput2Ref = TxOutRef "d0975d3bbb024c3f0df82614ac800435f56f13a14a2480c63938a6ac385ea556" 2

txD29SpendingRedeemer :: BuiltinData
txD29SpendingRedeemer = PlutusTx.dataToBuiltinData (PlutusTx.Constr 0 [])

txD29GlobalStakeRedeemer :: BuiltinData
txD29GlobalStakeRedeemer =
    PlutusTx.toBuiltinData $ TransferAct [TokenExists 2] []

txD29TransferLogicStakeRedeemer :: BuiltinData
txD29TransferLogicStakeRedeemer =
    PlutusTx.dataToBuiltinData $
        PlutusTx.List
            [ PlutusTx.Constr 0 [PlutusTx.I 1]
            , PlutusTx.Constr 0 [PlutusTx.I 1]
            , PlutusTx.Constr 0 [PlutusTx.I 1]
            ]

txD29ProtocolParamsDatumData :: BuiltinData
txD29ProtocolParamsDatumData =
    PlutusTx.dataToBuiltinData $
        PlutusTx.List
            [ PlutusTx.B (hexToBytes "f1f838a525637791ca06d1fd415358a27ba829024ee0b90af5e32f14")
            , PlutusTx.Constr 1 [PlutusTx.B (hexToBytes "fca77bcce1e5e73c97a0bfa8c90f7cd2faff6fd6ed5b6fec1c04eefa")]
            ]

txD29RefDatum1Data :: BuiltinData
txD29RefDatum1Data =
    PlutusTx.dataToBuiltinData $
        PlutusTx.List
            [ PlutusTx.B (hexToBytes "338ef18819c855fc8970be65827abb86089b58fad24b31cff332ce97")
            , PlutusTx.B (hexToBytes "87f2531e16923b81a0f3f507363db6b7cebf30735cb76b94543ca7ac")
            ]

txD29DirectoryNodeDatumData :: BuiltinData
txD29DirectoryNodeDatumData =
    PlutusTx.dataToBuiltinData $
        PlutusTx.List
            [ PlutusTx.B (hexToBytes "b34a184f1f2871aa4d33544caecefef5242025f45c3fa5213d7662a9")
            , PlutusTx.B (hexToBytes "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
            , PlutusTx.Constr 1 [PlutusTx.B (hexToBytes "5b7d265e7d862937c6e1a5266bba7dc685b786b1cc33551aa66867c9")]
            , PlutusTx.Constr 1 [PlutusTx.B (hexToBytes "b427cc9d5b829bbf66a784066fa3c03684fee2bfac7b571654ae8f55")]
            ]

txD29ScriptAddressWithStake :: PubKeyHash -> Address
txD29ScriptAddressWithStake stakePkh =
    Address txD29BaseScriptCred (Just (StakingHash (PubKeyCredential stakePkh)))

txD29PubAddress :: Address
txD29PubAddress = Address (PubKeyCredential txD29InputStakePkh) Nothing

txD29ReferenceScriptAddress0 :: Address
txD29ReferenceScriptAddress0 = Address (ScriptCredential (scriptHashHex "b8459bc5cc1dae2962abad27450f7dcc81376ae6ebda8d25e942508e")) Nothing

txD29ReferenceScriptAddress1 :: Address
txD29ReferenceScriptAddress1 = Address (ScriptCredential (scriptHashHex "9f6381bdf046fd671ebac45c1ad59263e282fa00a7083620fb449413")) Nothing

txD29ReferenceScriptAddress2 :: Address
txD29ReferenceScriptAddress2 = Address (ScriptCredential (scriptHashHex "9c0b7840bea475b3b20714bfa2d105df42a13283ca53f62692ec3f8d")) Nothing

txD29ScriptInput :: TxInInfo
txD29ScriptInput =
    TxInInfo
        { txInInfoOutRef = txD29ScriptInputRef
        , txInInfoResolved =
            TxOut
                { txOutAddress = txD29ScriptAddressWithStake txD29InputStakePkh
                , txOutValue = mkAdaValue 1_137_840 <> mkValue [assetUnitHex txD29WstUnit 42]
                , txOutDatum = NoOutputDatum
                , txOutReferenceScript = Nothing
                }
        }

txD29PubInput :: TxInInfo
txD29PubInput =
    TxInInfo
        { txInInfoOutRef = txD29PubInputRef
        , txInInfoResolved =
            TxOut
                { txOutAddress = txD29PubAddress
                , txOutValue = mkAdaValue 9_998_519_974
                , txOutDatum = NoOutputDatum
                , txOutReferenceScript = Nothing
                }
        }

txD29RefInput0 :: TxInInfo
txD29RefInput0 =
    TxInInfo
        { txInInfoOutRef = txD29RefInput0Ref
        , txInInfoResolved =
            TxOut
                { txOutAddress = txD29ReferenceScriptAddress0
                , txOutValue = mkAdaValue 1_383_510 <> mkValue [assetUnitHex "c348817600e8cd22ddf01b4fef9437a4d768700c313415fc2ad5ee0950726f746f636f6c506172616d73" 1]
                , txOutDatum = OutputDatum (Datum txD29ProtocolParamsDatumData)
                , txOutReferenceScript = Nothing
                }
        }

txD29RefInput1 :: TxInInfo
txD29RefInput1 =
    TxInInfo
        { txInInfoOutRef = txD29RefInput1Ref
        , txInInfoResolved =
            TxOut
                { txOutAddress = txD29ReferenceScriptAddress1
                , txOutValue = mkAdaValue 1_430_920 <> mkValue [assetUnitHex "8ea903ae40af4e0cff4164285b050c6a24ef10576b9fbf091a870b3f338ef18819c855fc8970be65827abb86089b58fad24b31cff332ce97" 1]
                , txOutDatum = OutputDatum (Datum txD29RefDatum1Data)
                , txOutReferenceScript = Nothing
                }
        }

txD29RefInput2 :: TxInInfo
txD29RefInput2 =
    TxInInfo
        { txInInfoOutRef = txD29RefInput2Ref
        , txInInfoResolved =
            TxOut
                { txOutAddress = txD29ReferenceScriptAddress2
                , txOutValue = mkAdaValue 1_732_620 <> mkValue [assetUnitHex "f1f838a525637791ca06d1fd415358a27ba829024ee0b90af5e32f14b34a184f1f2871aa4d33544caecefef5242025f45c3fa5213d7662a9" 1]
                , txOutDatum = OutputDatum (Datum txD29DirectoryNodeDatumData)
                , txOutReferenceScript = Nothing
                }
        }

txD29Output0 :: TxOut
txD29Output0 =
    TxOut
        { txOutAddress = txD29ScriptAddressWithStake txD29InputStakePkh
        , txOutValue = mkAdaValue 1_137_840 <> mkValue [assetUnitHex txD29WstUnit 28]
        , txOutDatum = NoOutputDatum
        , txOutReferenceScript = Nothing
        }

txD29Output1 :: TxOut
txD29Output1 =
    TxOut
        { txOutAddress = txD29ScriptAddressWithStake txD29RecipientStakePkh
        , txOutValue = mkAdaValue 1_133_530 <> mkValue [assetUnitHex txD29WstUnit 14]
        , txOutDatum = NoOutputDatum
        , txOutReferenceScript = Nothing
        }

txD29Output2 :: TxOut
txD29Output2 =
    TxOut
        { txOutAddress = txD29PubAddress
        , txOutValue = mkAdaValue 9_997_039_904
        , txOutDatum = NoOutputDatum
        , txOutReferenceScript = Nothing
        }

-- /// --- Plutarch baseline --------------------------------------------------------------------------
-- ///
-- /// Replicate a Preview transaction based on the Plutarch version of the
-- /// programmable logic validators. This transaction has been used as a benchmark
-- /// for optimisation and improvements over the Aiken version.
-- ///
-- /// Characteristics:
-- /// - 2 inputs: one script-locked, one for fuel
-- /// - 3 outputs: two with programmable tokens, and one for change
-- /// - one programmable token kind (i.e. two nodes from the registry referenced)
-- /// - transfer logic is script-based, provided as withdrawal
-- ///
-- /// Source: https://preview.cexplorer.io/tx/d29ce2a9f79a70a91d83a40e0e1cf346ab94979b6f0ba001de8a89895aa518df?tab=contracts
-- /// ------------------------------------------------------------------------------------------------
txD29TxInfo :: TxInfo
txD29TxInfo =
    TxInfo
        { txInfoInputs = [txD29ScriptInput, txD29PubInput]
        , txInfoReferenceInputs = [txD29RefInput0, txD29RefInput1, txD29RefInput2]
        , txInfoOutputs = [txD29Output0, txD29Output1, txD29Output2]
        , txInfoFee = 346_540
        , txInfoMint = UnsafeMintValue Map.empty
        , txInfoTxCerts = []
        , txInfoWdrl = Map.unsafeFromList [(txD29GlobalStakeCred, 0), (txD29TransferLogicStakeCred, 0)]
        , txInfoValidRange = always
        , txInfoSignatories = [txD29InputStakePkh]
        , txInfoRedeemers =
            Map.unsafeFromList
                [ (Spending txD29ScriptInputRef, Redeemer txD29SpendingRedeemer)
                , (Rewarding txD29GlobalStakeCred, Redeemer txD29GlobalStakeRedeemer)
                , (Rewarding txD29TransferLogicStakeCred, Redeemer txD29TransferLogicStakeRedeemer)
                ]
        , txInfoData = Map.empty
        , txInfoId = txIdHex txD29Hash
        , txInfoVotes = Map.empty
        , txInfoProposalProcedures = []
        , txInfoCurrentTreasuryAmount = Nothing
        , txInfoTreasuryDonation = Nothing
        }

txD29ProgrammableLogicBaseSpendingCtx :: ScriptContext
txD29ProgrammableLogicBaseSpendingCtx =
    ScriptContext
        txD29TxInfo
        (Redeemer txD29SpendingRedeemer)
        (SpendingScript txD29ScriptInputRef Nothing)

txD29ProgrammableLogicBaseStakeCtx :: ScriptContext
txD29ProgrammableLogicBaseStakeCtx =
    ScriptContext
        txD29TxInfo
        (Redeemer txD29GlobalStakeRedeemer)
        (RewardingScript txD29GlobalStakeCred)

benchCases :: [BenchCase]
benchCases =
    [ mkCase "programmableLogicBase" (EvalBaseSpend progInputRef) mkProgrammableLogicBase [toData globalCred, toData baseSpendingCtx] baseSpendingCtx
    , mkCase "programmableLogicGlobal.TransferAct" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransferCtx] globalTransferCtx
    , mkCase "programmableLogicGlobal.TransferAct.TokenDoesNotExist" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransferDoesNotExistCtx] globalTransferDoesNotExistCtx
    , mkCase "programmableLogicGlobal.TransferAct.MixedMany" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransferMixedManyCtx] globalTransferMixedManyCtx
    , mkCase "programmableLogicGlobal.TransferAct.Spend5Utxos" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransfer5Ctx] globalTransfer5Ctx
    , mkCase "programmableLogicGlobal.TransferAct.Spend10Utxos" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransfer10Ctx] globalTransfer10Ctx
    , mkCase "programmableLogicGlobal.TransferAct.Spend15Utxos" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransfer15Ctx] globalTransfer15Ctx
    , mkCase "programmableLogicGlobal.SeizeAct1" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalSeize1Ctx] globalSeize1Ctx
    , mkCase
        "programmableLogicGlobal.SeizeAct1.ExternalScriptAnd50PubKeyInputs"
        (EvalGlobalReward globalCred)
        mkProgrammableLogicGlobal
        [toData protocolParamsCS, toData globalSeize1ExternalScript50PubKeyCtx]
        globalSeize1ExternalScript50PubKeyCtx
    , mkCase "programmableLogicGlobal.SeizeAct5" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalSeize5Ctx] globalSeize5Ctx
    , mkCase "programmableLogicGlobal.SeizeAct10" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalSeize10Ctx] globalSeize10Ctx
    , mkCase "programmableLogicGlobal.SeizeAct20" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalSeize20Ctx] globalSeize20Ctx
    , mkCase "directoryNodeMinting.InitDirectory" (EvalDirectoryMint directoryPolicyCS) mkDirectoryNodeMP [toData initRef, toData issuancePolicyCS, toData directoryInitCtx] directoryInitCtx
    , mkCase "directoryNodeMinting.InsertDirectoryNode" (EvalDirectoryMint directoryPolicyCS) mkDirectoryNodeMP [toData initRef, toData issuancePolicyCS, toData directoryInsertCtx] directoryInsertCtx
    , mkCase "programmableLogicMinting.Mint" (EvalProgrammableMint mintingPolicyCS) mkProgrammableLogicMinting [toData progLogicBaseCred, toData mintingLogicHash, toData programmableMintCtx] programmableMintCtx
    , mkCase "programmableLogicMinting.Burn" (EvalProgrammableMint mintingPolicyCS) mkProgrammableLogicMinting [toData progLogicBaseCred, toData mintingLogicHash, toData programmableBurnCtx] programmableBurnCtx
    , mkCase "protocolParamsMinting" (EvalProtocolParamsMint protocolParamsCS) mkProtocolParametersMinting [toData protocolParamsInitRef, toData protocolParamsMintCtx] protocolParamsMintCtx
    , mkCase "issuanceCborHexMinting" (EvalIssuanceMint issuancePolicyCS) mkIssuanceCborHexMinting [toData issuanceInitRef, toData issuanceMintCtx] issuanceMintCtx
    , mkCase
        ("programmableLogicBase.Tx." <> take 8 mainnetDexTxHash <> ".Spending")
        (EvalBaseSpend mainnetDexBaseInputRef)
        mkProgrammableLogicBase
        [ toData globalCred
        , toData mainnetDexBaseSpendingCtx
        ]
        mainnetDexBaseSpendingCtx
    , mkCase
        ("programmableLogicGlobal.TransferAct.Tx." <> take 8 mainnetDexTxHash <> ".NightAdaDex16Swaps")
        (EvalGlobalReward globalCred)
        mkProgrammableLogicGlobal
        [ toData protocolParamsCS
        , toData mainnetDexGlobalTransferCtx
        ]
        mainnetDexGlobalTransferCtx
    , mkCase
        ("programmableLogicBase.Tx." <> take 8 txD29Hash <> ".Spending")
        (EvalBaseSpend txD29ScriptInputRef)
        mkProgrammableLogicBase
        [ toData txD29GlobalStakeCred
        , toData txD29ProgrammableLogicBaseSpendingCtx
        ]
        txD29ProgrammableLogicBaseSpendingCtx
    , mkCase
        ("programmableLogicBase.Tx." <> take 8 txD29Hash <> ".Stake")
        (EvalGlobalReward txD29GlobalStakeCred)
        mkProgrammableLogicGlobal
        [ toData txD29ProtocolParamsCS
        , toData txD29ProgrammableLogicBaseStakeCtx
        ]
        txD29ProgrammableLogicBaseStakeCtx
    ]
