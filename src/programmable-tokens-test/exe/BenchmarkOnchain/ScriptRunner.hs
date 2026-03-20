{-# LANGUAGE NamedFieldPuns #-}

module BenchmarkOnchain.ScriptRunner (
    BenchCase,
    EvalKind (..),
    EvalSpec (..),
    mkBenchCase,
    runScriptBenchmark,
) where

import BenchmarkOnchain.Formatting (abbrev, budgetCpuPct, budgetCpuText, budgetMemPct, budgetMemText, formatPercent, formatUnits, indentLines, renderTable, scenarioSeparator, uniquePreservingOrder)
import Data.Either (isRight)
import Data.List (foldl', intercalate, sortOn)
import Plutarch.Evaluate (applyArguments, evalScript)
import Plutarch.Script (Script)
import PlutusLedgerApi.V3 (Credential, CurrencySymbol, Data, ExBudget (ExBudget), ExCPU (ExCPU), ExMemory (ExMemory), ScriptContext, TxOutRef)
import ProgrammableTokens.Test (productionMaxTxExBudget)

data BenchCase = BenchCase
    { bcName :: String
    , bcPrimarySpec :: EvalSpec
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

mkBenchCase :: String -> EvalKind -> Script -> [Data] -> ScriptContext -> BenchCase
mkBenchCase name kind script args ctx =
    BenchCase name (EvalSpec kind script args) ctx

runScriptBenchmark :: String -> [BenchCase] -> (ScriptContext -> [EvalSpec]) -> IO ()
runScriptBenchmark title benchCases scenarioEvalSpecsFromCtx = do
    let ExBudget (ExCPU maxCpu) (ExMemory maxMem) = productionMaxTxExBudget
    putStrLn title
    rows <- traverse (runCase scenarioEvalSpecsFromCtx) benchCases
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
    caseName BenchRow{brName} = brName

runCase :: (ScriptContext -> [EvalSpec]) -> BenchCase -> IO BenchRow
runCase scenarioEvalSpecsFromCtx bench = do
    let primarySpec = bcPrimarySpec bench
        otherSpecs =
            filter ((/= esKind primarySpec) . esKind) (scenarioEvalSpecsFromCtx (bcScenarioCtx bench))
    primaryResult <- runEvalSpec primarySpec
    otherResults <- traverse runEvalSpec otherSpecs
    let results = primaryResult : otherResults
        ok = all erSuccess results
        totalBudget = sumBudgets (fmap erBudget results)
        witnessRows = fmap scriptWitnessRowFromEvalResult results
        breakdown = aggregateBreakdown (bcName bench) results
    if ok
        then pure (BenchRow (bcName bench) ok totalBudget (length results) (erBudget primaryResult) witnessRows breakdown)
        else do
            putStrLn ("failure logs for " <> bcName bench <> ":")
            mapM_
                ( \(idx, result) ->
                    if erSuccess result
                        then pure ()
                        else putStrLn ("  [" <> show idx <> "] " <> renderEvalKind (erKind result) <> ": " <> erLogText result)
                )
                (zip [0 :: Int ..] results)
            pure (BenchRow (bcName bench) ok totalBudget (length results) (erBudget primaryResult) witnessRows breakdown)

runEvalSpec :: EvalSpec -> IO EvalResult
runEvalSpec EvalSpec{esKind, esScript, esArgs} = do
    let (res, budget, logs) = evalScript (applyArguments esScript esArgs)
    pure
        EvalResult
            { erKind = esKind
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
    step acc EvalResult{erKind, erBudget} =
        let scriptName = scriptNameForEvalKind erKind
         in case break ((== scriptName) . sbScriptName) acc of
                (prefix, current : suffix) ->
                    prefix
                        <> [ current
                                { sbCount = sbCount current + 1
                                , sbBudget = sumBudgets [sbBudget current, erBudget]
                                }
                           ]
                        <> suffix
                (prefix, []) ->
                    prefix
                        <> [ ScriptBreakdown
                                { sbCaseName = caseName
                                , sbScriptName = scriptName
                                , sbCount = 1
                                , sbBudget = erBudget
                                }
                           ]

contractTotals :: [ScriptWitnessRow] -> [(String, Int, ExBudget)]
contractTotals witnessRows =
    fmap toTotal (uniquePreservingOrder (fmap swContract witnessRows))
  where
    toTotal contract =
        let matchingRows = filter ((== contract) . swContract) witnessRows
         in (contract, length matchingRows, sumBudgets (fmap swBudget matchingRows))

renderScenarioResult :: BenchRow -> [String]
renderScenarioResult BenchRow{brName, brSuccess, brBudget, brRows} =
    let contracts = uniquePreservingOrder (fmap swContract brRows)
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
        renderScriptBlock idx ScriptWitnessRow{swContract, swPurpose, swDetail, swBudget} =
            [ swContract <> ":"
            , "  witness: #" <> show idx <> if idx == 1 then " (primary)" else ""
            , "  purpose: " <> swPurpose <> " | detail: " <> swDetail
            , "  CPU: "
                <> budgetCpuText swBudget
                <> " | Mem: "
                <> budgetMemText swBudget
                <> " | CPU %: "
                <> formatPercent (budgetCpuPct swBudget)
                <> " | Mem %: "
                <> formatPercent (budgetMemPct swBudget)
            ]
     in [scenarioSeparator, "Scenario: " <> brName <> if brSuccess then " [PASS]" else " [FAIL]", "Scripts: " <> show (length brRows)]
            <> renderContractSummary
            <> ( case contractTotals brRows of
                    [] -> []
                    totals -> "Contract totals:" : fmap renderContractLine totals
               )
            <> [ "Totals:"
               , "  CPU: " <> budgetCpuText brBudget <> " | Mem: " <> budgetMemText brBudget
               , "  CPU %: " <> formatPercent (budgetCpuPct brBudget) <> " | Mem %: " <> formatPercent (budgetMemPct brBudget)
               , ""
               ]
            <> if null brRows
                then ["No Plutus script witnesses"]
                else indentLines 2 (intercalate [""] (zipWith renderScriptBlock [1 :: Int ..] brRows))

renderBenchRow :: BenchRow -> [String]
renderBenchRow BenchRow{brName, brSuccess, brBudget, brScriptCount, brPrimaryBudget} =
    [ brName
    , if brSuccess then "PASS" else "FAIL"
    , budgetCpuText brBudget
    , formatPercent (budgetCpuPct brBudget)
    , budgetMemText brBudget
    , formatPercent (budgetMemPct brBudget)
    , show brScriptCount
    , budgetCpuText brPrimaryBudget
    , budgetMemText brPrimaryBudget
    ]

renderBenchTable :: [BenchRow] -> [String]
renderBenchTable rows =
    renderTable
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
        (fmap renderBenchRow rows)

renderBreakdownRow :: ScriptBreakdown -> [String]
renderBreakdownRow ScriptBreakdown{sbCaseName, sbScriptName, sbCount, sbBudget} =
    [ sbCaseName
    , sbScriptName
    , show sbCount
    , budgetCpuText sbBudget
    , budgetMemText sbBudget
    ]

renderBreakdownTable :: [BenchRow] -> [String]
renderBreakdownTable rows =
    renderTable
        [ "Case"
        , "Script"
        , "Count"
        , "CPU"
        , "Mem"
        ]
        (fmap renderBreakdownRow (concatMap brBreakdown rows))

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

scriptWitnessRowFromEvalResult :: EvalResult -> ScriptWitnessRow
scriptWitnessRowFromEvalResult EvalResult{erKind, erBudget} =
    ScriptWitnessRow
        { swContract = scriptNameForEvalKind erKind
        , swPurpose = scriptPurposeForEvalKind erKind
        , swDetail = scriptDetailForEvalKind erKind
        , swBudget = erBudget
        }
