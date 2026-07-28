{-# LANGUAGE NamedFieldPuns #-}

module BenchmarkOnchain.ScriptRunner (
    BenchCase,
    EvalKind (..),
    EvalSpec (..),
    ScalingAxis (..),
    mkBenchCase,
    runScriptBenchmark,
    runScriptBenchmarkWithAxes,
) where

import BenchmarkOnchain.Formatting (abbrev, budgetCpuPct, budgetCpuText, budgetMemPct, budgetMemText, formatPercent, formatUnits, indentLines, renderTable, scenarioSeparator, uniquePreservingOrder)
import Data.Either (isRight)
import Data.List (foldl', intercalate, isInfixOf, sortOn)
import Data.ByteString.Short qualified as SBS
import Plutarch.Evaluate (applyArguments, evalScript')
import Plutarch.Script (Script, serialiseScript)
import PlutusLedgerApi.V1.Value (adaSymbol, adaToken, valueOf)
import PlutusLedgerApi.V3 (Address (Address), Credential (PubKeyCredential, ScriptCredential), CurrencySymbol, Data, ExBudget (ExBudget), ExCPU (ExCPU), ExMemory (ExMemory), Lovelace (getLovelace), ScriptContext (scriptContextTxInfo), TxInInfo (txInInfoResolved), TxInfo (..), TxOut (txOutAddress, txOutValue), TxOutRef, Value (Value, getValue))
import PlutusLedgerApi.V3.MintValue (mintValueToMap)
import PlutusTx.AssocMap qualified as Map
import ProgrammableTokens.Test (productionMaxTxExBudget)
import System.Environment (lookupEnv)

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
    , brWdrlOrder :: [String]
    -- ^ The withdrawal map in LEDGER order. Any validator lookup that scans
    -- this map costs more the further along its target sits, and the
    -- participating script hashes -- including the validators' own -- decide
    -- that order. Printing it makes a cost change caused by a reordering
    -- distinguishable from one caused by the implementation.
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
    , sbSize :: Int
    -- ^ serialised (deployed) script size in bytes; constant across witnesses of
    -- the same script.
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
    , erSize :: Int
    }

mkBenchCase :: String -> EvalKind -> Script -> [Data] -> ScriptContext -> BenchCase
mkBenchCase name kind script args ctx =
    BenchCase name (EvalSpec kind script args) ctx

runScriptBenchmark :: String -> [BenchCase] -> (ScriptContext -> [EvalSpec]) -> IO ()
runScriptBenchmark title benchCases scenarioEvalSpecsFromCtx =
    runScriptBenchmarkWithAxes title benchCases scenarioEvalSpecsFromCtx []

-- | Diagnostic mode: with @BENCH_DUMP_SHAPE@ set, print one machine-readable
-- line per benchmark case describing the SHAPE of the transaction its
-- 'ScriptContext' encodes (fee, input/reference-input/output/withdrawal/mint/
-- signatory counts, zero-ada outputs) instead of running the benchmark. Setting
-- @BENCH_DUMP_DETAIL@ to a case-name substring additionally prints that case's
-- per-input and per-output values. This exists so the Plutarch and Aiken
-- harnesses can be proven to build the SAME transaction for each scenario name —
-- a ratio between two structurally different transactions is meaningless. It
-- only READS the contexts; nothing here can change a benchmark number.
dumpShapes :: [BenchCase] -> IO ()
dumpShapes benchCases = do
    detailFilter <- lookupEnv "BENCH_DUMP_DETAIL"
    mapM_ (dumpCase detailFilter) benchCases
  where
    dumpCase detailFilter BenchCase{bcName, bcScenarioCtx} = do
        let txInfo = scriptContextTxInfo bcScenarioCtx
            outs = txInfoOutputs txInfo
            mintMap = Value (mintValueToMap (txInfoMint txInfo))
            zeroAdaOuts = length [o | o <- outs, ada (txOutValue o) == 0]
        putStrLn $
            intercalate
                "\t"
                [ "SHAPE"
                , bcName
                , "fee=" <> show (getLovelace (txInfoFee txInfo))
                , "in=" <> show (length (txInfoInputs txInfo))
                , "ref=" <> show (length (txInfoReferenceInputs txInfo))
                , "out=" <> show (length outs)
                , "wdrl=" <> show (length (Map.toList (txInfoWdrl txInfo)))
                , "mint=" <> show (assetCount mintMap)
                , "sig=" <> show (length (txInfoSignatories txInfo))
                , "zeroAdaOut=" <> show zeroAdaOuts
                , "rdmr=" <> show (length (Map.toList (txInfoRedeemers txInfo)))
                , "inAda=" <> show (sum (fmap (ada . txOutValue . txInInfoResolved) (txInfoInputs txInfo)))
                , "outAda=" <> show (sum (fmap (ada . txOutValue) outs))
                , -- order-sensitive shape: per-input and per-output
                  -- "<ada>:<distinct assets>:<script|pubkey>", in tx order
                  "inSeq=" <> intercalate "," (fmap (slot . txInInfoResolved) (txInfoInputs txInfo))
                , "outSeq=" <> intercalate "," (fmap slot outs)
                , "wdrlSeq=" <> intercalate "," (fmap (take 10 . drop 1 . dropWhile (/= ' ') . show . fst) (Map.toList (txInfoWdrl txInfo)))
                ]
        case detailFilter of
            Just needle | needle `isInfixOf` bcName -> do
                mapM_
                    (\(i, txIn) -> putStrLn ("  in[" <> show i <> "] " <> renderValue (txOutValue (txInInfoResolved txIn))))
                    (zip [0 :: Int ..] (txInfoInputs txInfo))
                mapM_
                    (\(i, o) -> putStrLn ("  out[" <> show i <> "] " <> renderValue (txOutValue o)))
                    (zip [0 :: Int ..] outs)
                putStrLn ("  mint " <> renderValue mintMap)
            _ -> pure ()

    ada v = valueOf v adaSymbol adaToken

    slot o =
        show (ada (txOutValue o))
            <> ":"
            <> show (assetCount (txOutValue o))
            <> ":"
            <> credKind (txOutAddress o)

    credKind (Address (ScriptCredential _) _) = "s"
    credKind (Address (PubKeyCredential _) _) = "p"

    assetCount v =
        length [() | (cs, inner) <- Map.toList (getValue v), (tn, _) <- Map.toList inner, (cs, tn) /= (adaSymbol, adaToken)]

    renderValue v =
        "ada=" <> show (ada v) <> " assets=" <> show (assetCount v) <> " " <> show (fmap (fmap Map.toList) (Map.toList (getValue v)))

runScriptBenchmarkWithAxes :: String -> [BenchCase] -> (ScriptContext -> [EvalSpec]) -> [ScalingAxis] -> IO ()
runScriptBenchmarkWithAxes title benchCases scenarioEvalSpecsFromCtx axes = do
    shapeDump <- lookupEnv "BENCH_DUMP_SHAPE"
    case shapeDump of
        Just _ -> dumpShapes benchCases
        Nothing -> runScriptBenchmarkWithAxes' title benchCases scenarioEvalSpecsFromCtx axes

runScriptBenchmarkWithAxes' :: String -> [BenchCase] -> (ScriptContext -> [EvalSpec]) -> [ScalingAxis] -> IO ()
runScriptBenchmarkWithAxes' title benchCases scenarioEvalSpecsFromCtx axes = do
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
    case axes of
        [] -> pure ()
        _ -> do
            putStrLn ""
            mapM_ putStrLn (renderExtrapolationTables sorted axes)
  where
    caseName BenchRow{brName} = brName

-- | One scaling dimension: a display name plus the (size, scenario-name) series
-- to fit. The named scenarios must all be present in the benchmark catalogue.
data ScalingAxis = ScalingAxis
    { saName :: String
    , saPoints :: [(Integer, String)]
    }

-- | Least-squares fit @y = intercept + slope*n@ over the series.
fitLine :: [(Double, Double)] -> (Double, Double)
fitLine pts =
    let n = fromIntegral (length pts)
        sx = sum (fmap fst pts)
        sy = sum (fmap snd pts)
        sxx = sum (fmap (\(x, _) -> x * x) pts)
        sxy = sum (fmap (uncurry (*)) pts)
        slope = (n * sxy - sx * sy) / (n * sxx - sx * sx)
        intercept = (sy - slope * sx) / n
     in (intercept, slope)

rSquared :: (Double, Double) -> [(Double, Double)] -> Double
rSquared (intercept, slope) pts =
    let meanY = sum (fmap snd pts) / fromIntegral (length pts)
        ssTot = sum (fmap (\(_, y) -> (y - meanY) ^ (2 :: Int)) pts)
        ssRes = sum (fmap (\(x, y) -> (y - (intercept + slope * x)) ^ (2 :: Int)) pts)
     in if ssTot == 0 then 1 else 1 - ssRes / ssTot

-- | Largest @n@ with @intercept + slope*n <= limit@ (Nothing = unbounded within
-- the fit, i.e. non-positive slope).
solveMaxDim :: (Double, Double) -> Double -> Maybe Integer
solveMaxDim (intercept, slope) limit
    | slope <= 0 = Nothing
    | otherwise = Just (max 0 (floor ((limit - intercept) / slope)))

-- | The mainnet per-transaction memory budget (matches 'productionMaxTxExBudget')
-- and the 16.5M figure the Aiken README uses for its published projections —
-- shown side by side so the tables are comparable in both directions.
aikenReadmeMemLimit :: Double
aikenReadmeMemLimit = 16_500_000

renderExtrapolationTables :: [BenchRow] -> [ScalingAxis] -> [String]
renderExtrapolationTables rows axes =
    [ "Scaling extrapolation (least-squares linear fit per dimension)"
    , "Projected max = largest n with fitted cost within budget; binding resource in parentheses."
    ]
        <> renderBasis "validator only (primary script)" brPrimaryBudget
        <> [""]
        <> renderBasis "full scenario (all scripts in the transaction)" brBudget
  where
    ExBudget (ExCPU maxCpuI) (ExMemory maxMemI) = productionMaxTxExBudget
    -- ExCPU/ExMemory wrap CostingInteger (SatInt), which has Show but not
    -- Integral; read . show is the conversion idiom used by Formatting too.
    costingToDouble :: (Show a) => a -> Double
    costingToDouble = read . show
    maxCpu = costingToDouble maxCpuI
    maxMem = costingToDouble maxMemI

    renderBasis basisLabel budgetOf =
        ("Basis: " <> basisLabel)
            : renderTable
                (["Dimension", "Points", "CPU slope/unit", "Mem slope/unit", "R² cpu", "R² mem", "Max n @Mem14M", "Max n @Mem16.5M"])
                (fmap (axisRow budgetOf) axes)

    axisRow budgetOf ScalingAxis{saName, saPoints} =
        let series =
                [ (fromIntegral n, budget)
                | (n, nm) <- saPoints
                , Just budget <- [lookupBudget nm]
                ]
            lookupBudget nm = budgetOf <$> lookup nm rowsByName
            cpuPts = fmap (\(n, ExBudget (ExCPU c) _) -> (n, costingToDouble c)) series
            memPts = fmap (\(n, ExBudget _ (ExMemory m)) -> (n, costingToDouble m)) series
            cpuFit = fitLine cpuPts
            memFit = fitLine memPts
            maxAt memLimit =
                let cpuMax = solveMaxDim cpuFit maxCpu
                    memMax = solveMaxDim memFit memLimit
                 in case (cpuMax, memMax) of
                        (Nothing, Nothing) -> "unbounded"
                        (Just c, Nothing) -> show c <> " (cpu)"
                        (Nothing, Just m) -> show m <> " (mem)"
                        (Just c, Just m) -> if c <= m then show c <> " (cpu)" else show m <> " (mem)"
         in [ saName
            , show (length series)
            , formatUnits (round (snd cpuFit) :: Integer)
            , formatUnits (round (snd memFit) :: Integer)
            , formatR2 (rSquared cpuFit cpuPts)
            , formatR2 (rSquared memFit memPts)
            , maxAt maxMem
            , maxAt aikenReadmeMemLimit
            ]

    formatR2 v = let scaled = fromIntegral (round (v * 10000) :: Integer) / 10000 :: Double in show scaled

    rowsByName = [(brName r, r) | r <- rows]

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
        then pure (BenchRow (bcName bench) ok totalBudget (length results) (erBudget primaryResult) witnessRows breakdown (wdrlOrder bench))
        else do
            putStrLn ("failure logs for " <> bcName bench <> ":")
            mapM_
                ( \(idx, result) ->
                    if erSuccess result
                        then pure ()
                        else putStrLn ("  [" <> show idx <> "] " <> renderEvalKind (erKind result) <> ": " <> erLogText result)
                )
                (zip [0 :: Int ..] results)
            pure (BenchRow (bcName bench) ok totalBudget (length results) (erBudget primaryResult) witnessRows breakdown (wdrlOrder bench))

{- | The withdrawal map as the ledger orders it, abbreviated. Scans over this
map are the one place where a script's OWN hash feeds back into its measured
cost, so a diff of two benchmark reports should make a reordering obvious.
-}
wdrlOrder :: BenchCase -> [String]
wdrlOrder BenchCase{bcScenarioCtx} =
    fmap (renderCred . fst) (Map.toList (txInfoWdrl (scriptContextTxInfo bcScenarioCtx)))
  where
    renderCred (ScriptCredential sh) = "script " <> abbrev (show sh)
    renderCred (PubKeyCredential pkh) = "pubkey " <> abbrev (show pkh)

runEvalSpec :: EvalSpec -> IO EvalResult
runEvalSpec EvalSpec{esKind, esScript, esArgs} = do
    -- Evaluate under the real protocol per-tx budget (10B CPU / 14M mem).
    -- Plutarch's default 'evalScript' restricts memory to 10M — BELOW the tx
    -- limit — which spuriously fails large-but-valid scenarios (e.g. a
    -- 150-input seize needing ~10.7M mem).
    let (res, budget, logs) = evalScript' productionMaxTxExBudget (applyArguments esScript esArgs)
    pure
        EvalResult
            { erKind = esKind
            , erSuccess = isRight res
            , erBudget = budget
            , erLogText = show logs
            , erSize = SBS.length (serialiseScript esScript)
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
    step acc EvalResult{erKind, erBudget, erSize} =
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
                                , sbSize = erSize
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
renderScenarioResult BenchRow{brName, brSuccess, brBudget, brRows, brWdrlOrder} =
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
            <> ( case brWdrlOrder of
                    [] -> []
                    creds -> ["Withdrawal order: " <> intercalate ", " (zipWith (\i c -> show (i :: Int) <> ":" <> c) [0 ..] creds)]
               )
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
renderBreakdownRow ScriptBreakdown{sbCaseName, sbScriptName, sbCount, sbBudget, sbSize} =
    [ sbCaseName
    , sbScriptName
    , show sbCount
    , budgetCpuText sbBudget
    , budgetMemText sbBudget
    , show sbSize
    ]

renderBreakdownTable :: [BenchRow] -> [String]
renderBreakdownTable rows =
    renderTable
        [ "Case"
        , "Script"
        , "Count"
        , "CPU"
        , "Mem"
        , "Size"
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
