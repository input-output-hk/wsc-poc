{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module BenchmarkOnchain.SimpleRunner (
    BenchCase,
    mkTermCase,
    runSimpleBenchmark,
) where

import BenchmarkOnchain.Compile (compileNoTracing)
import BenchmarkOnchain.Formatting (budgetCpuPct, budgetCpuText, budgetMemPct, budgetMemText, formatPercent, formatUnits, renderTable)
import Data.Either (isRight)
import Data.List (sortOn)
import Plutarch.Evaluate (applyArguments, evalScript)
import Plutarch.Internal.Term (Script, Term)
import PlutusLedgerApi.V3 (Data, ExBudget (ExBudget), ExCPU (ExCPU), ExMemory (ExMemory))
import ProgrammableTokens.Test (productionMaxTxExBudget)

data BenchCase = BenchCase
    { bcName :: String
    , bcScript :: Script
    , bcArgs :: [Data]
    }

data BenchRow = BenchRow
    { brName :: String
    , brSuccess :: Bool
    , brBudget :: ExBudget
    }

mkTermCase :: String -> (forall s. Term s a) -> [Data] -> BenchCase
mkTermCase name term args = BenchCase name (compileNoTracing term) args

runSimpleBenchmark :: String -> [BenchCase] -> IO ()
runSimpleBenchmark title benchCases = do
    let ExBudget (ExCPU maxCpu) (ExMemory maxMem) = productionMaxTxExBudget
    putStrLn title
    rows <- traverse runCase benchCases
    let sorted = sortOn caseName rows
    putStrLn $
        "Max tx ex units: CPU "
            <> formatUnits maxCpu
            <> " | Mem "
            <> formatUnits maxMem
    putStrLn ""
    mapM_ putStrLn (renderBenchTable sorted)
  where
    caseName BenchRow{brName} = brName

runCase :: BenchCase -> IO BenchRow
runCase bench = do
    let (res, budget, logs) = evalScript (applyArguments (bcScript bench) (bcArgs bench))
        ok = isRight res
    if ok
        then pure (BenchRow (bcName bench) ok budget)
        else do
            putStrLn ("failure logs for " <> bcName bench <> ": " <> show logs)
            pure (BenchRow (bcName bench) ok budget)

renderBenchRow :: BenchRow -> [String]
renderBenchRow BenchRow{brName, brSuccess, brBudget} =
    [ brName
    , if brSuccess then "PASS" else "FAIL"
    , budgetCpuText brBudget
    , formatPercent (budgetCpuPct brBudget)
    , budgetMemText brBudget
    , formatPercent (budgetMemPct brBudget)
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
        ]
        (fmap renderBenchRow rows)
