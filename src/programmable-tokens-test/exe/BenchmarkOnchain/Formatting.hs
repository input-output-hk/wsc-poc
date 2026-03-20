module BenchmarkOnchain.Formatting (
    abbrev,
    budgetCpuPct,
    budgetCpuText,
    budgetMemPct,
    budgetMemText,
    formatPercent,
    formatUnits,
    indentLines,
    renderTable,
    scenarioSeparator,
    uniquePreservingOrder,
) where

import Data.List (intercalate)
import Numeric (showFFloat)
import PlutusLedgerApi.V3 (ExBudget (ExBudget), ExCPU (ExCPU), ExMemory (ExMemory))
import ProgrammableTokens.Test (productionMaxTxExBudget)

renderTable :: [String] -> [[String]] -> [String]
renderTable header rows =
    let allRows = header : rows
        widths = fmap maximumColumnWidth (transpose allRows)
        separator = renderSeparator widths
     in renderColumns widths header
            : separator
            : fmap (renderColumns widths) rows

abbrev :: String -> String
abbrev = abbrevTo 72

budgetCpuPct :: ExBudget -> Double
budgetCpuPct (ExBudget (ExCPU cpu) _) =
    let ExBudget (ExCPU maxCpu) _ = productionMaxTxExBudget
     in percentage cpu maxCpu

budgetCpuText :: ExBudget -> String
budgetCpuText (ExBudget (ExCPU cpu) _) = formatUnits cpu

budgetMemPct :: ExBudget -> Double
budgetMemPct (ExBudget _ (ExMemory mem)) =
    let ExBudget _ (ExMemory maxMem) = productionMaxTxExBudget
     in percentage mem maxMem

budgetMemText :: ExBudget -> String
budgetMemText (ExBudget _ (ExMemory mem)) = formatUnits mem

formatPercent :: Double -> String
formatPercent pct = showFFloat (Just 2) pct "%"

formatUnits :: (Show a) => a -> String
formatUnits = formatThousands . show

indentLines :: Int -> [String] -> [String]
indentLines width = fmap ((replicate width ' ') <>)

scenarioSeparator :: String
scenarioSeparator = replicate 80 '='

uniquePreservingOrder :: (Eq a) => [a] -> [a]
uniquePreservingOrder = foldl step []
  where
    step seen x
        | x `elem` seen = seen
        | otherwise = seen <> [x]

abbrevTo :: Int -> String -> String
abbrevTo limit s
    | length s <= limit = s
    | limit <= 8 = take limit s
    | otherwise =
        let suffixLen = min 10 (max 4 (limit `div` 4))
            prefixLen = max 1 (limit - suffixLen - 3)
         in take prefixLen s <> "..." <> drop (length s - suffixLen) s

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
    let (prefix, suffix) = splitAt n xs
     in prefix : chunksOf n suffix

formatThousands :: String -> String
formatThousands ('-' : rest) = '-' : formatThousands rest
formatThousands digits =
    intercalate "," . reverse . fmap reverse . chunksOf 3 . reverse $ digits

maximumColumnWidth :: [String] -> Int
maximumColumnWidth = maximum . fmap length

padLeft :: Int -> String -> String
padLeft width s = replicate (width - length s) ' ' <> s

padRight :: Int -> String -> String
padRight width s = s <> replicate (width - length s) ' '

percentage :: (Show a) => a -> a -> Double
percentage numerator denominator =
    (toDouble numerator * 100) / toDouble denominator
  where
    toDouble :: (Show a) => a -> Double
    toDouble = read . show

renderColumns :: [Int] -> [String] -> String
renderColumns widths cols =
    intercalate " | " (zipWith renderColumn [0 :: Int ..] (zip widths cols))
  where
    renderColumn idx (width, col)
        | idx <= 1 = padRight width col
        | otherwise = padLeft width col

renderSeparator :: [Int] -> String
renderSeparator widths =
    intercalate "-+-" (fmap (`replicate` '-') widths)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose rows = fmap head rows : transpose (fmap tail rows)
