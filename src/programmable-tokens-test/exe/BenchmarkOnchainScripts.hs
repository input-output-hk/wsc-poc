{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Either (isRight)
import Data.List (intercalate, sortOn)
import Data.Word (Word8)
import Numeric (showFFloat)
import Plutarch.Evaluate (applyArguments, evalScript)
import Plutarch.Internal.Term (
    Config (NoTracing),
    Script,
    Term,
    compile,
 )
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Value (assetClass, assetClassValue)
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins qualified as BI
import ProgrammableTokens.Test (productionMaxTxExBudget)
import ProgrammableTokens.Test.ScriptContext.Builder (
    ScriptContextBuilder,
    buildBalancedScriptContext,
    buildScriptContext,
    mkAdaValue,
    withAddress,
    withFee,
    withInlineDatum,
    withInput,
    withMint,
    withMintingScript,
    withOutRef,
    withOutput,
    withRedeemer,
    withReferenceInput,
    withRewardingScript,
    withScriptInput,
    withSigner,
    withTxOutAddress,
    withTxOutInlineDatum,
    withTxOutValue,
    withValue,
    withWithdrawal,
 )
import SmartTokens.Contracts.Issuance (mkProgrammableLogicMinting)
import SmartTokens.Contracts.IssuanceCborHex (
    IssuanceCborHex (IssuanceCborHex),
    mkIssuanceCborHexMinting,
 )
import SmartTokens.Contracts.ProgrammableLogicBase (
    ProgrammableLogicGlobalRedeemer (TransferAct),
    TokenProof (TokenDoesNotExist, TokenExists),
    mkProgrammableLogicBase,
    mkProgrammableLogicGlobal,
    mkSeizeActRedeemerFromAbsoluteInputIdxs,
 )
import SmartTokens.Contracts.ProtocolParams (mkProtocolParametersMinting)
import SmartTokens.LinkedList.MintDirectory (
    DirectoryNodeAction (InitDirectory, InsertDirectoryNode),
    mkDirectoryNodeMP,
 )
import SmartTokens.Types.Constants (issuanceCborHexToken, protocolParamsToken)
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (DirectorySetNode))
import SmartTokens.Types.ProtocolParams (
    ProgrammableLogicGlobalParams (ProgrammableLogicGlobalParams),
 )

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
    mapM_ putStrLn (renderBenchTable sorted)
  where
    caseName BenchRow{brName = name} = name

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

runCase :: BenchCase -> IO BenchRow
runCase bench = do
    let (res, budget, logs) = evalScript (applyArguments (bcScript bench) (bcArgs bench))
        ok = isRight res
    if ok
        then pure (BenchRow (bcName bench) ok budget)
        else do
            putStrLn ("failure logs for " <> bcName bench <> ": " <> show logs)
            pure (BenchRow (bcName bench) ok budget)

renderBenchTable :: [BenchRow] -> [String]
renderBenchTable rows =
    let header =
            [ "Case"
            , "Result"
            , "CPU"
            , "CPU %"
            , "Mem"
            , "Mem %"
            ]
        renderedRows = fmap renderBenchRow rows
        allRows = header : renderedRows
        widths = fmap maximumColumnWidth (transpose allRows)
        separator = renderSeparator widths
     in renderColumns widths header
            : separator
            : fmap (renderColumns widths) renderedRows

renderBenchRow :: BenchRow -> [String]
renderBenchRow BenchRow{brName = name, brSuccess = success, brBudget = budget} =
    [ name
    , if success then "PASS" else "FAIL"
    , budgetCpuText budget
    , formatPercent (budgetCpuPct budget)
    , budgetMemText budget
    , formatPercent (budgetMemPct budget)
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

compileNoTracing :: (forall s. Term s a) -> Script
compileNoTracing t =
    either (error . ("compile failed: " <>) . show) id (compile NoTracing t)

mkCase :: String -> (forall s. Term s a) -> [Data] -> BenchCase
mkCase name term args = BenchCase name (compileNoTracing term) args

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

tailCS :: CurrencySymbol
tailCS = CurrencySymbol maxBs28

baseInputRef :: TxOutRef
baseInputRef = TxOutRef "ba5e" 0

progInputRef :: TxOutRef
progInputRef = TxOutRef "f00d" 0

paramRef :: TxOutRef
paramRef = TxOutRef "aa00" 0

dirNodeRef :: TxOutRef
dirNodeRef = TxOutRef "bb00" 0

initRef :: TxOutRef
initRef = TxOutRef "1a1a" 0

insertNodeInRef :: TxOutRef
insertNodeInRef = TxOutRef "c0de" 0

issuanceRef :: TxOutRef
issuanceRef = TxOutRef "c0fe" 0

protocolParamsInitRef :: TxOutRef
protocolParamsInitRef = TxOutRef "aa11" 0

issuanceInitRef :: TxOutRef
issuanceInitRef = TxOutRef "bb11" 0

protocolParamsDatum :: ProgrammableLogicGlobalParams
protocolParamsDatum =
    ProgrammableLogicGlobalParams directoryNodeCS progLogicBaseCred

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
        ( withOutRef (TxOutRef "5e12" idx)
            <> withAddress seizeInputAddr
            <> withValue seizeInputValue
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
    buildBalancedScriptContext
        ( withScriptInput
            (PlutusTx.toBuiltinData ())
            ( withOutRef baseInputRef
                <> withAddress (scriptAddress progLogicBaseHash)
                <> withValue (mkAdaValue 3_000_000)
            )
            <> withWithdrawal globalCred 0
        )

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
                (TxOutRef "bb00" 0)
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                (PlutusTx.toBuiltinData directoryCoveringNode)
            <> withRefInputDatumValue
                (TxOutRef "bb10" 0)
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                (PlutusTx.toBuiltinData directoryProgrammableNode)
            <> withRefInputDatumValue
                (TxOutRef "bb11" 0)
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                (PlutusTx.toBuiltinData directoryProgrammableNode2)
            <> withRefInputDatumValue
                (TxOutRef "bb12" 0)
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
        ( withOutRef (TxOutRef "fa11" idx)
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
                    (TxOutRef "bb10" 0)
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
                    ( withTxOutAddress (Address progLogicBaseCred Nothing)
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
            )

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
    [ mkCase "programmableLogicBase" mkProgrammableLogicBase [toData globalCred, toData baseSpendingCtx]
    , mkCase "programmableLogicGlobal.TransferAct" mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransferCtx]
    , mkCase "programmableLogicGlobal.TransferAct.TokenDoesNotExist" mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransferDoesNotExistCtx]
    , mkCase "programmableLogicGlobal.TransferAct.MixedMany" mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransferMixedManyCtx]
    , mkCase "programmableLogicGlobal.TransferAct.Spend5Utxos" mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransfer5Ctx]
    , mkCase "programmableLogicGlobal.TransferAct.Spend10Utxos" mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransfer10Ctx]
    , mkCase "programmableLogicGlobal.TransferAct.Spend15Utxos" mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransfer15Ctx]
    , mkCase "programmableLogicGlobal.SeizeAct1" mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalSeize1Ctx]
    , mkCase "programmableLogicGlobal.SeizeAct5" mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalSeize5Ctx]
    , mkCase "programmableLogicGlobal.SeizeAct10" mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalSeize10Ctx]
    , mkCase "programmableLogicGlobal.SeizeAct20" mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalSeize20Ctx]
    , mkCase "directoryNodeMinting.InitDirectory" mkDirectoryNodeMP [toData initRef, toData issuancePolicyCS, toData directoryInitCtx]
    , mkCase "directoryNodeMinting.InsertDirectoryNode" mkDirectoryNodeMP [toData initRef, toData issuancePolicyCS, toData directoryInsertCtx]
    , mkCase "programmableLogicMinting.Mint" mkProgrammableLogicMinting [toData progLogicBaseCred, toData mintingLogicHash, toData programmableMintCtx]
    , mkCase "programmableLogicMinting.Burn" mkProgrammableLogicMinting [toData progLogicBaseCred, toData mintingLogicHash, toData programmableBurnCtx]
    , mkCase "protocolParamsMinting" mkProtocolParametersMinting [toData protocolParamsInitRef, toData protocolParamsMintCtx]
    , mkCase "issuanceCborHexMinting" mkIssuanceCborHexMinting [toData issuanceInitRef, toData issuanceMintCtx]
    , mkCase
        ("programmableLogicBase.Tx." <> take 8 txD29Hash <> ".Spending")
        mkProgrammableLogicBase
        [ toData txD29GlobalStakeCred
        , toData txD29ProgrammableLogicBaseSpendingCtx
        ]
    , mkCase
        ("programmableLogicBase.Tx." <> take 8 txD29Hash <> ".Stake")
        mkProgrammableLogicGlobal
        [ toData txD29ProtocolParamsCS
        , toData txD29ProgrammableLogicBaseStakeCtx
        ]
    ]
