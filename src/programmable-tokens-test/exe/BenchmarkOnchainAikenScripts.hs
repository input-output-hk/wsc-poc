{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Plutarch.Script (Script, deserialiseScript)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Value (assetClass, assetClassValue)
import PlutusLedgerApi.V3 hiding (deserialiseScript)
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins qualified as BI
import ProgrammableTokens.OffChain.AikenProgrammableTokenScripts qualified as Aiken
import ProgrammableTokens.OffChain.Scripts qualified as OffchainScripts
import ProgrammableTokens.Test (productionMaxTxExBudget)
import ProgrammableTokens.Test.ScriptContext.Builder (ScriptContextBuilder, buildBalancedScriptContext, buildScriptContext, mkAdaValue, withAddress, withFee, withInlineDatum, withInput, withMint, withMintingScript, withOutRef, withOutput, withRedeemer, withReferenceInput, withRewardingScript, withScriptInput, withSigner, withTxOutAddress, withTxOutInlineDatum, withTxOutValue, withValue, withWithdrawal)
import SmartTokens.Contracts.ProgrammableLogicBase (TokenProof (TokenDoesNotExist, TokenExists), mkSeizeActRedeemerFromAbsoluteInputIdxs)
import SmartTokens.Core.Scripts (ScriptTarget (Production))
import SmartTokens.LinkedList.MintDirectory (DirectoryNodeAction (InsertDirectoryNode))
import SmartTokens.Types.Constants (issuanceCborHexToken, protocolParamsToken)

main :: IO ()
main = do
    let ExBudget (ExCPU maxCpu) (ExMemory maxMem) = productionMaxTxExBudget
    putStrLn "Onchain Aiken script benchmark (NoTracing, one case per redeemer path)"
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
    , bcPrimaryKind :: EvalKind
    , bcScenarioCtx :: ScriptContext
    }

data BenchRow = BenchRow
    { brName :: String
    , brSuccess :: Bool
    , brBudget :: ExBudget
    , brScriptCount :: Int
    , brPrimaryBudget :: ExBudget
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
    if ok
        then pure (BenchRow (bcName bench) ok totalBudget (length scenarioSpecs) (erBudget primaryResult))
        else do
            putStrLn ("failure logs for " <> bcName bench <> ":")
            mapM_
                ( \(idx, result) ->
                    if erSuccess result
                        then pure ()
                        else putStrLn ("  [" <> show idx <> "] " <> renderEvalKind (erKind result) <> ": " <> erLogText result)
                )
                (zip [0 :: Int ..] results)
            pure (BenchRow (bcName bench) ok totalBudget (length scenarioSpecs) (erBudget primaryResult))

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

data EvalKind
    = EvalBaseSpend TxOutRef
    | EvalGlobalReward Credential
    | EvalDirectoryMint CurrencySymbol
    | EvalDirectorySpend TxOutRef
    | EvalProgrammableMint CurrencySymbol
    | EvalProtocolParamsMint CurrencySymbol
    | EvalIssuanceMint CurrencySymbol
    | EvalAlwaysSucceedsSpend TxOutRef
    | EvalAlwaysSucceedsReward Credential
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
        EvalAlwaysSucceedsSpend outRef -> "alwaysSucceeds spending " <> show outRef
        EvalAlwaysSucceedsReward cred -> "alwaysSucceeds rewarding " <> show cred

compiledAlwaysSucceedsScript :: Script
compiledAlwaysSucceedsScript =
    cardanoApiScriptToScript (OffchainScripts.alwaysSucceedsScript Production)

mkAikenCase :: String -> EvalKind -> C.PlutusScript C.PlutusScriptV3 -> [Data] -> ScriptContext -> BenchCase
mkAikenCase name primaryKind script args ctx =
    BenchCase name (cardanoApiScriptToScript script) args primaryKind ctx

cardanoApiScriptToScript :: C.PlutusScript C.PlutusScriptV3 -> Script
cardanoApiScriptToScript (C.PlutusScriptSerialised serialisedBytes) =
    deserialiseScript serialisedBytes

aikenMintArgs :: ScriptContext -> [Data]
aikenMintArgs ctx = [toData ctx]

aikenRewardArgs :: ScriptContext -> [Data]
aikenRewardArgs ctx = [toData ctx]

aikenSpendArgs :: TxOutRef -> ScriptContext -> [Data]
aikenSpendArgs _ ctx = [toData ctx]

scriptCredentialHash :: Credential -> ScriptHash
scriptCredentialHash (ScriptCredential sh) = sh
scriptCredentialHash cred = error ("expected ScriptCredential, got: " <> show cred)

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
                                (cardanoApiScriptToScript (Aiken.aikenProgrammableLogicBaseScript globalCred))
                                (aikenSpendArgs outRef (spendingPurposeCtx ctx input))
                | sh == scriptCredentialHash txD29BaseScriptCred ->
                    let outRef = txInInfoOutRef input
                     in Just $
                            EvalSpec
                                (EvalBaseSpend outRef)
                                (cardanoApiScriptToScript (Aiken.aikenProgrammableLogicBaseScript txD29GlobalStakeCred))
                                (aikenSpendArgs outRef (spendingPurposeCtx ctx input))
                | sh == ScriptHash (bs28 0x44) ->
                    let outRef = txInInfoOutRef input
                     in Just $
                            EvalSpec
                                (EvalDirectorySpend outRef)
                                (cardanoApiScriptToScript (Aiken.aikenDirectoryNodeSpendingScript protocolParamsCS))
                                (aikenSpendArgs outRef (spendingPurposeCtx ctx input))
                | sh == externalAlwaysSucceedsHash || sh == externalAlwaysSucceedsHash2 ->
                    let outRef = txInInfoOutRef input
                     in Just $
                            EvalSpec
                                (EvalAlwaysSucceedsSpend outRef)
                                compiledAlwaysSucceedsScript
                                [toData (spendingPurposeCtx ctx input)]
            _ -> Nothing

    withdrawalEvalSpec :: Credential -> Maybe EvalSpec
    withdrawalEvalSpec cred
        | cred == globalCred =
            Just $
                EvalSpec
                    (EvalGlobalReward cred)
                    (cardanoApiScriptToScript (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS))
                    (aikenRewardArgs (rewardingPurposeCtx ctx cred))
        | cred == txD29GlobalStakeCred =
            Just $
                EvalSpec
                    (EvalGlobalReward cred)
                    (cardanoApiScriptToScript (Aiken.aikenProgrammableLogicGlobalScript txD29ProtocolParamsCS))
                    (aikenRewardArgs (rewardingPurposeCtx ctx cred))
        | cred == txD29TransferLogicStakeCred =
            Just $
                EvalSpec
                    (EvalAlwaysSucceedsReward cred)
                    compiledAlwaysSucceedsScript
                    [toData (rewardingPurposeCtx ctx cred)]
        | cred `elem` auxiliaryAlwaysSucceedsCreds =
            Just $
                EvalSpec
                    (EvalAlwaysSucceedsReward cred)
                    compiledAlwaysSucceedsScript
                    [toData (rewardingPurposeCtx ctx cred)]
        | otherwise = Nothing

    auxiliaryAlwaysSucceedsCreds =
        [ ScriptCredential transferLogicHash
        , issuerCred
        , ScriptCredential mintingLogicHash
        , ScriptCredential externalAlwaysSucceedsHash
        , ScriptCredential externalAlwaysSucceedsHash2
        ]

    mintEvalSpec :: CurrencySymbol -> Maybe EvalSpec
    mintEvalSpec cs
        | cs == directoryPolicyCS =
            Just $
                EvalSpec
                    (EvalDirectoryMint cs)
                    (cardanoApiScriptToScript (Aiken.aikenDirectoryNodeMintingScript initRef issuanceInitRef))
                    (aikenMintArgs (mintingPurposeCtx ctx cs))
        | cs == mintingPolicyCS =
            Just $
                EvalSpec
                    (EvalProgrammableMint cs)
                    (cardanoApiScriptToScript (Aiken.aikenProgrammableLogicMintingScript progLogicBaseCred (ScriptCredential mintingLogicHash)))
                    (aikenMintArgs (mintingPurposeCtx ctx cs))
        | cs == protocolParamsCS =
            Just $
                EvalSpec
                    (EvalProtocolParamsMint cs)
                    (cardanoApiScriptToScript (Aiken.aikenProtocolParamsMintingScript protocolParamsInitRef))
                    (aikenMintArgs (mintingPurposeCtx ctx cs))
        | cs == issuancePolicyCS =
            Just $
                EvalSpec
                    (EvalIssuanceMint cs)
                    (cardanoApiScriptToScript (Aiken.aikenIssuanceCborHexMintingScript issuanceInitRef))
                    (aikenMintArgs (mintingPurposeCtx ctx cs))
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

aikenConstr :: Integer -> [PlutusTx.Data] -> BuiltinData
aikenConstr tag fields =
    PlutusTx.dataToBuiltinData (PlutusTx.Constr tag fields)

aikenProtocolParamsDatumData :: CurrencySymbol -> Credential -> BuiltinData
aikenProtocolParamsDatumData registryNodeCs progLogicCredential =
    aikenConstr
        0
        [ PlutusTx.toData registryNodeCs
        , PlutusTx.toData progLogicCredential
        ]

aikenIssuanceDatumData :: BuiltinByteString -> BuiltinByteString -> BuiltinData
aikenIssuanceDatumData prefix postfix =
    aikenConstr
        0
        [ PlutusTx.toData prefix
        , PlutusTx.toData postfix
        ]

aikenCurrencySymbolBytes :: CurrencySymbol -> BuiltinByteString
aikenCurrencySymbolBytes (CurrencySymbol bs) = bs

aikenDirectoryNodeDatumData ::
    BuiltinByteString ->
    BuiltinByteString ->
    Credential ->
    Credential ->
    BuiltinByteString ->
    BuiltinData
aikenDirectoryNodeDatumData key next transferLogicScript issuerLogicScript globalStateCs =
    aikenConstr
        0
        [ PlutusTx.toData key
        , PlutusTx.toData next
        , PlutusTx.toData transferLogicScript
        , PlutusTx.toData issuerLogicScript
        , PlutusTx.toData globalStateCs
        ]

aikenTransferProofData :: TokenProof -> PlutusTx.Data
aikenTransferProofData = \case
    TokenExists nodeIdx ->
        PlutusTx.Constr 0 [PlutusTx.I nodeIdx]
    TokenDoesNotExist nodeIdx ->
        PlutusTx.Constr 1 [PlutusTx.I nodeIdx]

aikenTransferActRedeemerData :: [TokenProof] -> BuiltinData
aikenTransferActRedeemerData proofs =
    aikenConstr 0 [PlutusTx.List (fmap aikenTransferProofData proofs)]

aikenMintingActionRedeemerData :: Credential -> BuiltinData
aikenMintingActionRedeemerData mintingLogicCredential =
    aikenConstr 0 [PlutusTx.toData mintingLogicCredential]

policyIdCurrencySymbol :: C.PolicyId -> CurrencySymbol
policyIdCurrencySymbol =
    CurrencySymbol . PV1.toBuiltin . C.serialiseToRawBytes

scriptPolicyCurrencySymbol :: C.PlutusScript C.PlutusScriptV3 -> CurrencySymbol
scriptPolicyCurrencySymbol =
    policyIdCurrencySymbol
        . C.scriptPolicyId
        . C.PlutusScript C.PlutusScriptV3

scriptHashFromCardanoScript :: C.PlutusScript C.PlutusScriptV3 -> ScriptHash
scriptHashFromCardanoScript =
    ScriptHash
        . PV1.toBuiltin
        . C.serialiseToRawBytes
        . C.hashScript
        . C.PlutusScript C.PlutusScriptV3

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

computeRegisteredCs :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> CurrencySymbol
computeRegisteredCs prefix postfix hashedParam =
    CurrencySymbol $
        BI.blake2b_224
            ( versionHeader
                <> prefix
                <> hashedParam
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

tailNodeBs :: BuiltinByteString
tailNodeBs = PV1.toBuiltin (BS.replicate 30 0xff)

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

protocolParamsDatum :: BuiltinData
protocolParamsDatum =
    aikenProtocolParamsDatumData directoryNodeCS progLogicBaseCred

directoryCoveringNode :: BuiltinData
directoryCoveringNode =
    aikenDirectoryNodeDatumData
        ""
        tailNodeBs
        (ScriptCredential transferLogicHash)
        issuerCred
        ""

directoryProgrammableNode :: BuiltinData
directoryProgrammableNode =
    aikenDirectoryNodeDatumData
        (aikenCurrencySymbolBytes programmableTransferCS)
        tailNodeBs
        (ScriptCredential transferLogicHash)
        issuerCred
        ""

directoryProgrammableNode2 :: BuiltinData
directoryProgrammableNode2 =
    aikenDirectoryNodeDatumData
        (aikenCurrencySymbolBytes programmableTransferCS2)
        tailNodeBs
        (ScriptCredential transferLogicHash)
        issuerCred
        ""

directoryProgrammableNode3 :: BuiltinData
directoryProgrammableNode3 =
    aikenDirectoryNodeDatumData
        (aikenCurrencySymbolBytes programmableTransferCS3)
        tailNodeBs
        (ScriptCredential transferLogicHash)
        issuerCred
        ""

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
            (aikenTransferActRedeemerData [TokenExists 1])
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
                protocolParamsDatum
            <> withRefInputDatumValue
                dirNodeRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                directoryProgrammableNode
        )

globalTransferDoesNotExistCtx :: ScriptContext
globalTransferDoesNotExistCtx =
    buildBalancedScriptContext
        ( withRewardingScript
            (aikenTransferActRedeemerData [TokenDoesNotExist 1])
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
                protocolParamsDatum
            <> withRefInputDatumValue
                dirNodeRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                directoryCoveringNode
        )

globalTransferMixedManyCtx :: ScriptContext
globalTransferMixedManyCtx =
    buildBalancedScriptContext
        ( withRewardingScript
            (aikenTransferActRedeemerData [TokenDoesNotExist 1, TokenExists 2, TokenExists 3, TokenExists 4, TokenDoesNotExist 1])
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
                protocolParamsDatum
            <> withRefInputDatumValue
                dirNodeRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                directoryCoveringNode
            <> withRefInputDatumValue
                directoryProgrammableNodeRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                directoryProgrammableNode
            <> withRefInputDatumValue
                directoryProgrammableNode2Ref
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                directoryProgrammableNode2
            <> withRefInputDatumValue
                directoryProgrammableNode3Ref
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                directoryProgrammableNode3
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
                (aikenTransferActRedeemerData [TokenDoesNotExist 1, TokenExists 2])
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
                    protocolParamsDatum
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    directoryCoveringNode
                <> withRefInputDatumValue
                    directoryProgrammableNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    directoryProgrammableNode
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
                <> seizeResidualOutputBuilder seizeInputCount
                <> correspondingOutputsBuilder
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    protocolParamsDatum
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    directoryProgrammableNode
            )

globalSeize1Ctx :: ScriptContext
globalSeize1Ctx = mkGlobalSeizeCtx 1

globalSeize5Ctx :: ScriptContext
globalSeize5Ctx = mkGlobalSeizeCtx 5

globalSeize10Ctx :: ScriptContext
globalSeize10Ctx = mkGlobalSeizeCtx 10

globalSeize20Ctx :: ScriptContext
globalSeize20Ctx = mkGlobalSeizeCtx 20

mkGlobalSeizeExternalScriptAndManyPubKeyCtx :: Integer -> ScriptContext
mkGlobalSeizeExternalScriptAndManyPubKeyCtx pubKeyInputCount =
    let pubKeyInputIdxs = [0 .. (pubKeyInputCount - 1)]
        seizeRedeemer =
            mkSeizeActRedeemerFromAbsoluteInputIdxs
                1
                -- The Aiken ThirdPartyAct validator only allows indexed inputs to
                -- be programmable inputs. The external script spend is present in
                -- the transaction, but it is not listed in input_idxs.
                [pubKeyInputCount]
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
                    protocolParamsDatum
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    directoryProgrammableNode
            )

globalSeize1ExternalScript50PubKeyCtx :: ScriptContext
globalSeize1ExternalScript50PubKeyCtx =
    mkGlobalSeizeExternalScriptAndManyPubKeyCtx manyPubKeyInputCount

directoryInitCtx :: ScriptContext
directoryInitCtx =
    let mintValue = mkValue [(directoryPolicyCS, TokenName "", 1)]
        emptyNodeDatum =
            aikenDirectoryNodeDatumData
                ""
                tailNodeBs
                (PubKeyCredential (PubKeyHash ""))
                (PubKeyCredential (PubKeyHash ""))
                ""
     in buildScriptContext
            ( withRedeemer (aikenConstr 0 [])
                <> withMintingScript mintValue (aikenConstr 0 [])
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

programmableMintCtx :: ScriptContext
programmableMintCtx =
    let scriptRedeemer = aikenMintingActionRedeemerData (ScriptCredential mintingLogicHash)
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
    let scriptRedeemer = aikenMintingActionRedeemerData (ScriptCredential mintingLogicHash)
        burnValue = mkValue [(mintingPolicyCS, TokenName "0c", -1)]
     in buildBalancedScriptContext
            ( withRedeemer scriptRedeemer
                <> withMintingScript burnValue scriptRedeemer
                <> withWithdrawal (ScriptCredential mintingLogicHash) 0
            )

aikenIssuanceDatum :: BuiltinData
aikenIssuanceDatum = aikenIssuanceDatumData "0d" "0e"

aikenProtocolParamsAlwaysFailHash :: ScriptHash
aikenProtocolParamsAlwaysFailHash =
    scriptHashFromCardanoScript Aiken.aikenProtocolParamsSpendingScript

aikenIssuanceAlwaysFailHash :: ScriptHash
aikenIssuanceAlwaysFailHash =
    scriptHashFromCardanoScript Aiken.aikenIssuanceCborHexSpendingScript

aikenIssuancePolicyCS :: CurrencySymbol
aikenIssuancePolicyCS =
    scriptPolicyCurrencySymbol (Aiken.aikenIssuanceCborHexMintingScript issuanceInitRef)

aikenProtocolParamsMintCtx :: ScriptContext
aikenProtocolParamsMintCtx =
    let mintValue = mkValue [(protocolParamsCS, protocolParamsToken, 1)]
     in buildBalancedScriptContext
            ( withMintingScript mintValue (PlutusTx.toBuiltinData ())
                <> withInput
                    ( withOutRef protocolParamsInitRef
                        <> withAddress (pubKeyAddress signerPkh)
                        <> withValue (mkAdaValue 2_000_000)
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddress aikenProtocolParamsAlwaysFailHash)
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue)
                        <> withTxOutInlineDatum protocolParamsDatum
                    )
            )

aikenIssuanceMintCtx :: ScriptContext
aikenIssuanceMintCtx =
    let mintValue = mkValue [(issuancePolicyCS, issuanceCborHexToken, 1)]
     in buildBalancedScriptContext
            ( withMintingScript mintValue (PlutusTx.toBuiltinData ())
                <> withInput
                    ( withOutRef issuanceInitRef
                        <> withAddress (pubKeyAddress signerPkh)
                        <> withValue (mkAdaValue 2_000_000)
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddress aikenIssuanceAlwaysFailHash)
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue)
                        <> withTxOutInlineDatum aikenIssuanceDatum
                    )
            )

aikenDirectoryInsertCtx :: ScriptContext
aikenDirectoryInsertCtx =
    let issuancePrefix = "0d"
        issuancePostfix = "0e"
        hashedMintingParam = bs28 0x55
        insertProtocolParamsDatum =
            aikenProtocolParamsDatumData directoryPolicyCS progLogicBaseCred
        insertedCs = computeRegisteredCs issuancePrefix issuancePostfix hashedMintingParam
        insertedCsBs = case insertedCs of
            CurrencySymbol bs -> bs
        insertedToken = TokenName insertedCsBs
        insertRedeemer = InsertDirectoryNode insertedCs (ScriptHash hashedMintingParam)
        nodeMintValue = mkValue [(directoryPolicyCS, insertedToken, 1)]
        registeredAssetMintValue = mkValue [(insertedCs, TokenName "0b", 1)]
        coveringNode =
            aikenDirectoryNodeDatumData
                ""
                tailNodeBs
                (ScriptCredential transferLogicHash)
                issuerCred
                ""
        coveringOutput =
            aikenDirectoryNodeDatumData
                ""
                (aikenCurrencySymbolBytes insertedCs)
                (ScriptCredential transferLogicHash)
                issuerCred
                ""
        insertedNode =
            aikenDirectoryNodeDatumData
                (aikenCurrencySymbolBytes insertedCs)
                tailNodeBs
                (ScriptCredential transferLogicHash)
                issuerCred
                ""
     in buildScriptContext
            ( withRedeemer (PlutusTx.toBuiltinData insertRedeemer)
                <> withMintingScript nodeMintValue (PlutusTx.toBuiltinData insertRedeemer)
                <> withMint registeredAssetMintValue (PlutusTx.toBuiltinData ())
                <> withScriptInput
                    (PlutusTx.toBuiltinData ())
                    ( withOutRef insertNodeInRef
                        <> withAddress (scriptAddress (ScriptHash (bs28 0x44)))
                        <> withValue (mkAdaValue 2_000_000 <> mkValue [(directoryPolicyCS, TokenName "", 1)])
                        <> withInlineDatum coveringNode
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddress (ScriptHash (bs28 0x44)))
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mkValue [(directoryPolicyCS, TokenName "", 1)])
                        <> withTxOutInlineDatum coveringOutput
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddress (ScriptHash (bs28 0x44)))
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mkValue [(directoryPolicyCS, insertedToken, 1)])
                        <> withTxOutInlineDatum insertedNode
                    )
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    insertProtocolParamsDatum
                <> withRefInputDatumValue
                    issuanceRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 2_000_000 <> mkValue [(aikenIssuancePolicyCS, issuanceCborHexToken, 1)])
                    aikenIssuanceDatum
            )

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
                        (aikenTransferActRedeemerData [TokenExists 2, TokenDoesNotExist 1])
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
                        protocolParamsDatum
                    <> withRefInputDatumValue
                        dirNodeRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                        directoryCoveringNode
                    <> withRefInputDatumValue
                        directoryProgrammableNodeRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                        directoryProgrammableNode
                )

mainnetDexBaseSpendingCtx :: ScriptContext
mainnetDexBaseSpendingCtx =
    let ScriptContext txInfo _ _ = mainnetDexGlobalTransferCtx
     in ScriptContext
            txInfo
            (Redeemer (PlutusTx.toBuiltinData ()))
            (SpendingScript mainnetDexBaseInputRef Nothing)

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
    aikenTransferActRedeemerData [TokenExists 2]

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
    aikenProtocolParamsDatumData
        (currencySymbolHex "f1f838a525637791ca06d1fd415358a27ba829024ee0b90af5e32f14")
        txD29BaseScriptCred

txD29RefDatum1Data :: BuiltinData
txD29RefDatum1Data =
    aikenIssuanceDatumData
        (hexToBuiltin "338ef18819c855fc8970be65827abb86089b58fad24b31cff332ce97")
        (hexToBuiltin "87f2531e16923b81a0f3f507363db6b7cebf30735cb76b94543ca7ac")

txD29DirectoryNodeDatumData :: BuiltinData
txD29DirectoryNodeDatumData =
    aikenConstr
        0
        [ PlutusTx.B (hexToBytes "b34a184f1f2871aa4d33544caecefef5242025f45c3fa5213d7662a9")
        , PlutusTx.B (hexToBytes "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
        , PlutusTx.toData txD29TransferLogicStakeCred
        , PlutusTx.toData (ScriptCredential (scriptHashHex "b427cc9d5b829bbf66a784066fa3c03684fee2bfac7b571654ae8f55"))
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
    [ mkAikenCase
        "programmableLogicBase"
        (EvalBaseSpend progInputRef)
        (Aiken.aikenProgrammableLogicBaseScript globalCred)
        (aikenSpendArgs progInputRef baseSpendingCtx)
        baseSpendingCtx
    , mkAikenCase
        "programmableLogicGlobal.TransferAct"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalTransferCtx)
        globalTransferCtx
    , mkAikenCase
        "programmableLogicGlobal.TransferAct.TokenDoesNotExist"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalTransferDoesNotExistCtx)
        globalTransferDoesNotExistCtx
    , mkAikenCase
        "programmableLogicGlobal.TransferAct.MixedMany"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalTransferMixedManyCtx)
        globalTransferMixedManyCtx
    , mkAikenCase
        "programmableLogicGlobal.TransferAct.Spend5Utxos"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalTransfer5Ctx)
        globalTransfer5Ctx
    , mkAikenCase
        "programmableLogicGlobal.TransferAct.Spend10Utxos"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalTransfer10Ctx)
        globalTransfer10Ctx
    , mkAikenCase
        "programmableLogicGlobal.TransferAct.Spend15Utxos"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalTransfer15Ctx)
        globalTransfer15Ctx
    , mkAikenCase
        "programmableLogicGlobal.SeizeAct1"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalSeize1Ctx)
        globalSeize1Ctx
    , mkAikenCase
        "programmableLogicGlobal.SeizeAct1.ExternalScriptAnd50PubKeyInputs"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalSeize1ExternalScript50PubKeyCtx)
        globalSeize1ExternalScript50PubKeyCtx
    , mkAikenCase
        "programmableLogicGlobal.SeizeAct5"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalSeize5Ctx)
        globalSeize5Ctx
    , mkAikenCase
        "programmableLogicGlobal.SeizeAct10"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalSeize10Ctx)
        globalSeize10Ctx
    , mkAikenCase
        "programmableLogicGlobal.SeizeAct20"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalSeize20Ctx)
        globalSeize20Ctx
    , mkAikenCase
        "directoryNodeMinting.InitDirectory"
        (EvalDirectoryMint directoryPolicyCS)
        (Aiken.aikenDirectoryNodeMintingScript initRef issuanceInitRef)
        (aikenMintArgs directoryInitCtx)
        directoryInitCtx
    , mkAikenCase
        "directoryNodeMinting.InsertDirectoryNode"
        (EvalDirectoryMint directoryPolicyCS)
        (Aiken.aikenDirectoryNodeMintingScript initRef issuanceInitRef)
        (aikenMintArgs aikenDirectoryInsertCtx)
        aikenDirectoryInsertCtx
    , mkAikenCase
        "programmableLogicMinting.Mint"
        (EvalProgrammableMint mintingPolicyCS)
        (Aiken.aikenProgrammableLogicMintingScript progLogicBaseCred (ScriptCredential mintingLogicHash))
        (aikenMintArgs programmableMintCtx)
        programmableMintCtx
    , mkAikenCase
        "programmableLogicMinting.Burn"
        (EvalProgrammableMint mintingPolicyCS)
        (Aiken.aikenProgrammableLogicMintingScript progLogicBaseCred (ScriptCredential mintingLogicHash))
        (aikenMintArgs programmableBurnCtx)
        programmableBurnCtx
    , mkAikenCase
        "protocolParamsMinting"
        (EvalProtocolParamsMint protocolParamsCS)
        (Aiken.aikenProtocolParamsMintingScript protocolParamsInitRef)
        (aikenMintArgs aikenProtocolParamsMintCtx)
        aikenProtocolParamsMintCtx
    , mkAikenCase
        "issuanceCborHexMinting"
        (EvalIssuanceMint issuancePolicyCS)
        (Aiken.aikenIssuanceCborHexMintingScript issuanceInitRef)
        (aikenMintArgs aikenIssuanceMintCtx)
        aikenIssuanceMintCtx
    , mkAikenCase
        ("programmableLogicBase.Tx." <> take 8 mainnetDexTxHash <> ".Spending")
        (EvalBaseSpend mainnetDexBaseInputRef)
        (Aiken.aikenProgrammableLogicBaseScript globalCred)
        (aikenSpendArgs mainnetDexBaseInputRef mainnetDexBaseSpendingCtx)
        mainnetDexBaseSpendingCtx
    , mkAikenCase
        ("programmableLogicGlobal.TransferAct.Tx." <> take 8 mainnetDexTxHash <> ".NightAdaDex16Swaps")
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs mainnetDexGlobalTransferCtx)
        mainnetDexGlobalTransferCtx
    , mkAikenCase
        ("programmableLogicBase.Tx." <> take 8 txD29Hash <> ".Spending")
        (EvalBaseSpend txD29ScriptInputRef)
        (Aiken.aikenProgrammableLogicBaseScript txD29GlobalStakeCred)
        (aikenSpendArgs txD29ScriptInputRef txD29ProgrammableLogicBaseSpendingCtx)
        txD29ProgrammableLogicBaseSpendingCtx
    , mkAikenCase
        ("programmableLogicBase.Tx." <> take 8 txD29Hash <> ".Stake")
        (EvalGlobalReward txD29GlobalStakeCred)
        (Aiken.aikenProgrammableLogicGlobalScript txD29ProtocolParamsCS)
        (aikenRewardArgs txD29ProgrammableLogicBaseStakeCtx)
        txD29ProgrammableLogicBaseStakeCtx
    ]
