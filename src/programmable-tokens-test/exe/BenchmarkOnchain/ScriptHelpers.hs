module BenchmarkOnchain.ScriptHelpers (
    assetUnitHex,
    bs28,
    currencySymbolHex,
    hexToBuiltin,
    hexToBytes,
    lookupRedeemerData,
    mintingPurposeCtx,
    mkValue,
    pubKeyAddress,
    pubKeyHashHex,
    rewardingPurposeCtx,
    stripZeroChangeOutput,
    scriptAddress,
    scriptAddressWithSignerStake,
    scriptAddressWithStakeCredential,
    scriptCredentialHash,
    scriptHashHex,
    spendingPurposeCtx,
    tokenNameHex,
    txId32,
    txIdHex,
    txOutRef32,
    withAuxiliaryRewardingScript,
    withPubKeyInputValue,
    withRefInputDatumValue,
) where

import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Word (Word8)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Value (assetClass, assetClassValue)
import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import ProgrammableTokens.Test.ScriptContext.Builder (ScriptContextBuilder, mkAdaValue, withAddress, withInlineDatum, withInput, withOutRef, withReferenceInput, withRewardingScriptWitness, withValue)

assetUnitHex :: String -> Integer -> (CurrencySymbol, TokenName, Integer)
assetUnitHex unit quantity =
    let policyId = take 56 unit
        tokenName = drop 56 unit
     in (currencySymbolHex policyId, tokenNameHex tokenName, quantity)

bs28 :: Word8 -> BuiltinByteString
bs28 w = PV1.toBuiltin (BS.replicate 28 w)

currencySymbolHex :: String -> CurrencySymbol
currencySymbolHex = CurrencySymbol . hexToBuiltin

hexToBuiltin :: String -> BuiltinByteString
hexToBuiltin = PV1.toBuiltin . hexToBytes

hexToBytes :: String -> BS.ByteString
hexToBytes = BS.pack . go
  where
    go [] = []
    go [_] = error "hexToBuiltin: odd-length hex string"
    go (a : b : rest) =
        fromIntegral (hexNibble a * 16 + hexNibble b) : go rest

    hexNibble c
        | c >= '0' && c <= '9' = ord c - ord '0'
        | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
        | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
        | otherwise = error ("hexToBuiltin: invalid hex char: " <> [c])

lookupRedeemerData :: ScriptPurpose -> TxInfo -> BuiltinData
lookupRedeemerData purpose txInfo =
    case Map.lookup purpose (txInfoRedeemers txInfo) of
        Just (Redeemer dat) -> dat
        Nothing -> PlutusTx.toBuiltinData ()

mintingPurposeCtx :: ScriptContext -> CurrencySymbol -> ScriptContext
mintingPurposeCtx (ScriptContext txInfo _ _) cs =
    ScriptContext
        txInfo
        (Redeemer (lookupRedeemerData (Minting cs) txInfo))
        (MintingScript cs)

mkValue :: [(CurrencySymbol, TokenName, Integer)] -> Value
mkValue = foldMap (\(cs, tn, amount) -> assetClassValue (assetClass cs tn) amount)

pubKeyAddress :: PubKeyHash -> Address
pubKeyAddress pkh = Address (PubKeyCredential pkh) Nothing

pubKeyHashHex :: String -> PubKeyHash
pubKeyHashHex = PubKeyHash . hexToBuiltin

rewardingPurposeCtx :: ScriptContext -> Credential -> ScriptContext
rewardingPurposeCtx (ScriptContext txInfo _ _) cred =
    ScriptContext
        txInfo
        (Redeemer (lookupRedeemerData (Rewarding cred) txInfo))
        (RewardingScript cred)

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

scriptAddress :: ScriptHash -> Address
scriptAddress sh = Address (ScriptCredential sh) Nothing

scriptAddressWithSignerStake :: ScriptHash -> PubKeyHash -> Address
scriptAddressWithSignerStake sh pkh =
    Address (ScriptCredential sh) (Just (StakingHash (PubKeyCredential pkh)))

scriptAddressWithStakeCredential :: ScriptHash -> Credential -> Address
scriptAddressWithStakeCredential sh cred =
    Address (ScriptCredential sh) (Just (StakingHash cred))

scriptCredentialHash :: Credential -> ScriptHash
scriptCredentialHash (ScriptCredential sh) = sh
scriptCredentialHash cred = error ("expected ScriptCredential, got: " <> show cred)

scriptHashHex :: String -> ScriptHash
scriptHashHex = ScriptHash . hexToBuiltin

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

tokenNameHex :: String -> TokenName
tokenNameHex = TokenName . hexToBuiltin

txId32 :: Word8 -> Word8 -> TxId
txId32 hi lo =
    TxId (PV1.toBuiltin (BS.replicate 31 hi <> BS.singleton lo))

txIdHex :: String -> TxId
txIdHex = TxId . hexToBuiltin

txOutRef32 :: Word8 -> Word8 -> Integer -> TxOutRef
txOutRef32 hi lo idx = TxOutRef (txId32 hi lo) idx

withAuxiliaryRewardingScript :: Credential -> BuiltinData -> ScriptContextBuilder
withAuxiliaryRewardingScript cred redeemer =
    withRewardingScriptWitness redeemer cred 0

withPubKeyInputValue :: PubKeyHash -> TxOutRef -> Int -> ScriptContextBuilder
withPubKeyInputValue signerPkh ref lovelace =
    withInput
        ( withOutRef ref
            <> withAddress (pubKeyAddress signerPkh)
            <> withValue (mkAdaValue lovelace)
        )

withRefInputDatumValue :: TxOutRef -> Address -> Value -> BuiltinData -> ScriptContextBuilder
withRefInputDatumValue ref addr value dat =
    withReferenceInput
        ( withOutRef ref
            <> withAddress addr
            <> withValue value
            <> withInlineDatum dat
        )
