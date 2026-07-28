module BenchmarkOnchain.ScriptHelpers (
    assetUnitHex,
    bs2,
    bs28,
    bytesToHex,
    currencySymbolHex,
    hexToBuiltin,
    hexToBytes,
    inCurrencySymbolOrder,
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
    verifyAndMaybeDumpIds,
    withAuxiliaryRewardingScript,
    withPubKeyInputValue,
    withRefInputDatumValue,
    withdrawalIndexOf,
) where

import Data.ByteString qualified as BS
import Data.Char (intToDigit, ord)
import Data.List (elemIndex, nub, sortBy, sortOn)
import Data.Word (Word8)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Value (assetClass, assetClassValue)
import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import ProgrammableTokens.Test.ScriptContext.Builder (ScriptContextBuilder, compareCredentialLedger, mkAdaValue, withAddress, withInlineDatum, withInput, withOutRef, withReferenceInput, withRewardingScriptWitness, withValue)
import System.Environment (lookupEnv)

assetUnitHex :: String -> Integer -> (CurrencySymbol, TokenName, Integer)
assetUnitHex unit quantity =
    let policyId = take 56 unit
        tokenName = drop 56 unit
     in (currencySymbolHex policyId, tokenNameHex tokenName, quantity)

bs28 :: Word8 -> BuiltinByteString
bs28 w = PV1.toBuiltin (BS.replicate 28 w)

-- | Two-byte bytestring; byte-wise lexicographic order equals numeric order of
-- the (hi, lo) pair, which keeps generated token names canonically sorted.
bs2 :: Word8 -> Word8 -> BuiltinByteString
bs2 hi lo = PV1.toBuiltin (BS.pack [hi, lo])

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

{- | Position of @target@ in a withdrawal map holding exactly @creds@.

Redeemers that carry a withdrawal index (@TransferAct@'s per-proof
transfer-logic index, @SeizeAct@'s issuer index, the issuance policy's
minting-logic index) are only correct relative to the LEDGER'S OWN credential
order — script credentials first, ascending by hash, then key credentials
('compareCredentialLedger'). Those indices must therefore be computed from the
credentials, never written as literals: derived script hashes change the order,
and a stale literal silently points a proof at the wrong script.
-}
withdrawalIndexOf :: [Credential] -> Credential -> Integer
withdrawalIndexOf creds target =
    case elemIndex target ordered of
        Just idx -> fromIntegral idx
        Nothing -> error ("withdrawalIndexOf: credential not in the withdrawal set: " <> show target)
  where
    ordered = sortBy compareCredentialLedger (nub creds)

{- | Reorder per-policy witnesses (transfer proofs) into canonical
(lexicographic) currency-symbol order.

The transfer validators walk the aggregated programmable input value, which the
ledger presents in canonical currency-symbol order, and consume one positional
proof per entry. Once policy ids are real hashes their relative order is
whatever blake2b-224 says it is, so a proof list can no longer be written down
in source order — it has to be keyed by the symbol it covers and sorted here.
-}
inCurrencySymbolOrder :: [(CurrencySymbol, a)] -> [a]
inCurrencySymbolOrder = fmap snd . sortOn fst

bytesToHex :: BS.ByteString -> String
bytesToHex = concatMap byteHex . BS.unpack
  where
    byteHex w = [intToDigit (fromIntegral w `div` 16), intToDigit (fromIntegral w `mod` 16)]

{- | Ledger-validity gate for a harness's derived deployment ids: every one must
be exactly 28 bytes. With @BENCH_DUMP_IDS@ set the ids are printed as
@ID <label> <hex>@ and the caller should skip the benchmark run (indicated by a
'True' result).
-}
verifyAndMaybeDumpIds :: [(String, BuiltinByteString)] -> IO Bool
verifyAndMaybeDumpIds ids = do
    mapM_ checkOne ids
    dumpRequested <- lookupEnv "BENCH_DUMP_IDS"
    case dumpRequested of
        Nothing -> pure False
        Just _ -> do
            mapM_ (\(label, bs) -> putStrLn ("ID\t" <> label <> "\t" <> bytesToHex (PV1.fromBuiltin bs))) ids
            pure True
  where
    checkOne (label, bs) =
        let len = BS.length (PV1.fromBuiltin bs)
         in if len == 28
                then pure ()
                else error (label <> ": expected a 28-byte id, got " <> show len <> " bytes")
