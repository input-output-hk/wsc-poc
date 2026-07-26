-- | Bridge between @cardano-api@ script values and the Plutus identities the
-- benchmark fixtures are written in.
--
-- Every function here goes through @cardano-api@'s own hashing
-- ('C.hashScript' / 'C.scriptPolicyId'), i.e. the exact blake2b-224 a node
-- computes over the serialised, PARAMETER-APPLIED script. Nothing in this
-- module invents a hash; the fixture ids are derived by feeding the offchain
-- deployment builders their real parameters and hashing the result.
module BenchmarkOnchain.CardanoScriptHelpers (
    assertHash28,
    cardanoScriptHash,
    cardanoStakeCredential,
    cardanoTxIn,
    policyIdCurrencySymbol,
    scriptCurrencySymbol,
    scriptHashFromCardanoScript,
) where

import Cardano.Api qualified as C
import Convex.PlutusLedger.V3 (unTransTxOutRef)
import Data.ByteString qualified as BS
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V3 (BuiltinByteString, CurrencySymbol (CurrencySymbol), ScriptHash (ScriptHash), TxOutRef)

-- | The script hash (28 bytes) of an already parameter-applied Plutus V3 script.
scriptHashFromCardanoScript :: C.PlutusScript C.PlutusScriptV3 -> ScriptHash
scriptHashFromCardanoScript =
    ScriptHash
        . assertHash28 "script hash"
        . PV1.toBuiltin
        . C.serialiseToRawBytes
        . C.hashScript
        . C.PlutusScript C.PlutusScriptV3

-- | The currency symbol of a minting policy id.
policyIdCurrencySymbol :: C.PolicyId -> CurrencySymbol
policyIdCurrencySymbol =
    CurrencySymbol . assertHash28 "policy id" . PV1.toBuiltin . C.serialiseToRawBytes

-- | The policy id (as a 'CurrencySymbol') of an already parameter-applied
-- Plutus V3 minting script.
scriptCurrencySymbol :: C.PlutusScript C.PlutusScriptV3 -> CurrencySymbol
scriptCurrencySymbol =
    policyIdCurrencySymbol . C.scriptPolicyId . C.PlutusScript C.PlutusScriptV3

-- | Plutus 'ScriptHash' -> @cardano-api@ 'C.ScriptHash', for feeding a derived
-- credential back into a script builder as a parameter.
cardanoScriptHash :: ScriptHash -> C.ScriptHash
cardanoScriptHash (ScriptHash bs) =
    either
        (error . ("cardanoScriptHash: " <>) . show)
        id
        (C.deserialiseFromRawBytes C.AsScriptHash (PV1.fromBuiltin bs))

-- | The stake credential a script-hash parameter is passed as (the offchain
-- deployment builders take 'C.StakeCredential', not a bare hash).
cardanoStakeCredential :: ScriptHash -> C.StakeCredential
cardanoStakeCredential = C.StakeCredentialByScript . cardanoScriptHash

-- | Plutus 'TxOutRef' -> @cardano-api@ 'C.TxIn', for the one-shot mint
-- parameters of the protocol-params / issuance / directory policies.
cardanoTxIn :: TxOutRef -> C.TxIn
cardanoTxIn ref =
    either (error . ("cardanoTxIn: " <>) . show) id (unTransTxOutRef ref)

-- | Ledger validity gate: every policy id and script credential a fixture uses
-- must be exactly 28 bytes. Applied at every derivation site so a malformed id
-- can never reach a fixture.
assertHash28 :: String -> BuiltinByteString -> BuiltinByteString
assertHash28 label bs
    | byteLength == 28 = bs
    | otherwise =
        error
            ( label
                <> ": expected a 28-byte hash, got "
                <> show byteLength
                <> " bytes"
            )
  where
    byteLength = BS.length (PV1.fromBuiltin bs)
