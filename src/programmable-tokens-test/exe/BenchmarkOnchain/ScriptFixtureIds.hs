-- | Fixture identities that are the SAME on both harnesses.
--
-- Two kinds of thing live here:
--
--   1. Transaction coordinates — 'TxOutRef's, 'TxId's, pubkey hashes, scenario
--      sizes. These are seed data, not hashes of anything, so both harnesses
--      share them verbatim; that is what makes the two sides build the same
--      transaction.
--
--   2. SYNTHETIC-BUT-VALID hashes ('syntheticScriptHash' /
--      'syntheticCurrencySymbol'). Some fixtures need an identity for something
--      this repository does not compile: a token issuer's transfer/minting
--      logic script, an unrelated third-party asset used as noise, a DEX pool's
--      stake script. There is no script to hash, so these are blake2b-224 of a
--      fixed descriptive seed: deterministic across runs, exactly 28 bytes, and
--      shaped like a real hash rather than a repeated byte. They are NOT claimed
--      to be the hash of any script and every one of them is named
--      @synthetic…@ at its definition site.
--
-- Everything that IS the hash of a real, parameter-applied script lives in the
-- per-implementation modules 'BenchmarkOnchain.PlutarchFixtureIds' and
-- 'BenchmarkOnchain.AikenFixtureIds', because the two implementations deploy
-- different bytes and therefore have different ids.
module BenchmarkOnchain.ScriptFixtureIds (
    burnRedeemInputTxId,
    directoryInsertFundingRef,
    directoryMintingNodeRef,
    directoryProgrammableNode2Ref,
    directoryProgrammableNode3Ref,
    directoryProgrammableNodeRef,
    dirNodeRef,
    externalAlwaysSucceedsHash,
    externalAlwaysSucceedsHash2,
    externalScriptInputRef,
    initRef,
    insertNodeInRef,
    issuanceInitRef,
    issuanceRef,
    issuerCred,
    issuerLogicHash,
    leadingPubKeyInputTxId,
    mainnetDexBaseInputRef,
    mainnetDexFeeInputRef,
    mainnetDexPoolInputRef,
    mainnetDexSwapInputTxId,
    manyOutputsCount,
    manyOutputsInputRef,
    manyPoliciesCount,
    manyPoliciesInputRef,
    manyPolicyCS,
    manyPolicyNodeRef,
    manyPubKeyInputCount,
    manyTokensCount,
    manyTokensInputRef,
    manyTokensTokenName,
    mintingLogicHash,
    mixedOwnersInputTxId,
    nonProgrammableCS,
    nonProgrammableCS2,
    paramRef,
    progInputRef,
    programmableBurnInputRef,
    programmableMintFundingRef,
    programmableTransferMintingLogicHash,
    programmableTransferMintingLogicHash2,
    programmableTransferMintingLogicHash3,
    protocolParamsInitRef,
    recipientPkh,
    registeredTokenMintingLogicHash,
    seizeFeeFundingRef,
    seizeInputTxId,
    seizeNoiseInputTxId,
    signerPkh,
    syntheticCurrencySymbol,
    syntheticHash28,
    syntheticScriptHash,
    thirdSignerPkh,
    topUpInputRef,
    transferLogicHash,
    transferManyInputTxId,
) where

import BenchmarkOnchain.CardanoScriptHelpers (assertHash28)
import BenchmarkOnchain.ScriptHelpers (bs2, bs28, txId32, txOutRef32)
import Data.ByteString.Char8 qualified as BS8
import Data.List (sort)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V3
import PlutusTx.Builtins qualified as BI

-- Synthetic-but-valid identities -----------------------------------------
--
-- Ledger-valid stand-ins for identities whose script this repository does not
-- build. blake2b-224 of a namespaced seed: stable across runs and machines,
-- exactly 28 bytes, and (unlike @bs28 0x1a@) indistinguishable in shape from a
-- real hash, so it can never be mistaken for one that was derived.

-- | blake2b-224 of @"wsc-bench-synthetic:" <> seed@.
syntheticHash28 :: String -> BuiltinByteString
syntheticHash28 seed =
    assertHash28
        ("synthetic hash " <> show seed)
        (BI.blake2b_224 (PV1.toBuiltin (BS8.pack ("wsc-bench-synthetic:" <> seed))))

syntheticScriptHash :: String -> ScriptHash
syntheticScriptHash = ScriptHash . syntheticHash28

syntheticCurrencySymbol :: String -> CurrencySymbol
syntheticCurrencySymbol = CurrencySymbol . syntheticHash28

signerPkh :: PubKeyHash
signerPkh = PubKeyHash (bs28 0x01)

recipientPkh :: PubKeyHash
recipientPkh = PubKeyHash (bs28 0x02)

-- | Third-party seize ("issuer logic") script of the benchmarked token. Issuer
-- supplied, not built here — synthetic.
issuerLogicHash :: ScriptHash
issuerLogicHash = syntheticScriptHash "issuer-third-party-logic-script"

issuerCred :: Credential
issuerCred = ScriptCredential issuerLogicHash

-- | Transfer-logic script of the benchmarked token. Issuer supplied — synthetic.
transferLogicHash :: ScriptHash
transferLogicHash = syntheticScriptHash "token-transfer-logic-script"

-- | Minting-logic script of the benchmarked token. Issuer supplied — synthetic.
-- It is a genuine PARAMETER of the programmable minting policy on both
-- implementations, so the policy ids derived from it are real hashes of real
-- parameter-applied scripts.
mintingLogicHash :: ScriptHash
mintingLogicHash = syntheticScriptHash "token-minting-logic-script"

-- | Minting-logic scripts of the three additional programmable tokens the
-- transfer fixtures move. Issuer supplied — synthetic; the policy ids derived
-- from them are real.
programmableTransferMintingLogicHash :: ScriptHash
programmableTransferMintingLogicHash = syntheticScriptHash "transfer-token-1-minting-logic-script"

programmableTransferMintingLogicHash2 :: ScriptHash
programmableTransferMintingLogicHash2 = syntheticScriptHash "transfer-token-2-minting-logic-script"

programmableTransferMintingLogicHash3 :: ScriptHash
programmableTransferMintingLogicHash3 = syntheticScriptHash "transfer-token-3-minting-logic-script"

-- | Minting-logic script of the token registered by the directory-insert
-- fixture. Issuer supplied — synthetic.
registeredTokenMintingLogicHash :: ScriptHash
registeredTokenMintingLogicHash = syntheticScriptHash "directory-insert-registered-token-minting-logic-script"

-- | Unrelated (non-programmable) assets carried alongside the programmable
-- ones. No script exists for them by construction — synthetic.
nonProgrammableCS :: CurrencySymbol
nonProgrammableCS = syntheticCurrencySymbol "unrelated-non-programmable-asset-1"

nonProgrammableCS2 :: CurrencySymbol
nonProgrammableCS2 = syntheticCurrencySymbol "unrelated-non-programmable-asset-2"

-- | Two third-party scripts that own mini-ledger UTxOs (the DEX swap and pool
-- stake credentials, and the two script owners of the mixed-ownership batch).
-- They are not part of this deployment — synthetic. The harness evaluates an
-- always-succeeds stand-in for them, which is why they must be DISTINCT from
-- each other and cannot both be the always-succeeds script's own hash.
externalAlwaysSucceedsHash :: ScriptHash
externalAlwaysSucceedsHash = syntheticScriptHash "external-third-party-stake-script-1"

externalAlwaysSucceedsHash2 :: ScriptHash
externalAlwaysSucceedsHash2 = syntheticScriptHash "external-third-party-stake-script-2"

manyPubKeyInputCount :: Integer
manyPubKeyInputCount = 50

seizeInputTxId :: TxId
seizeInputTxId = txId32 0x5e 0x12

-- | Pubkey UTxO that funds the fee and the residual output's min-UTxO ada in the
-- seize scenarios. Sorts after 'seizeInputTxId' so it is appended to the end of
-- the (TxOutRef-sorted) input list and does not shift any other input's index.
seizeFeeFundingRef :: TxOutRef
seizeFeeFundingRef = txOutRef32 0x5e 0xfe 0

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

-- Scale-stress fixture identities (Aiken bench-axis parity scenarios).

-- | Scenario sizes for the scale-stress transfer axes.
manyTokensCount :: Integer
manyTokensCount = 50

manyOutputsCount :: Integer
manyOutputsCount = 20

manyPoliciesCount :: Integer
manyPoliciesCount = 10

manyTokensInputRef :: TxOutRef
manyTokensInputRef = txOutRef32 0xcc 0x01 0

manyOutputsInputRef :: TxOutRef
manyOutputsInputRef = txOutRef32 0xcc 0x02 0

manyPoliciesInputRef :: TxOutRef
manyPoliciesInputRef = txOutRef32 0xcc 0x03 0

seizeNoiseInputTxId :: TxId
seizeNoiseInputTxId = txId32 0x5e 0x13

-- | Batch-redemption (burn from many collected UTxOs) fixture inputs.
burnRedeemInputTxId :: TxId
burnRedeemInputTxId = txId32 0x5e 0x14

-- | Mixed-ownership batch fixture inputs (different stake owners per input).
mixedOwnersInputTxId :: TxId
mixedOwnersInputTxId = txId32 0xcc 0x04

-- | Existing-treasury input for the mint top-up fixture.
topUpInputRef :: TxOutRef
topUpInputRef = txOutRef32 0xcc 0x05 0

-- | Third pubkey-stake owner for the mixed-ownership fixture (sorts after
-- signerPkh 0x01 and recipientPkh 0x02, keeping signatories canonical).
thirdSignerPkh :: PubKeyHash
thirdSignerPkh = PubKeyHash (bs28 0x03)

-- | Largest @policyCount@ any many-policies scenario asks for.
manyPolicyPoolSize :: Int
manyPolicyPoolSize = 40

-- | Distinct policies for the many-policies axis. There is no script behind
-- them (each would need its own minting-logic script), so they are
-- synthetic-but-valid hashes — but they are SORTED before being indexed, so
-- @manyPolicyCS i@ is still strictly ascending in @i@. That is load-bearing:
-- the positional transfer proofs address directory-node reference inputs at
-- index @1 + i@, and the validators walk the input value in canonical
-- (lexicographic) currency-symbol order. Taking the first @n@ of a sorted pool
-- keeps every prefix ascending too, so the 5/10/20/40 scenarios all line up.
manyPolicyPool :: [CurrencySymbol]
manyPolicyPool =
    fmap CurrencySymbol . sort $
        [syntheticHash28 ("many-policies-axis-policy-" <> show i) | i <- [0 .. manyPolicyPoolSize - 1]]

manyPolicyCS :: Integer -> CurrencySymbol
manyPolicyCS i
    | i < 0 || fromIntegral i >= manyPolicyPoolSize =
        error ("manyPolicyCS: index out of range: " <> show i)
    | otherwise = manyPolicyPool !! fromIntegral i

-- | One directory-node reference input per many-policies policy.
manyPolicyNodeRef :: Integer -> TxOutRef
manyPolicyNodeRef i = txOutRef32 0xbb (0x20 + fromIntegral i) 0

-- | Distinct sorted token names for the many-tokens axis (two-byte names in
-- numeric = lexicographic order).
manyTokensTokenName :: Integer -> TokenName
manyTokensTokenName i =
    TokenName (bs2 (fromIntegral (i `div` 256)) (fromIntegral (i `mod` 256)))

issuanceRef :: TxOutRef
issuanceRef = txOutRef32 0xc0 0xfe 0

protocolParamsInitRef :: TxOutRef
protocolParamsInitRef = txOutRef32 0xaa 0x11 0

issuanceInitRef :: TxOutRef
issuanceInitRef = txOutRef32 0xbb 0x21 0

programmableMintFundingRef :: TxOutRef
programmableMintFundingRef = txOutRef32 0xfa 0xce 0

programmableBurnInputRef :: TxOutRef
programmableBurnInputRef = txOutRef32 0xfa 0xce 1

directoryInsertFundingRef :: TxOutRef
directoryInsertFundingRef = txOutRef32 0xfa 0xce 2

directoryMintingNodeRef :: TxOutRef
directoryMintingNodeRef = txOutRef32 0xbb 0x13 0
