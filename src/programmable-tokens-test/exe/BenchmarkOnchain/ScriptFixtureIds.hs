module BenchmarkOnchain.ScriptFixtureIds (
    directoryInsertFundingRef,
    directoryMintingNodeRef,
    directoryNodeCS,
    directoryPolicyCS,
    directoryProgrammableNode2Ref,
    directoryProgrammableNode3Ref,
    directoryProgrammableNodeRef,
    dirNodeRef,
    externalAlwaysSucceedsHash,
    externalAlwaysSucceedsHash2,
    externalScriptInputRef,
    globalCred,
    globalScriptHash,
    initRef,
    insertNodeInRef,
    issuanceInitRef,
    issuancePolicyCS,
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
    mintingPolicyCS,
    nonProgrammableCS,
    nonProgrammableCS2,
    paramRef,
    progInputRef,
    progLogicBaseCred,
    progLogicBaseHash,
    programmableBurnInputRef,
    programmableMintFundingRef,
    programmableTransferCS,
    programmableTransferCS2,
    programmableTransferCS3,
    protocolParamsCS,
    protocolParamsInitRef,
    recipientPkh,
    seizeInputTxId,
    seizeNoiseInputTxId,
    signerPkh,
    transferLogicHash,
    transferManyInputTxId,
) where

import BenchmarkOnchain.ScriptHelpers (bs2, bs28, txId32, txOutRef32)
import PlutusLedgerApi.V3

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

-- | Distinct programmable policies for the many-policies axis; ascending byte
-- patterns keep the generated currency symbols in canonical (lexicographic)
-- order, clear of every other fixture policy (0x10..0x1e).
manyPolicyCS :: Integer -> CurrencySymbol
manyPolicyCS i = CurrencySymbol (bs28 (0x60 + fromIntegral i))

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
