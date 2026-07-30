-- | The Plutarch deployment's real identities.
--
-- Every id below is @blake2b-224@ of a compiled, PARAMETER-APPLIED script,
-- produced by the same offchain builders
-- ('ProgrammableTokens.OffChain.Scripts') a real deployment uses — so the
-- benchmark fixtures assert currency symbols and credentials that the scripts
-- they are benchmarking actually have. They are derived bottom-up in the order
-- an honest deployment must follow:
--
-- @
--   protocolParamsInitRef ──▶ protocolParamsCS
--                              ├─▶ globalScriptHash ─┐
--                              ├─▶ seizeScriptHash   ├─▶ progLogicBaseHash
--                              ├─▶ directorySpendHash
--                              └─▶ mintingPolicyCS / programmableTransferCS…
--   issuanceInitRef       ──▶ issuancePolicyCS
--   initRef + issuanceInitRef ─▶ directoryNodeCS
-- @
--
-- The compilation target is 'Production' throughout, which is exactly the
-- config 'BenchmarkOnchain.Compile.compileNoTracing' uses
-- (@tryCompile Production = compile NoTracing@), so the scripts being hashed
-- here are the scripts being measured.
--
-- Note the two-step directory derivation: 'directoryNodeMintingScript' takes
-- the issuance INIT ref and recomputes the issuance policy id itself, so
-- 'directoryNodeCS' and 'issuancePolicyCS' are guaranteed consistent.
module BenchmarkOnchain.PlutarchFixtureIds (
    directoryNodeCS,
    directoryPolicyCS,
    directorySpendHash,
    globalCred,
    globalScriptHash,
    issuanceAlwaysFailHash,
    issuancePolicyCS,
    mintingPolicyCS,
    plutarchDeploymentIds,
    progLogicBaseCred,
    progLogicBaseHash,
    programmableTransferCS,
    programmableTransferCS2,
    programmableTransferCS3,
    protocolParamsAlwaysFailHash,
    protocolParamsCS,
    protocolParamsPolicyId,
    seizeCred,
    seizeScriptHash,
) where

import BenchmarkOnchain.CardanoScriptHelpers (cardanoStakeCredential, cardanoTxIn, policyIdCurrencySymbol, scriptHashFromCardanoScript)
import BenchmarkOnchain.ScriptFixtureIds (
    initRef,
    issuanceInitRef,
    mintingLogicHash,
    pinnedDirectoryNodeCS,
    pinnedDirectorySpendHash,
    pinnedGlobalScriptHash,
    pinnedIssuanceAlwaysFailHash,
    pinnedIssuancePolicyCS,
    pinnedMintingPolicyCS,
    pinnedProgLogicBaseHash,
    pinnedProgrammableTransferCS,
    pinnedProgrammableTransferCS2,
    pinnedProgrammableTransferCS3,
    pinnedProtocolParamsAlwaysFailHash,
    pinnedProtocolParamsCS,
    pinnedSeizeScriptHash,
    programmableTransferMintingLogicHash,
    programmableTransferMintingLogicHash2,
    programmableTransferMintingLogicHash3,
    protocolParamsInitRef,
    useDerivedDeploymentIds,
 )
import Cardano.Api qualified as C
import PlutusLedgerApi.V3
import ProgrammableTokens.OffChain.Scripts qualified as OffchainScripts
import SmartTokens.Core.Scripts (ScriptTarget (Production))

-- (1) Protocol-params NFT: one-shot mint pinned to 'protocolParamsInitRef',
-- parameterised by the canonical always-fail spending-script hash the anchor
-- must sit at.
protocolParamsPolicyId :: C.PolicyId
protocolParamsPolicyId =
    OffchainScripts.scriptPolicyIdV3
        (OffchainScripts.protocolParamsMintingScript Production (cardanoTxIn protocolParamsInitRef))

derivedProtocolParamsCS :: CurrencySymbol
derivedProtocolParamsCS = policyIdCurrencySymbol protocolParamsPolicyId

derivedProtocolParamsAlwaysFailHash :: ScriptHash
derivedProtocolParamsAlwaysFailHash =
    scriptHashFromCardanoScript (OffchainScripts.protocolParamsSpendingScript Production)

-- (2) Issuance-cbor-hex NFT: one-shot mint pinned to 'issuanceInitRef'.
derivedIssuancePolicyCS :: CurrencySymbol
derivedIssuancePolicyCS =
    policyIdCurrencySymbol
        (OffchainScripts.scriptPolicyIdV3 (OffchainScripts.issuanceCborHexMintingScript Production (cardanoTxIn issuanceInitRef)))

derivedIssuanceAlwaysFailHash :: ScriptHash
derivedIssuanceAlwaysFailHash =
    scriptHashFromCardanoScript (OffchainScripts.issuanceCborHexSpendingScript Production)

-- (3) Directory (registry) node policy: parameterised by its own one-shot init
-- ref and the issuance policy id.
derivedDirectoryNodeCS :: CurrencySymbol
derivedDirectoryNodeCS =
    policyIdCurrencySymbol
        ( OffchainScripts.scriptPolicyIdV3
            (OffchainScripts.directoryNodeMintingScript Production (cardanoTxIn initRef) (cardanoTxIn issuanceInitRef))
        )

-- | A deployment has ONE directory policy: the NFTs that authenticate registry
-- nodes are minted by the very script the directory-mint scenarios benchmark.
-- (Before real derivation these were two unrelated placeholders, 0x11 and 0x18,
-- so the registry nodes referenced by the transfer scenarios were authenticated
-- by a policy that did not correspond to the benchmarked directory script.)
directoryPolicyCS :: CurrencySymbol
directoryPolicyCS = directoryNodeCS

-- | Directory node SPENDING script, parameterised by the protocol-params policy.
derivedDirectorySpendHash :: ScriptHash
derivedDirectorySpendHash =
    scriptHashFromCardanoScript (OffchainScripts.directoryNodeSpendingScript Production protocolParamsPolicyId)

-- (4) Global transfer validator and standalone seize validator: both
-- parameterised by the protocol-params policy id alone.
derivedGlobalScriptHash :: ScriptHash
derivedGlobalScriptHash =
    scriptHashFromCardanoScript (OffchainScripts.programmableLogicGlobalScript Production protocolParamsPolicyId)

derivedSeizeScriptHash :: ScriptHash
derivedSeizeScriptHash =
    scriptHashFromCardanoScript (OffchainScripts.programmableSeizeScript Production protocolParamsPolicyId)

-- (5) Programmable-logic base (the mini-ledger payment credential): takes the
-- global validator's stake credential and the protocol-params policy id, from
-- which it re-derives the seize credential internally — so 'progLogicBaseHash'
-- and 'seizeScriptHash' cannot drift apart.
derivedProgLogicBaseHash :: ScriptHash
derivedProgLogicBaseHash =
    scriptHashFromCardanoScript
        ( OffchainScripts.programmableLogicBaseScript
            Production
            (cardanoStakeCredential derivedGlobalScriptHash)
            protocolParamsPolicyId
        )

-- (6) Programmable token policies: the shared issuance policy applied to the
-- protocol-params currency symbol and to THAT token's minting-logic hash.
programmableTokenPolicyCS :: ScriptHash -> CurrencySymbol
programmableTokenPolicyCS tokenMintingLogicHash =
    policyIdCurrencySymbol
        ( OffchainScripts.scriptPolicyIdV3
            ( OffchainScripts.programmableLogicMintingScript
                Production
                derivedProtocolParamsCS
                (cardanoStakeCredential tokenMintingLogicHash)
            )
        )

-- | The token whose mint/burn scenarios are benchmarked.
derivedMintingPolicyCS :: CurrencySymbol
derivedMintingPolicyCS = programmableTokenPolicyCS mintingLogicHash

-- | The tokens the transfer / seize scenarios move.
derivedProgrammableTransferCS :: CurrencySymbol
derivedProgrammableTransferCS = programmableTokenPolicyCS programmableTransferMintingLogicHash

derivedProgrammableTransferCS2 :: CurrencySymbol
derivedProgrammableTransferCS2 = programmableTokenPolicyCS programmableTransferMintingLogicHash2

derivedProgrammableTransferCS3 :: CurrencySymbol
derivedProgrammableTransferCS3 = programmableTokenPolicyCS programmableTransferMintingLogicHash3

-- Mode switch ---------------------------------------------------------------
--
-- The exported names resolve to the PINNED shared constants by default so
-- benchmark contexts are invariant under script-byte changes; with
-- @BENCH_DERIVED_IDS=1@ they resolve to the real derived hashes above. See the
-- note in "BenchmarkOnchain.ScriptFixtureIds".

pick :: a -> a -> a
pick derived pinned = if useDerivedDeploymentIds then derived else pinned

protocolParamsCS :: CurrencySymbol
protocolParamsCS = pick derivedProtocolParamsCS pinnedProtocolParamsCS

protocolParamsAlwaysFailHash :: ScriptHash
protocolParamsAlwaysFailHash = pick derivedProtocolParamsAlwaysFailHash pinnedProtocolParamsAlwaysFailHash

issuancePolicyCS :: CurrencySymbol
issuancePolicyCS = pick derivedIssuancePolicyCS pinnedIssuancePolicyCS

issuanceAlwaysFailHash :: ScriptHash
issuanceAlwaysFailHash = pick derivedIssuanceAlwaysFailHash pinnedIssuanceAlwaysFailHash

directoryNodeCS :: CurrencySymbol
directoryNodeCS = pick derivedDirectoryNodeCS pinnedDirectoryNodeCS

directorySpendHash :: ScriptHash
directorySpendHash = pick derivedDirectorySpendHash pinnedDirectorySpendHash

globalScriptHash :: ScriptHash
globalScriptHash = pick derivedGlobalScriptHash pinnedGlobalScriptHash

globalCred :: Credential
globalCred = ScriptCredential globalScriptHash

seizeScriptHash :: ScriptHash
seizeScriptHash = pick derivedSeizeScriptHash pinnedSeizeScriptHash

seizeCred :: Credential
seizeCred = ScriptCredential seizeScriptHash

progLogicBaseHash :: ScriptHash
progLogicBaseHash = pick derivedProgLogicBaseHash pinnedProgLogicBaseHash

progLogicBaseCred :: Credential
progLogicBaseCred = ScriptCredential progLogicBaseHash

mintingPolicyCS :: CurrencySymbol
mintingPolicyCS = pick derivedMintingPolicyCS pinnedMintingPolicyCS

programmableTransferCS :: CurrencySymbol
programmableTransferCS = pick derivedProgrammableTransferCS pinnedProgrammableTransferCS

programmableTransferCS2 :: CurrencySymbol
programmableTransferCS2 = pick derivedProgrammableTransferCS2 pinnedProgrammableTransferCS2

programmableTransferCS3 :: CurrencySymbol
programmableTransferCS3 = pick derivedProgrammableTransferCS3 pinnedProgrammableTransferCS3

-- | Every ACTIVE id, labelled, for the startup 28-byte check and the
-- @BENCH_DUMP_IDS@ dump.
plutarchDeploymentIds :: [(String, BuiltinByteString)]
plutarchDeploymentIds =
    [ ("protocolParamsCS", unCurrencySymbol protocolParamsCS)
    , ("protocolParamsSpendHash", unScriptHash protocolParamsAlwaysFailHash)
    , ("issuancePolicyCS", unCurrencySymbol issuancePolicyCS)
    , ("issuanceSpendHash", unScriptHash issuanceAlwaysFailHash)
    , ("directoryNodeCS", unCurrencySymbol directoryNodeCS)
    , ("directorySpendHash", unScriptHash directorySpendHash)
    , ("globalScriptHash", unScriptHash globalScriptHash)
    , ("seizeScriptHash", unScriptHash seizeScriptHash)
    , ("progLogicBaseHash", unScriptHash progLogicBaseHash)
    , ("mintingPolicyCS", unCurrencySymbol mintingPolicyCS)
    , ("programmableTransferCS", unCurrencySymbol programmableTransferCS)
    , ("programmableTransferCS2", unCurrencySymbol programmableTransferCS2)
    , ("programmableTransferCS3", unCurrencySymbol programmableTransferCS3)
    ]
  where
    unCurrencySymbol (CurrencySymbol bs) = bs
    unScriptHash (ScriptHash bs) = bs
