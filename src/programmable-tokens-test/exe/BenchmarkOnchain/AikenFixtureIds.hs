-- | The Aiken deployment's real identities.
--
-- The mirror of 'BenchmarkOnchain.PlutarchFixtureIds': every id below is
-- @blake2b-224@ of a compiled, PARAMETER-APPLIED Aiken script, produced by the
-- same builders ('ProgrammableTokens.OffChain.AikenProgrammableTokenScripts')
-- a real Aiken deployment uses. Each harness therefore benchmarks an honest
-- deployment of its OWN implementation rather than asserting currency symbols
-- no script could have.
--
-- The two deployments necessarily differ in their ids — the scripts differ, so
-- the hashes do. Where a fixture holds several policies in one value that
-- changes their sorted order between the harnesses; that is a genuine property
-- of the two deployments, not a harness artifact.
--
-- Derivation order (the dependency chain a deployment must follow):
--
-- @
--   protocolParamsInitRef ──▶ protocolParamsCS
--                              ├─▶ directorySpendCred ──▶ directoryNodeCS
--                              └─▶ globalCred ──▶ progLogicBaseCred ──▶ token policies
--   issuanceInitRef       ──▶ issuancePolicyCS
-- @
--
-- Note there is no separate seize script: Aiken folds third-party seizure into
-- @programmable_logic_global@, so 'seizeCred' is the global credential.
module BenchmarkOnchain.AikenFixtureIds (
    aikenDeploymentIds,
    directoryNodeCS,
    directoryPolicyCS,
    directorySpendCred,
    directorySpendHash,
    globalCred,
    globalScriptHash,
    issuanceAlwaysFailHash,
    issuancePolicyCS,
    mintingPolicyCS,
    progLogicBaseCred,
    progLogicBaseHash,
    programmableTransferCS,
    programmableTransferCS2,
    programmableTransferCS3,
    protocolParamsAlwaysFailHash,
    protocolParamsCS,
    seizeCred,
) where

import BenchmarkOnchain.CardanoScriptHelpers (scriptCurrencySymbol, scriptHashFromCardanoScript)
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
    programmableTransferMintingLogicHash,
    programmableTransferMintingLogicHash2,
    programmableTransferMintingLogicHash3,
    protocolParamsInitRef,
    useDerivedDeploymentIds,
 )
import PlutusLedgerApi.V3
import ProgrammableTokens.OffChain.AikenProgrammableTokenScripts qualified as Aiken

-- (1) Protocol-params NFT: one-shot mint pinned to 'protocolParamsInitRef'.
derivedProtocolParamsCS :: CurrencySymbol
derivedProtocolParamsCS =
    scriptCurrencySymbol (Aiken.aikenProtocolParamsMintingScript protocolParamsInitRef)

derivedProtocolParamsAlwaysFailHash :: ScriptHash
derivedProtocolParamsAlwaysFailHash =
    scriptHashFromCardanoScript Aiken.aikenProtocolParamsSpendingScript

-- (2) Issuance-cbor-hex NFT: one-shot mint pinned to 'issuanceInitRef'.
derivedIssuancePolicyCS :: CurrencySymbol
derivedIssuancePolicyCS =
    scriptCurrencySymbol (Aiken.aikenIssuanceCborHexMintingScript issuanceInitRef)

derivedIssuanceAlwaysFailHash :: ScriptHash
derivedIssuanceAlwaysFailHash =
    scriptHashFromCardanoScript Aiken.aikenIssuanceCborHexSpendingScript

-- (3) Registry node spending script, parameterised by the protocol-params
-- policy; its credential is in turn a parameter of the registry MINTING policy.
derivedDirectorySpendHash :: ScriptHash
derivedDirectorySpendHash =
    scriptHashFromCardanoScript (Aiken.aikenDirectoryNodeSpendingScript derivedProtocolParamsCS)

derivedDirectorySpendCred :: Credential
derivedDirectorySpendCred = ScriptCredential derivedDirectorySpendHash

-- | The registry (directory) node authentication policy. The builder recomputes
-- the issuance policy id internally from 'issuanceInitRef', so this and
-- 'issuancePolicyCS' cannot drift apart.
derivedDirectoryNodeCS :: CurrencySymbol
derivedDirectoryNodeCS =
    scriptCurrencySymbol
        (Aiken.aikenDirectoryNodeMintingScript initRef issuanceInitRef derivedDirectorySpendCred)

-- | A deployment has ONE registry policy: the NFTs authenticating registry
-- nodes are minted by the very script the directory-mint scenarios benchmark.
directoryPolicyCS :: CurrencySymbol
directoryPolicyCS = directoryNodeCS

-- (4) Global validator. Aiken has no standalone seize script — third-party
-- seizure is a redeemer arm of this same validator — so 'seizeCred' is it.
derivedGlobalScriptHash :: ScriptHash
derivedGlobalScriptHash =
    scriptHashFromCardanoScript (Aiken.aikenProgrammableLogicGlobalScript derivedProtocolParamsCS)

derivedGlobalCred :: Credential
derivedGlobalCred = ScriptCredential derivedGlobalScriptHash

-- (5) Programmable-logic base: the mini-ledger payment credential, taking the
-- global validator's credential as its only parameter.
derivedProgLogicBaseHash :: ScriptHash
derivedProgLogicBaseHash =
    scriptHashFromCardanoScript (Aiken.aikenProgrammableLogicBaseScript derivedGlobalCred)


-- (6) Programmable token policies: @issuance_mint@ applied to the base
-- credential, the registry policy, that token's minting-logic credential, and
-- the global stake credential used for the delegation lookup.
programmableTokenPolicyCS :: ScriptHash -> CurrencySymbol
programmableTokenPolicyCS tokenMintingLogicHash =
    scriptCurrencySymbol
        ( Aiken.aikenProgrammableLogicMintingScript
            (ScriptCredential derivedProgLogicBaseHash)
            derivedDirectoryNodeCS
            (ScriptCredential tokenMintingLogicHash)
            derivedGlobalCred
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

-- Mode switch: see the note in "BenchmarkOnchain.ScriptFixtureIds". Aiken has
-- no standalone seize script (seizure is a redeemer arm of the global
-- validator), so in BOTH modes 'seizeCred' is the global credential; the
-- pinned global hash is the lexicographically smallest, so it still sorts
-- first in every withdrawal map.

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

directorySpendHash :: ScriptHash
directorySpendHash = pick derivedDirectorySpendHash pinnedDirectorySpendHash

directorySpendCred :: Credential
directorySpendCred = ScriptCredential directorySpendHash

directoryNodeCS :: CurrencySymbol
directoryNodeCS = pick derivedDirectoryNodeCS pinnedDirectoryNodeCS

globalScriptHash :: ScriptHash
globalScriptHash = pick derivedGlobalScriptHash pinnedGlobalScriptHash

globalCred :: Credential
globalCred = ScriptCredential globalScriptHash

seizeCred :: Credential
seizeCred = globalCred

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
aikenDeploymentIds :: [(String, BuiltinByteString)]
aikenDeploymentIds =
    [ ("protocolParamsCS", unCurrencySymbol protocolParamsCS)
    , ("protocolParamsSpendHash", unScriptHash protocolParamsAlwaysFailHash)
    , ("issuancePolicyCS", unCurrencySymbol issuancePolicyCS)
    , ("issuanceSpendHash", unScriptHash issuanceAlwaysFailHash)
    , ("directoryNodeCS", unCurrencySymbol directoryNodeCS)
    , ("directorySpendHash", unScriptHash directorySpendHash)
    , ("globalScriptHash", unScriptHash globalScriptHash)
    , ("progLogicBaseHash", unScriptHash progLogicBaseHash)
    , ("mintingPolicyCS", unCurrencySymbol mintingPolicyCS)
    , ("programmableTransferCS", unCurrencySymbol programmableTransferCS)
    , ("programmableTransferCS2", unCurrencySymbol programmableTransferCS2)
    , ("programmableTransferCS3", unCurrencySymbol programmableTransferCS3)
    ]
  where
    unCurrencySymbol (CurrencySymbol bs) = bs
    unScriptHash (ScriptHash bs) = bs
