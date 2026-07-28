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
    programmableTransferMintingLogicHash,
    programmableTransferMintingLogicHash2,
    programmableTransferMintingLogicHash3,
    protocolParamsInitRef,
 )
import PlutusLedgerApi.V3
import ProgrammableTokens.OffChain.AikenProgrammableTokenScripts qualified as Aiken

-- (1) Protocol-params NFT: one-shot mint pinned to 'protocolParamsInitRef'.
protocolParamsCS :: CurrencySymbol
protocolParamsCS =
    scriptCurrencySymbol (Aiken.aikenProtocolParamsMintingScript protocolParamsInitRef)

protocolParamsAlwaysFailHash :: ScriptHash
protocolParamsAlwaysFailHash =
    scriptHashFromCardanoScript Aiken.aikenProtocolParamsSpendingScript

-- (2) Issuance-cbor-hex NFT: one-shot mint pinned to 'issuanceInitRef'.
issuancePolicyCS :: CurrencySymbol
issuancePolicyCS =
    scriptCurrencySymbol (Aiken.aikenIssuanceCborHexMintingScript issuanceInitRef)

issuanceAlwaysFailHash :: ScriptHash
issuanceAlwaysFailHash =
    scriptHashFromCardanoScript Aiken.aikenIssuanceCborHexSpendingScript

-- (3) Registry node spending script, parameterised by the protocol-params
-- policy; its credential is in turn a parameter of the registry MINTING policy.
directorySpendHash :: ScriptHash
directorySpendHash =
    scriptHashFromCardanoScript (Aiken.aikenDirectoryNodeSpendingScript protocolParamsCS)

directorySpendCred :: Credential
directorySpendCred = ScriptCredential directorySpendHash

-- | The registry (directory) node authentication policy. The builder recomputes
-- the issuance policy id internally from 'issuanceInitRef', so this and
-- 'issuancePolicyCS' cannot drift apart.
directoryNodeCS :: CurrencySymbol
directoryNodeCS =
    scriptCurrencySymbol
        (Aiken.aikenDirectoryNodeMintingScript initRef issuanceInitRef directorySpendCred)

-- | A deployment has ONE registry policy: the NFTs authenticating registry
-- nodes are minted by the very script the directory-mint scenarios benchmark.
directoryPolicyCS :: CurrencySymbol
directoryPolicyCS = directoryNodeCS

-- (4) Global validator. Aiken has no standalone seize script — third-party
-- seizure is a redeemer arm of this same validator — so 'seizeCred' is it.
globalScriptHash :: ScriptHash
globalScriptHash =
    scriptHashFromCardanoScript (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)

globalCred :: Credential
globalCred = ScriptCredential globalScriptHash

seizeCred :: Credential
seizeCred = globalCred

-- (5) Programmable-logic base: the mini-ledger payment credential, taking the
-- global validator's credential as its only parameter.
progLogicBaseHash :: ScriptHash
progLogicBaseHash =
    scriptHashFromCardanoScript (Aiken.aikenProgrammableLogicBaseScript globalCred)

progLogicBaseCred :: Credential
progLogicBaseCred = ScriptCredential progLogicBaseHash

-- (6) Programmable token policies: @issuance_mint@ applied to the base
-- credential, the registry policy, that token's minting-logic credential, and
-- the global stake credential used for the delegation lookup.
programmableTokenPolicyCS :: ScriptHash -> CurrencySymbol
programmableTokenPolicyCS tokenMintingLogicHash =
    scriptCurrencySymbol
        ( Aiken.aikenProgrammableLogicMintingScript
            progLogicBaseCred
            directoryNodeCS
            (ScriptCredential tokenMintingLogicHash)
            globalCred
        )

-- | The token whose mint/burn scenarios are benchmarked.
mintingPolicyCS :: CurrencySymbol
mintingPolicyCS = programmableTokenPolicyCS mintingLogicHash

-- | The tokens the transfer / seize scenarios move.
programmableTransferCS :: CurrencySymbol
programmableTransferCS = programmableTokenPolicyCS programmableTransferMintingLogicHash

programmableTransferCS2 :: CurrencySymbol
programmableTransferCS2 = programmableTokenPolicyCS programmableTransferMintingLogicHash2

programmableTransferCS3 :: CurrencySymbol
programmableTransferCS3 = programmableTokenPolicyCS programmableTransferMintingLogicHash3

-- | Every derived id, labelled, for the startup 28-byte check and the
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
