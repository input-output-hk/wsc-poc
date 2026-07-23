{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the two-parameter dual-arm issuance policy
-- (doc/design-issuance-dual-arm-custody.md §5-§9). These exercise the Local arm:
-- the policy reads the base / directory credentials from the protocol-params
-- reference input, proves registration by a witnessed reference input, and
-- enforces custody with the universal full-output scan.
module ProgrammableTokens.Test.ProgrammableLogicMinting (
    tests,
) where

import Data.ByteString qualified as BS
import Data.Either (isLeft, isRight)
import Data.Word (Word8)
import Plutarch.Evaluate (applyArguments, evalScript)
import Plutarch.Internal.Term (Config (NoTracing), Script, Term, compile)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Value (assetClass, assetClassValue)
import PlutusLedgerApi.V3
import PlutusTx qualified
import ProgrammableTokens.Test.ScriptContext.Builder (
    ScriptContextBuilder,
    buildBalancedScriptContext,
    mkAdaValue,
    withAddress,
    withInlineDatum,
    withMintingScript,
    withOutRef,
    withOutput,
    withRedeemer,
    withReferenceInput,
    withRewardingScriptWitness,
    withTxOutAddress,
    withTxOutValue,
    withValue,
    withWithdrawal,
 )
import SmartTokens.Contracts.Issuance (MintRedeemer (..), RegistrationWitness (..), mkProgrammableLogicMinting)
import SmartTokens.Contracts.ProgrammableLogicBase (ProgrammableLogicGlobalRedeemer (SeizeAct))
import SmartTokens.Types.Constants (protocolParamsToken)
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (DirectorySetNode))
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (ProgrammableLogicGlobalParams))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "ProgrammableLogicMinting unit tests"
        [ testCase "unit_mint_to_programmable_logic_output_succeeds" unit_mint_to_programmable_logic_output_succeeds
        , testCase "unit_mint_to_base_without_stake_cred_succeeds" unit_mint_to_base_without_stake_cred_succeeds
        , testCase "unit_mint_to_pubkey_output_rejected" unit_mint_to_pubkey_output_rejected
        , testCase "unit_mint_split_between_base_and_pubkey_rejected" unit_mint_split_between_base_and_pubkey_rejected
        , testCase "unit_mint_of_unregistered_policy_rejected" unit_mint_of_unregistered_policy_rejected
        , -- §14.2 functionality
          testCase "unit_local_multi_name_cip68_succeeds" unit_local_multi_name_cip68_succeeds
        , testCase "unit_local_multi_recipient_succeeds" unit_local_multi_recipient_succeeds
        , testCase "unit_burnOnly_succeeds" unit_burnOnly_succeeds
        , -- §14.1 security red tests
          testCase "unit_local_mintA_escape_with_burnB_rejected" unit_local_mintA_escape_with_burnB_rejected
        , testCase "unit_local_negative_registration_index_rejected" unit_local_negative_registration_index_rejected
        , testCase "unit_burnOnly_with_positive_mint_rejected" unit_burnOnly_with_positive_mint_rejected
        , testCase "unit_delegateTransfer_without_global_rejected" unit_delegateTransfer_without_global_rejected
        , testCase "unit_delegateTransfer_with_global_succeeds" unit_delegateTransfer_with_global_succeeds
        , testCase "unit_delegateSeize_matching_node_succeeds" unit_delegateSeize_matching_node_succeeds
        , testCase "unit_delegateSeize_wrong_node_rejected" unit_delegateSeize_wrong_node_rejected
        ]

-- | Security S2: minting a policy with no directory registration is rejected.
unit_mint_of_unregistered_policy_rejected :: Assertion
unit_mint_of_unregistered_policy_rejected =
    assertScriptFails programmableMintUnregisteredCtx

unit_mint_to_programmable_logic_output_succeeds :: Assertion
unit_mint_to_programmable_logic_output_succeeds =
    assertScriptSucceeds programmableMintCtx

unit_mint_to_pubkey_output_rejected :: Assertion
unit_mint_to_pubkey_output_rejected =
    assertScriptFails programmableMintEscapeCtx

unit_mint_split_between_base_and_pubkey_rejected :: Assertion
unit_mint_split_between_base_and_pubkey_rejected =
    assertScriptFails programmableMintSplitEscapeCtx

-- | Under the dual-arm design the policy no longer enforces an inline stake
-- credential on mint outputs (§2.2): a stake-less base output is the builder's
-- responsibility, not a consensus rule. The Local full scan cares only that no
-- non-base output holds the policy, so a mint to a stake-less BASE output passes.
unit_mint_to_base_without_stake_cred_succeeds :: Assertion
unit_mint_to_base_without_stake_cred_succeeds =
    assertScriptSucceeds programmableMintNoStakeCtx

assertScriptSucceeds :: ScriptContext -> Assertion
assertScriptSucceeds ctx =
    assertBool "expected successful evaluation" (scriptSucceeds ctx)

assertScriptFails :: ScriptContext -> Assertion
assertScriptFails ctx =
    assertBool "expected script failure" (scriptFails ctx)

mintArgs :: ScriptContext -> [Data]
mintArgs ctx =
    [ PlutusTx.toData protocolParamsCS
    , PlutusTx.toData mintingLogicHash
    , PlutusTx.toData ctx
    ]

scriptSucceeds :: ScriptContext -> Bool
scriptSucceeds ctx =
    let (res, _budget, _logs) = evalScript (applyArguments mintingScript (mintArgs ctx))
     in isRight res

scriptFails :: ScriptContext -> Bool
scriptFails ctx =
    let (res, _budget, _logs) = evalScript (applyArguments mintingScript (mintArgs ctx))
     in isLeft res

compileNoTracing :: (forall s. Term s a) -> Script
compileNoTracing term =
    either (error . (("compile failed: " ++) . show)) id (compile NoTracing term)

mintingScript :: Script
mintingScript = compileNoTracing mkProgrammableLogicMinting

bs28 :: Word8 -> BuiltinByteString
bs28 w = PV1.toBuiltin (BS.replicate 28 w)

progLogicBaseHash :: ScriptHash
progLogicBaseHash = ScriptHash (bs28 0x12)

progLogicBaseCred :: Credential
progLogicBaseCred = ScriptCredential progLogicBaseHash

mintingLogicHash :: ScriptHash
mintingLogicHash = ScriptHash (bs28 0x16)

mintingPolicyCS :: CurrencySymbol
mintingPolicyCS = CurrencySymbol (bs28 0x19)

directoryNodeCS :: CurrencySymbol
directoryNodeCS = CurrencySymbol (bs28 0x11)

protocolParamsCS :: CurrencySymbol
protocolParamsCS = CurrencySymbol (bs28 0x10)

globalCred :: Credential
globalCred = ScriptCredential (ScriptHash (bs28 0x13))

seizeCred :: Credential
seizeCred = ScriptCredential (ScriptHash (bs28 0x1c))

transferLogicHash :: ScriptHash
transferLogicHash = ScriptHash (bs28 0x15)

issuerCred :: Credential
issuerCred = ScriptCredential (ScriptHash (bs28 0x18))

tailCS :: CurrencySymbol
tailCS = CurrencySymbol (PV1.toBuiltin (BS.replicate 28 0xff))

-- | Registry node registering @mintingPolicyCS@ (security S2 proof of
-- registration): the directory NFT named after the policy id, keyed on it.
mintingRegistryNode :: DirectorySetNode
mintingRegistryNode =
    DirectorySetNode mintingPolicyCS tailCS (ScriptCredential transferLogicHash) issuerCred (CurrencySymbol "")

-- | The four-field protocol-params datum the Local arm reads its base/directory
-- credentials from.
protocolParamsDatum :: ProgrammableLogicGlobalParams
protocolParamsDatum =
    ProgrammableLogicGlobalParams directoryNodeCS progLogicBaseCred globalCred seizeCred

-- | The protocol-params reference input. Its TxOutRef sorts BEFORE the registry
-- node ("aa.." < "bb.."), so it lands at reference-input index 0 and the
-- registry node at index 1.
paramsRefBuilder :: ScriptContextBuilder
paramsRefBuilder =
    withReferenceInput
        ( withOutRef (TxOutRef (TxId (PV1.toBuiltin (BS.replicate 32 0xaa))) 0)
            <> withAddress (pubKeyAddress signerPkh)
            <> withValue (mkAdaValue 3_000_000 <> assetClassValue (assetClass protocolParamsCS protocolParamsToken) 1)
            <> withInlineDatum (PlutusTx.toBuiltinData protocolParamsDatum)
        )

registryNodeRefBuilder :: ScriptContextBuilder
registryNodeRefBuilder =
    withReferenceInput
        ( withOutRef (TxOutRef (TxId (PV1.toBuiltin (BS.replicate 32 0xbb))) 0)
            <> withAddress (pubKeyAddress signerPkh)
            <> withValue (mkAdaValue 3_000_000 <> mintNamedNode)
            <> withInlineDatum (PlutusTx.toBuiltinData mintingRegistryNode)
        )
  where
    mintNamedNode = case mintingPolicyCS of CurrencySymbol bs -> assetClassValue (assetClass directoryNodeCS (TokenName bs)) 1

signerPkh :: PubKeyHash
signerPkh = PubKeyHash (bs28 0x01)

pubKeyAddress :: PubKeyHash -> Address
pubKeyAddress pkh = Address (PubKeyCredential pkh) Nothing

mintValue :: Integer -> Value
mintValue quantity =
    assetClassValue (assetClass mintingPolicyCS (TokenName "0c")) quantity

-- | Local mint redeemer: minting-logic withdrawal at index 0, protocol-params
-- reference input at index 0, registration proven by the registry node at the
-- given reference-input index.
localRedeemer :: Integer -> BuiltinData
localRedeemer regRefIdx =
    PlutusTx.toBuiltinData (Local 0 0 (RegisteredByReferenceInput regRefIdx))

-- | Base address WITH an inline stake credential (the owner).
baseAddrWithStake :: Address
baseAddrWithStake =
    Address progLogicBaseCred (Just (StakingHash (PubKeyCredential signerPkh)))

programmableMintCtx :: ScriptContext
programmableMintCtx =
    buildBalancedScriptContext
        ( withRedeemer (localRedeemer 1)
            <> withMintingScript (mintValue 1) (localRedeemer 1)
            <> withWithdrawal (ScriptCredential mintingLogicHash) 0
            <> paramsRefBuilder
            <> registryNodeRefBuilder
            <> withOutput
                ( withTxOutAddress baseAddrWithStake
                    <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                )
        )

-- | Security S2: identical to the success case but with NO registry-node
-- reference input, so the witnessed registration index resolves to no directory
-- NFT and the mint is rejected.
programmableMintUnregisteredCtx :: ScriptContext
programmableMintUnregisteredCtx =
    buildBalancedScriptContext
        ( withRedeemer (localRedeemer 1)
            <> withMintingScript (mintValue 1) (localRedeemer 1)
            <> withWithdrawal (ScriptCredential mintingLogicHash) 0
            <> paramsRefBuilder
            <> withOutput
                ( withTxOutAddress baseAddrWithStake
                    <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                )
        )

-- | Mint to a stake-less BASE output: accepted under §2.2 (no consensus stake rule).
programmableMintNoStakeCtx :: ScriptContext
programmableMintNoStakeCtx =
    buildBalancedScriptContext
        ( withRedeemer (localRedeemer 1)
            <> withMintingScript (mintValue 1) (localRedeemer 1)
            <> withWithdrawal (ScriptCredential mintingLogicHash) 0
            <> paramsRefBuilder
            <> registryNodeRefBuilder
            <> withOutput
                ( withTxOutAddress (Address progLogicBaseCred Nothing)
                    <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                )
        )

programmableMintEscapeCtx :: ScriptContext
programmableMintEscapeCtx =
    buildBalancedScriptContext
        ( withRedeemer (localRedeemer 1)
            <> withMintingScript (mintValue 1) (localRedeemer 1)
            <> withWithdrawal (ScriptCredential mintingLogicHash) 0
            <> paramsRefBuilder
            <> registryNodeRefBuilder
            <> withOutput
                ( withTxOutAddress (pubKeyAddress signerPkh)
                    <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                )
        )

programmableMintSplitEscapeCtx :: ScriptContext
programmableMintSplitEscapeCtx =
    buildBalancedScriptContext
        ( withRedeemer (localRedeemer 1)
            <> withMintingScript (mintValue 2) (localRedeemer 1)
            <> withWithdrawal (ScriptCredential mintingLogicHash) 0
            <> paramsRefBuilder
            <> registryNodeRefBuilder
            <> withOutput
                ( withTxOutAddress baseAddrWithStake
                    <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                )
            <> withOutput
                ( withTxOutAddress (pubKeyAddress signerPkh)
                    <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                )
        )

-- =====================================================================
-- §14.1 / §14.2 dual-arm custody test matrix (Local + BurnOnly arms;
-- delegate-arm binding). Indices: minting-logic withdrawal at 0 (single
-- withdrawal), protocol-params reference input at 0, registry node at 1.
-- =====================================================================

mintValueName :: BuiltinByteString -> Integer -> Value
mintValueName name quantity =
    assetClassValue (assetClass mintingPolicyCS (TokenName name)) quantity

-- | Second registered policy for mint-A/burn-B — a distinct token NAME of the
-- SAME policy (the CIP-68 pattern), so both are under @mintingPolicyCS@.
otherName :: BuiltinByteString
otherName = "0d"

-- §14.2: CIP-68 pair — two token names of one policy, both to base. PASSES.
unit_local_multi_name_cip68_succeeds :: Assertion
unit_local_multi_name_cip68_succeeds =
    assertScriptSucceeds $
        buildBalancedScriptContext
            ( withRedeemer (localRedeemer 1)
                <> withMintingScript (mintValue 1 <> mintValueName otherName 1) (localRedeemer 1)
                <> withWithdrawal (ScriptCredential mintingLogicHash) 0
                <> paramsRefBuilder
                <> registryNodeRefBuilder
                <> withOutput
                    ( withTxOutAddress baseAddrWithStake
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1 <> mintValueName otherName 1)
                    )
            )

-- §14.2: multi-recipient issuance — mint split across two base outputs. PASSES.
unit_local_multi_recipient_succeeds :: Assertion
unit_local_multi_recipient_succeeds =
    assertScriptSucceeds $
        buildBalancedScriptContext
            ( withRedeemer (localRedeemer 1)
                <> withMintingScript (mintValue 2) (localRedeemer 1)
                <> withWithdrawal (ScriptCredential mintingLogicHash) 0
                <> paramsRefBuilder
                <> registryNodeRefBuilder
                <> withOutput
                    ( withTxOutAddress baseAddrWithStake
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                    )
                <> withOutput
                    ( withTxOutAddress baseAddrWithStake
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                    )
            )

-- §14.2: BurnOnly — a pure burn needs only the minting-logic withdrawal and an
-- all-non-positive own-policy mint map. PASSES.
unit_burnOnly_succeeds :: Assertion
unit_burnOnly_succeeds =
    assertScriptSucceeds $
        buildBalancedScriptContext
            ( withRedeemer burnOnlyRedeemer
                <> withMintingScript (mintValue (-1)) burnOnlyRedeemer
                <> withWithdrawal (ScriptCredential mintingLogicHash) 0
            )

-- §14.1: mint name A to a PUBKEY output while burning name B — the per-output
-- full scan sees own-policy value outside base regardless of the B burn. FAILS.
unit_local_mintA_escape_with_burnB_rejected :: Assertion
unit_local_mintA_escape_with_burnB_rejected =
    assertScriptFails $
        buildBalancedScriptContext
            ( withRedeemer (localRedeemer 1)
                <> withMintingScript (mintValue 1 <> mintValueName otherName (-1)) (localRedeemer 1)
                <> withWithdrawal (ScriptCredential mintingLogicHash) 0
                <> paramsRefBuilder
                <> registryNodeRefBuilder
                <> withOutput
                    ( withTxOutAddress (pubKeyAddress signerPkh)
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                    )
            )

-- §14.1: a negative registration index must be rejected explicitly (pcheckedDrop),
-- not rely on budget exhaustion. FAILS.
unit_local_negative_registration_index_rejected :: Assertion
unit_local_negative_registration_index_rejected =
    assertScriptFails $
        buildBalancedScriptContext
            ( withRedeemer (localRedeemer (-1))
                <> withMintingScript (mintValue 1) (localRedeemer (-1))
                <> withWithdrawal (ScriptCredential mintingLogicHash) 0
                <> paramsRefBuilder
                <> registryNodeRefBuilder
                <> withOutput
                    ( withTxOutAddress baseAddrWithStake
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                    )
            )

-- §14.1: BurnOnly with a POSITIVE own-policy mint entry is not a pure burn. FAILS.
unit_burnOnly_with_positive_mint_rejected :: Assertion
unit_burnOnly_with_positive_mint_rejected =
    assertScriptFails $
        buildBalancedScriptContext
            ( withRedeemer burnOnlyRedeemer
                <> withMintingScript (mintValue 1) burnOnlyRedeemer
                <> withWithdrawal (ScriptCredential mintingLogicHash) 0
                <> withOutput
                    ( withTxOutAddress baseAddrWithStake
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                    )
            )

-- §14.1: a DelegateTransfer witness whose global-withdrawal index does not point
-- at the params global credential is rejected (no global custody actually runs).
unit_delegateTransfer_without_global_rejected :: Assertion
unit_delegateTransfer_without_global_rejected =
    assertScriptFails $
        buildBalancedScriptContext
            ( withRedeemer delegateTransferRedeemer
                <> withMintingScript (mintValue 1) delegateTransferRedeemer
                <> withWithdrawal (ScriptCredential mintingLogicHash) 0
                <> paramsRefBuilder
                <> registryNodeRefBuilder
                <> withOutput
                    ( withTxOutAddress (pubKeyAddress signerPkh)
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                    )
            )

burnOnlyRedeemer :: BuiltinData
burnOnlyRedeemer = PlutusTx.toBuiltinData (BurnOnly 0)

-- DelegateTransfer {wdrlIdx=0, paramsRefIdx=0, nodeRefIdx=1, globalWdrlIdx=0}.
-- With only the minting-logic withdrawal present, globalWdrlIdx=0 resolves to a
-- credential that is not the params global credential, so it is rejected.
delegateTransferRedeemer :: BuiltinData
delegateTransferRedeemer = PlutusTx.toBuiltinData (DelegateTransfer 0 0 1 0)

-- =====================================================================
-- Delegate-arm tests (DelegateTransfer / DelegateSeize). The issuance policy
-- only verifies the delegate is invoked and scoped to this policy — custody is
-- the delegate's job, so these contexts do not need the delegate to actually run.
-- Withdrawal indices are AssocMap insertion order; the seize redeemer sits in the
-- purpose-sorted redeemers map (Minting < Rewarding).
-- =====================================================================

-- §14.2: DelegateTransfer with the global withdrawal present at the witnessed
-- index (globalCred == params.globalLogicCred). PASSES.
unit_delegateTransfer_with_global_succeeds :: Assertion
unit_delegateTransfer_with_global_succeeds =
    assertScriptSucceeds $
        buildBalancedScriptContext
            ( withRedeemer delegateTransferOk
                <> withMintingScript (mintValue 1) delegateTransferOk
                <> withWithdrawal (ScriptCredential mintingLogicHash) 0
                <> withWithdrawal globalCred 0
                <> paramsRefBuilder
                <> registryNodeRefBuilder
                <> withOutput
                    ( withTxOutAddress baseAddrWithStake
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                    )
            )
  where
    -- DelegateTransfer {wdrlIdx, paramsRefIdx=0, nodeRefIdx=1, globalWdrlIdx}.
    delegateTransferOk = PlutusTx.toBuiltinData (DelegateTransfer mlWdrlIdx 0 1 globalWdrlIdx)

-- §14.2: DelegateSeize whose seize redeemer names the SAME directory node the
-- issuance witness proved (directoryNodeIdx == nodeRefIdx). PASSES.
unit_delegateSeize_matching_node_succeeds :: Assertion
unit_delegateSeize_matching_node_succeeds =
    assertScriptSucceeds $
        buildBalancedScriptContext
            ( withRedeemer (delegateSeizeRedeemer 1)
                <> withMintingScript (mintValue 1) (delegateSeizeRedeemer 1)
                <> withWithdrawal (ScriptCredential mintingLogicHash) 0
                <> withRewardingScriptWitness (PlutusTx.toBuiltinData (SeizeAct 1 [0] 0 1 0)) seizeCred 0
                <> paramsRefBuilder
                <> registryNodeRefBuilder
                <> withOutput
                    ( withTxOutAddress baseAddrWithStake
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                    )
            )

-- §14.1: DelegateSeize whose seize redeemer names a DIFFERENT node
-- (directoryNodeIdx /= nodeRefIdx) — a seize of policy B cannot authorize a mint
-- of policy A. FAILS.
unit_delegateSeize_wrong_node_rejected :: Assertion
unit_delegateSeize_wrong_node_rejected =
    assertScriptFails $
        buildBalancedScriptContext
            ( withRedeemer (delegateSeizeRedeemer 1)
                <> withMintingScript (mintValue 1) (delegateSeizeRedeemer 1)
                <> withWithdrawal (ScriptCredential mintingLogicHash) 0
                <> withRewardingScriptWitness (PlutusTx.toBuiltinData (SeizeAct 7 [0] 0 1 0)) seizeCred 0
                <> paramsRefBuilder
                <> registryNodeRefBuilder
                <> withOutput
                    ( withTxOutAddress baseAddrWithStake
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                    )
            )

-- Minting-logic withdrawal index and global withdrawal index (test-driven; see
-- 'mlWdrlIdx'/'globalWdrlIdx' below). DelegateSeize {wdrlIdx, paramsRefIdx=0,
-- nodeRefIdx=1, seizeRedeemerIdx}.
delegateSeizeRedeemer :: Integer -> BuiltinData
delegateSeizeRedeemer nodeRefIdx =
    PlutusTx.toBuiltinData (DelegateSeize mlWdrlIdx 0 nodeRefIdx seizeRedeemerIdx)

mlWdrlIdx :: Integer
mlWdrlIdx = 0

globalWdrlIdx :: Integer
globalWdrlIdx = 1

seizeRedeemerIdx :: Integer
seizeRedeemerIdx = 1
