{-# LANGUAGE OverloadedStrings #-}

module ProgrammableTokens.Test.ProgrammableLogicGlobal (
    tests,
) where

import Data.ByteString qualified as BS
import Data.Either (isLeft, isRight)
import Data.List (elemIndex, nub, sortBy)
import Data.Word (Word8)
import Plutarch.Builtin.Integer (pconstantInteger)
import Plutarch.Core.Context (pscriptContextTxInfo)
import Plutarch.Evaluate (applyArguments, evalScript)
import Plutarch.Internal.Term (Config (NoTracing), Script, Term, compile)
import Plutarch.LedgerApi.V3 qualified as PlutarchV3
import Plutarch.Prelude (pconstant, perror, pfromData, pif, plam, pmatch)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Value (assetClass, assetClassValue)
import PlutusLedgerApi.V3
import PlutusTx qualified
import ProgrammableTokens.Test.ScriptContext.Builder (
    ScriptContextBuilder,
    buildBalancedScriptContext,
    buildLedgerShapedScriptContext,
    buildScriptContext,
    compareCredentialLedger,
    mkAdaValue,
    withAddress,
    withInlineDatum,
    withInput,
    withMint,
    withOutRef,
    withOutput,
    withReferenceInput,
    withReferenceScript,
    withRewardingScript,
    withScriptInput,
    withSpendingScript,
    withSigner,
    withTxOutAddress,
    withTxOutInlineDatum,
    withTxOutReferenceScript,
    withTxOutValue,
    withValue,
    withWithdrawal,
 )
import SmartTokens.Contracts.ProgrammableLogicBase (
    MintProof (Member, NonMember),
    ProgrammableLogicGlobalRedeemer (TransferAct),
    mkProgrammableLogicGlobal,
    mkProgrammableSeize,
    mkSeizeActRedeemerFromRelativeInputIdxs,
    poutputsContainExpectedValueAtCred,
 )
import SmartTokens.LinkedList.SpendDirectory (pmkDirectorySpending)
import SmartTokens.Types.Constants (protocolParamsToken)
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (DirectorySetNode))
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (ProgrammableLogicGlobalParams))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup
        "ProgrammableLogicGlobal unit/property tests"
        [ testCase "unit_seizeAct_complete_indices_succeeds" unit_seizeAct_complete_indices_succeeds
        , testCase "unit_seizeAct_leading_pubkey_input_skipped_succeeds" unit_seizeAct_leading_pubkey_input_skipped_succeeds
        , testCase "unit_seizeAct_omitted_index_rejected" unit_seizeAct_omitted_index_rejected
        , testCase "unit_seizeAct_datum_mismatch_rejected" unit_seizeAct_datum_mismatch_rejected
        , testCase "unit_seizeAct_reference_script_mismatch_rejected" unit_seizeAct_reference_script_mismatch_rejected
        , testCase "unit_seizeAct_burn_offsets_delta_succeeds" unit_seizeAct_burn_offsets_delta_succeeds
        , testCase "unit_seizeAct_mint_with_containment_succeeds" unit_seizeAct_mint_with_containment_succeeds
        , testCase "unit_seizeAct_mint_smuggle_rejected" unit_seizeAct_mint_smuggle_rejected
        , testCase "unit_transferAct_burn_with_mint_proof_succeeds" unit_transferAct_burn_with_mint_proof_succeeds
        , testCase "unit_transferAct_burn_without_mint_proof_rejected" unit_transferAct_burn_without_mint_proof_rejected
        , testCase "unit_transferAct_wrong_transfer_wdrl_index_rejected" unit_transferAct_wrong_transfer_wdrl_index_rejected
        , testCase "unit_transferAct_escape_to_pubkey_rejected" unit_transferAct_escape_to_pubkey_rejected
        , testCase "unit_transferAct_pubkey_owner_empty_signatories_rejected" unit_transferAct_pubkey_owner_empty_signatories_rejected
        , testCase "unit_transferAct_two_policies_wholesale_succeeds" unit_transferAct_two_policies_wholesale_succeeds
        , testCase "unit_transferAct_two_policies_partial_escape_rejected" unit_transferAct_two_policies_partial_escape_rejected
        , testCase "unit_transferAct_two_policies_mint_containment_succeeds" unit_transferAct_two_policies_mint_containment_succeeds
        , testCase "unit_transferAct_two_policies_mint_smuggle_rejected" unit_transferAct_two_policies_mint_smuggle_rejected
        , testCase "unit_transferAct_mint_smuggle_rejected" unit_transferAct_mint_smuggle_rejected
        , testCase "unit_transferAct_mint_with_proof_and_containment_succeeds" unit_transferAct_mint_with_proof_and_containment_succeeds
        , testCase "unit_transferAct_mint_without_mint_proof_rejected" unit_transferAct_mint_without_mint_proof_rejected
        , testCase "unit_seizeAct_escape_to_pubkey_rejected" unit_seizeAct_escape_to_pubkey_rejected
        , testCase "unit_seizeAct_full_seizure_to_pubkey_rejected" unit_seizeAct_full_seizure_to_pubkey_rejected
        , testCase "unit_seizeAct_partial_name_seizure_to_pubkey_rejected" unit_seizeAct_partial_name_seizure_to_pubkey_rejected
        , testCase "unit_seizeAct_full_seizure_to_base_output_succeeds" unit_seizeAct_full_seizure_to_base_output_succeeds
        , testCase "unit_seizeAct_input_without_seized_policy_rejected" unit_seizeAct_input_without_seized_policy_rejected
        , testCase "unit_seizeAct_paired_output_stake_rewrite_rejected" unit_seizeAct_paired_output_stake_rewrite_rejected
        , testCase "unit_seizeAct_non_seized_policy_drained_rejected" unit_seizeAct_non_seized_policy_drained_rejected
        , testCase "unit_seizeAct_non_seized_policy_injected_rejected" unit_seizeAct_non_seized_policy_injected_rejected
        , testCase "unit_seizeAct_paired_output_ada_reduced_rejected" unit_seizeAct_paired_output_ada_reduced_rejected
        , testCase "unit_seizeAct_paired_output_ada_topped_up_succeeds" unit_seizeAct_paired_output_ada_topped_up_succeeds
        , testCase "unit_seizeAct_forged_params_ref_input_rejected" unit_seizeAct_forged_params_ref_input_rejected
        , testCase "unit_seizeAct_forged_directory_node_rejected" unit_seizeAct_forged_directory_node_rejected
        , testCase "unit_seizeAct_issuer_logic_not_invoked_rejected" unit_seizeAct_issuer_logic_not_invoked_rejected
        , testCase "unit_seizeAct_issuer_wdrl_index_wrong_credential_rejected" unit_seizeAct_issuer_wdrl_index_wrong_credential_rejected
        , testCase "unit_seizeAct_structural_pair_control_succeeds" unit_seizeAct_structural_pair_control_succeeds
        , testCase "unit_registrySpend_minting_own_key_rejected" unit_registrySpend_minting_own_key_rejected
        , testCase "unit_registrySpend_without_own_key_mint_succeeds" unit_registrySpend_without_own_key_mint_succeeds
        , testCase "unit_outputsContain_single_asset_split_across_prog_outputs_succeeds" unit_outputsContain_single_asset_split_across_prog_outputs_succeeds
        , testCase "unit_outputsContain_single_asset_pubkey_output_ignored" unit_outputsContain_single_asset_pubkey_output_ignored
        , testCase "unit_outputsContain_multi_asset_succeeds" unit_outputsContain_multi_asset_succeeds
        , testCase "unit_outputsContain_multi_asset_shortfall_rejected" unit_outputsContain_multi_asset_shortfall_rejected
        , testCase "unit_transferAct_sweep_other_owner_unsigned_rejected" unit_transferAct_sweep_other_owner_unsigned_rejected
        , testCase "unit_transferAct_sweep_other_owner_decoy_signature_rejected" unit_transferAct_sweep_other_owner_decoy_signature_rejected
        , testCase "unit_transferAct_all_owners_signed_succeeds" unit_transferAct_all_owners_signed_succeeds
        , testCase "unit_transferAct_script_owner_not_invoked_rejected" unit_transferAct_script_owner_not_invoked_rejected
        , testCase "unit_transferAct_script_owner_invoked_succeeds" unit_transferAct_script_owner_invoked_succeeds
        , testCase "unit_transferAct_owner_witness_points_at_another_invoked_script_rejected" unit_transferAct_owner_witness_points_at_another_invoked_script_rejected
        , testCase "unit_transferAct_owner_witness_points_at_wrong_entry_rejected" unit_transferAct_owner_witness_points_at_wrong_entry_rejected
        , testCase "unit_transferAct_two_script_owners_witnesses_swapped_rejected" unit_transferAct_two_script_owners_witnesses_swapped_rejected
        , testCase "unit_transferAct_two_script_owners_succeeds" unit_transferAct_two_script_owners_succeeds
        , testCase "unit_transferAct_pubkey_input_outside_mini_ledger_ignored" unit_transferAct_pubkey_input_outside_mini_ledger_ignored
        , testCase "unit_transferAct_unstaked_mini_ledger_input_rejected" unit_transferAct_unstaked_mini_ledger_input_rejected
        , testProperty "prop_seizeAct_complete_indices_succeeds" prop_seizeAct_complete_indices_succeeds
        , testProperty "prop_seizeAct_omitted_index_rejected" prop_seizeAct_omitted_index_rejected
        ]

unit_seizeAct_complete_indices_succeeds :: Assertion
unit_seizeAct_complete_indices_succeeds =
    assertSeizeSucceeds $
        mkGlobalSeizeCtx 3 [0, 0, 0]

-- | Item 2 (walk all inputs): a leading pubkey (fee) input is skipped, not an
-- error. The seize now walks every input and classifies by credential, so
-- ordinary funding inputs coexist with the seized programmable inputs.
unit_seizeAct_leading_pubkey_input_skipped_succeeds :: Assertion
unit_seizeAct_leading_pubkey_input_skipped_succeeds =
    assertSeizeSucceeds $
        mkGlobalSeizeCtxWithLeadingPubKey 1 [0]

unit_seizeAct_omitted_index_rejected :: Assertion
unit_seizeAct_omitted_index_rejected =
    assertSeizeFails $
        mkGlobalSeizeCtx 3 [0, 0]

unit_seizeAct_datum_mismatch_rejected :: Assertion
unit_seizeAct_datum_mismatch_rejected =
    assertSeizeFails mkGlobalSeizeDatumMismatchCtx

unit_seizeAct_reference_script_mismatch_rejected :: Assertion
unit_seizeAct_reference_script_mismatch_rejected =
    assertSeizeFails mkGlobalSeizeReferenceScriptMismatchCtx

unit_seizeAct_burn_offsets_delta_succeeds :: Assertion
unit_seizeAct_burn_offsets_delta_succeeds =
    assertSeizeSucceeds mkGlobalSeizeBurnCtx

unit_seizeAct_mint_with_containment_succeeds :: Assertion
unit_seizeAct_mint_with_containment_succeeds =
    assertSeizeSucceeds mkGlobalSeizeMintContainedCtx

unit_seizeAct_mint_smuggle_rejected :: Assertion
unit_seizeAct_mint_smuggle_rejected =
    assertSeizeFails mkGlobalSeizeMintEscapeCtx

unit_transferAct_burn_with_mint_proof_succeeds :: Assertion
unit_transferAct_burn_with_mint_proof_succeeds =
    assertScriptSucceeds $
        mkGlobalTransferMintCtx
            (TransferAct [1] [1] [] [Member] 0)
            (-1)
            0

unit_transferAct_burn_without_mint_proof_rejected :: Assertion
unit_transferAct_burn_without_mint_proof_rejected =
    assertScriptFails $
        mkGlobalTransferMintCtx
            (TransferAct [1] [1] [] [] 0)
            (-1)
            0

-- Scan-proofness witness: the per-proof withdrawal index must point at the
-- policy's transfer-logic withdrawal. Index 0 resolves to the global
-- validator's own withdrawal (also the cached first entry), so both the cache
-- and the indexed check miss and the transfer must be rejected.
unit_transferAct_wrong_transfer_wdrl_index_rejected :: Assertion
unit_transferAct_wrong_transfer_wdrl_index_rejected =
    assertScriptFails $
        mkGlobalTransferMintCtx
            (TransferAct [1] [0] [] [Member] 0)
            1
            1


-- ---------------------------------------------------------------------------
-- Mini-ledger ownership.
--
-- Every programmable UTxO shares ONE payment credential -- the base script --
-- and carries its owner in the STAKING credential. The containment check the
-- transfer path ends with only groups outputs by payment credential, so as far
-- as containment is concerned Alice's wallet and Bob's wallet are the same
-- place: a transaction that consumes Bob's UTxO and puts the tokens in Alice's
-- wallet satisfies it perfectly.
--
-- The ONLY thing standing between Bob and that transaction is the owner witness
-- 'pvalueFromCred' demands of every contributing input: a signature if the owner
-- is a pubkey, an invocation of the owner's script if it is a script. These
-- tests are that check's coverage.
-- ---------------------------------------------------------------------------

{- | Alice consumes her own UTxO and Bob's, and consolidates both into her own
wallet, signing only for herself. This is the plain theft case and it must be
rejected -- containment cannot see it.
-}
unit_transferAct_sweep_other_owner_unsigned_rejected :: Assertion
unit_transferAct_sweep_other_owner_unsigned_rejected =
    assertScriptFails $ mkGlobalTransferTwoOwnersCtx [signerPkh]

{- | The same sweep carrying an extra signature from a key that is not Bob's.
Guards the direction where the witness check degenerates into "the transaction
is signed by somebody" rather than "signed by the owner of THIS input".
-}
unit_transferAct_sweep_other_owner_decoy_signature_rejected :: Assertion
unit_transferAct_sweep_other_owner_decoy_signature_rejected =
    assertScriptFails $ mkGlobalTransferTwoOwnersCtx [signerPkh, decoyPkh]

{- | Control: the identical consolidation is legitimate once Bob signs too, so
the rejections above are about the missing witness and not about consolidating
two owners' UTxOs at all.
-}
unit_transferAct_all_owners_signed_succeeds :: Assertion
unit_transferAct_all_owners_signed_succeeds =
    assertScriptSucceeds $ mkGlobalTransferTwoOwnersCtx [signerPkh, ownerBPkh]

{- | Script-owned mini-ledger UTxO (a vault, a DEX pool) spent without invoking
the owning script. A pubkey owner is protected by a signature; a script owner is
protected only by its script actually running, so this is the same theft as
above against a smart-contract holder.
-}
unit_transferAct_script_owner_not_invoked_rejected :: Assertion
unit_transferAct_script_owner_not_invoked_rejected =
    assertScriptFails $ mkGlobalTransferScriptOwnerCtx False

-- | Control: invoking the owning script authorizes the same spend.
unit_transferAct_script_owner_invoked_succeeds :: Assertion
unit_transferAct_script_owner_invoked_succeeds =
    assertScriptSucceeds $ mkGlobalTransferScriptOwnerCtx True

{- | The owner witness names WHERE the owner's withdrawal sits. Pointing it at a
different script that the transaction does invoke -- here the token's transfer
logic, which any transfer carries anyway -- must not satisfy the owner check.
Without this the witness would degenerate into "some withdrawal exists at this
index", and a vault-owned UTxO could be spent by anyone willing to include an
unrelated withdrawal.
-}
unit_transferAct_owner_witness_points_at_another_invoked_script_rejected :: Assertion
unit_transferAct_owner_witness_points_at_another_invoked_script_rejected =
    assertScriptFails $
        mkGlobalTransferScriptOwnerCtxWitnessed False [wdrlIndexOf [globalCred, transferCred] transferCred]

{- | The owning script IS invoked, but the witness points at the global
validator's entry instead of the owner's. A wrong index must fail rather than
be tolerated because the right withdrawal happens to be present somewhere.
-}
unit_transferAct_owner_witness_points_at_wrong_entry_rejected :: Assertion
unit_transferAct_owner_witness_points_at_wrong_entry_rejected =
    assertScriptFails $
        mkGlobalTransferScriptOwnerCtxWitnessed True [wdrlIndexOf scriptOwnerWdrls globalCred]

{- | Two mini-ledger inputs owned by DIFFERENT scripts. The witness list is
positional over the script-owned inputs in input order, so swapping the two
entries points each input at the other's owner and must be rejected -- that is
what stops one invoked owner from covering every script-owned input in the
transaction.
-}
unit_transferAct_two_script_owners_witnesses_swapped_rejected :: Assertion
unit_transferAct_two_script_owners_witnesses_swapped_rejected =
    assertScriptFails $ mkGlobalTransferTwoScriptOwnersCtx True

-- | Control: the same transaction with the witnesses in the correct order.
unit_transferAct_two_script_owners_succeeds :: Assertion
unit_transferAct_two_script_owners_succeeds =
    assertScriptSucceeds $ mkGlobalTransferTwoScriptOwnersCtx False

{- | An ordinary pubkey input -- a fee input, here also carrying programmable
tokens that already live OUTSIDE the mini-ledger -- must be ignored, not folded
into the value the transfer is required to keep at the base credential. Every
real transaction has a fee input, so treating non-base inputs as contributing
would make the transfer path unusable; and tokens already outside the
mini-ledger are not this validator's business.
-}
unit_transferAct_pubkey_input_outside_mini_ledger_ignored :: Assertion
unit_transferAct_pubkey_input_outside_mini_ledger_ignored =
    assertScriptSucceeds mkGlobalTransferWithPubKeyInputCtx

{- | A UTxO sitting at the base credential with NO staking credential has no
owner, so there is no witness anyone could supply for it. It must be rejected
rather than treated as ownerless-and-therefore-free: the whole mini-ledger
shares one payment credential, so an unstaked UTxO that validated would be
spendable by anybody.
-}
unit_transferAct_unstaked_mini_ledger_input_rejected :: Assertion
unit_transferAct_unstaked_mini_ledger_input_rejected =
    assertScriptFails mkGlobalTransferUnstakedInputCtx

unit_transferAct_escape_to_pubkey_rejected :: Assertion
unit_transferAct_escape_to_pubkey_rejected =
    assertScriptFails mkGlobalTransferEscapeCtx

-- Regression guard: the transfer-proof walk cons-builds its multi-policy
-- accumulator, which once left the expected value in DESCENDING order and made
-- the (order-sensitive) containment walk reject valid two-policy transfers.
{- | A pubkey-owned mini-ledger input in a transaction carrying NO signatories.
The owner check walks the signatory list and must reject an empty one; the
otherwise-identical signed transaction is
'unit_transferAct_two_policies_wholesale_succeeds'. Without this, forcing the
empty-list branch of the owner check to accept survives every suite.
-}
unit_transferAct_pubkey_owner_empty_signatories_rejected :: Assertion
unit_transferAct_pubkey_owner_empty_signatories_rejected =
    assertScriptFails $
        mkGlobalTransferTwoPoliciesCtx'
            False
            (TransferAct [1, 2] [1, 1] [] [] 0)
            []
            (mkValue [(programmableTransferCS, TokenName "0c", 3), (programmableTransferCS2, TokenName "1c", 5)])

unit_transferAct_two_policies_wholesale_succeeds :: Assertion
unit_transferAct_two_policies_wholesale_succeeds =
    assertScriptSucceeds $
        mkGlobalTransferTwoPoliciesCtx
            (TransferAct [1, 2] [1, 1] [] [] 0)
            []
            ( mkValue
                [ (programmableTransferCS, TokenName "0c", 3)
                , (programmableTransferCS2, TokenName "1c", 5)
                ]
            )

unit_transferAct_two_policies_partial_escape_rejected :: Assertion
unit_transferAct_two_policies_partial_escape_rejected =
    assertScriptFails $
        mkGlobalTransferTwoPoliciesCtx
            (TransferAct [1, 2] [1, 1] [] [] 0)
            []
            ( mkValue
                [ (programmableTransferCS, TokenName "0c", 3)
                , (programmableTransferCS2, TokenName "1c", 4)
                ]
            )

-- Regression guard for the mint-delta union: with a mis-ordered accumulator the
-- sorted merge of transfer value and mint delta would mis-sum per-policy
-- requirements for multi-policy transfers that also mint.
unit_transferAct_two_policies_mint_containment_succeeds :: Assertion
unit_transferAct_two_policies_mint_containment_succeeds =
    assertScriptSucceeds $
        mkGlobalTransferTwoPoliciesCtx
            (TransferAct [1, 2] [1, 1] [] [Member] 0)
            [(programmableTransferCS2, TokenName "1c", 2)]
            ( mkValue
                [ (programmableTransferCS, TokenName "0c", 3)
                , (programmableTransferCS2, TokenName "1c", 7)
                ]
            )

unit_transferAct_two_policies_mint_smuggle_rejected :: Assertion
unit_transferAct_two_policies_mint_smuggle_rejected =
    assertScriptFails $
        mkGlobalTransferTwoPoliciesCtx
            (TransferAct [1, 2] [1, 1] [] [Member] 0)
            [(programmableTransferCS2, TokenName "1c", 2)]
            ( mkValue
                [ (programmableTransferCS, TokenName "0c", 3)
                , (programmableTransferCS2, TokenName "1c", 5)
                ]
            )

unit_transferAct_mint_smuggle_rejected :: Assertion
unit_transferAct_mint_smuggle_rejected =
    assertScriptFails $
        mkGlobalTransferMintCtx
            (TransferAct [1] [1] [] [Member] 0)
            1
            1

unit_transferAct_mint_with_proof_and_containment_succeeds :: Assertion
unit_transferAct_mint_with_proof_and_containment_succeeds =
    assertScriptSucceeds $
        mkGlobalTransferMintCtx
            (TransferAct [1] [1] [] [Member] 0)
            1
            2

unit_transferAct_mint_without_mint_proof_rejected :: Assertion
unit_transferAct_mint_without_mint_proof_rejected =
    assertScriptFails $
        mkGlobalTransferMintCtx
            (TransferAct [1] [1] [] [] 0)
            1
            2

unit_seizeAct_escape_to_pubkey_rejected :: Assertion
unit_seizeAct_escape_to_pubkey_rejected =
    assertSeizeFails mkGlobalSeizeDirectEscapeCtx

-- | Item 1 (goOuter nil-branch drop): fully seizing a policy from a base input
-- (paired output loses the progCS entry entirely) while the tokens reappear at a
-- pubkey output must be rejected. The paired base output holds only ADA, so
-- conservation across base outputs cannot cover the +5 delta.
unit_seizeAct_full_seizure_to_pubkey_rejected :: Assertion
unit_seizeAct_full_seizure_to_pubkey_rejected =
    assertSeizeFails mkGlobalSeizeFullToPubKeyCtx

-- | Item 1 (psubtractTokens nil-branch drop): seizing a single token-name out of
-- a multi-name progCS holding, sending it to a pubkey output, must be rejected.
unit_seizeAct_partial_name_seizure_to_pubkey_rejected :: Assertion
unit_seizeAct_partial_name_seizure_to_pubkey_rejected =
    assertSeizeFails mkGlobalSeizePartialNameToPubKeyCtx

-- | Positive control: a legitimate full seizure that relocates the tokens to
-- another base (progLogicCred) output must still succeed after the fix.
unit_seizeAct_full_seizure_to_base_output_succeeds :: Assertion
unit_seizeAct_full_seizure_to_base_output_succeeds =
    assertSeizeSucceeds mkGlobalSeizeFullToBaseCtx

-- | Item 3 / Aiken Finding 12: seizing from a base input that does not hold the
-- seized policy (here ADA-only) must be rejected — the issuer cannot pair, and
-- thereby contaminate, a UTxO that never held the seized token.
unit_seizeAct_input_without_seized_policy_rejected :: Assertion
unit_seizeAct_input_without_seized_policy_rejected =
    assertSeizeFails mkGlobalSeizeNoProgCSInputCtx

-- | Item 7 / Aiken R-01: while a registry node is being spent, the transaction
-- must not mint or burn that node's own programmable-token policy (its key).
unit_registrySpend_minting_own_key_rejected :: Assertion
unit_registrySpend_minting_own_key_rejected =
    assertBool
        "expected registry spend to reject minting the spent node's own key"
        (not (dirSpendSucceeds (mkDirSpendCtx True)))

unit_registrySpend_without_own_key_mint_succeeds :: Assertion
unit_registrySpend_without_own_key_mint_succeeds =
    assertBool
        "expected registry spend to succeed when the spent node's own key is not minted"
        (dirSpendSucceeds (mkDirSpendCtx False))

directorySpendScript :: Script
directorySpendScript = compileNoTracing pmkDirectorySpending

dirSpendSucceeds :: ScriptContext -> Bool
dirSpendSucceeds ctx =
    let (res, _budget, _logs) = evalScript (applyArguments directorySpendScript [PlutusTx.toData protocolParamsCS, PlutusTx.toData ctx])
     in isRight res

dirNodeSpendAddr :: Address
dirNodeSpendAddr = Address (ScriptCredential (ScriptHash (bs28 0x44))) Nothing

-- | Spend the covering directory node (datum key = programmableTransferCS) during
-- an insert (directory NFT minted). If @mintOwnKey@, the transaction also mints
-- the node's own key — which the R-01 guard must reject.
mkDirSpendCtx :: Bool -> ScriptContext
mkDirSpendCtx mintOwnKey =
    buildBalancedScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData ())
            ( withOutRef (TxOutRef "dd00" 0)
                <> withAddress dirNodeSpendAddr
                <> withValue (mkAdaValue 2_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                <> withInlineDatum (PlutusTx.toBuiltinData directoryProgrammableNode)
            )
            <> withMint (mkValue [(directoryNodeCS, TokenName "0d", 1)]) (PlutusTx.toBuiltinData ())
            <> ( if mintOwnKey
                    then withMint (mkValue [(programmableTransferCS, TokenName "0c", 1)]) (PlutusTx.toBuiltinData ())
                    else mempty
               )
            <> withRefInputDatumValue
                paramRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                (PlutusTx.toBuiltinData protocolParamsDatum)
        )

unit_outputsContain_single_asset_split_across_prog_outputs_succeeds :: Assertion
unit_outputsContain_single_asset_split_across_prog_outputs_succeeds =
    assertOutputsContainSucceeds
        progLogicBaseCred
        [ txOutAt progWalletA (mkValue [(programmableTransferCS, TokenName "0c", 2)])
        , txOutAt progWalletB (mkValue [(programmableTransferCS, TokenName "0c", 3)])
        , txOutAt (pubKeyAddress signerPkh) (mkValue [(programmableTransferCS, TokenName "0c", 50)])
        ]
        (mkValue [(programmableTransferCS, TokenName "0c", 5)])

unit_outputsContain_single_asset_pubkey_output_ignored :: Assertion
unit_outputsContain_single_asset_pubkey_output_ignored =
    assertOutputsContainFails
        progLogicBaseCred
        [ txOutAt progWalletA (mkValue [(programmableTransferCS, TokenName "0c", 4)])
        , txOutAt (pubKeyAddress signerPkh) (mkValue [(programmableTransferCS, TokenName "0c", 100)])
        ]
        (mkValue [(programmableTransferCS, TokenName "0c", 5)])

unit_outputsContain_multi_asset_succeeds :: Assertion
unit_outputsContain_multi_asset_succeeds =
    assertOutputsContainSucceeds
        progLogicBaseCred
        [ txOutAt progWalletA (mkValue [(programmableTransferCS, TokenName "0c", 2), (programmableTransferCS, TokenName "0d", 1)])
        , txOutAt progWalletB (mkValue [(programmableTransferCS, TokenName "0c", 3), (programmableTransferCS, TokenName "0d", 4)])
        , txOutAt (pubKeyAddress signerPkh) (mkValue [(programmableTransferCS, TokenName "0d", 100)])
        ]
        (mkValue [(programmableTransferCS, TokenName "0c", 5), (programmableTransferCS, TokenName "0d", 5)])

unit_outputsContain_multi_asset_shortfall_rejected :: Assertion
unit_outputsContain_multi_asset_shortfall_rejected =
    assertOutputsContainFails
        progLogicBaseCred
        [ txOutAt progWalletA (mkValue [(programmableTransferCS, TokenName "0c", 2), (programmableTransferCS, TokenName "0d", 1)])
        , txOutAt progWalletB (mkValue [(programmableTransferCS, TokenName "0c", 3), (programmableTransferCS, TokenName "0d", 3)])
        , txOutAt (pubKeyAddress signerPkh) (mkValue [(programmableTransferCS, TokenName "0d", 100)])
        ]
        (mkValue [(programmableTransferCS, TokenName "0c", 5), (programmableTransferCS, TokenName "0d", 5)])

prop_seizeAct_complete_indices_succeeds :: QC.Property
prop_seizeAct_complete_indices_succeeds =
    QC.forAll (QC.chooseInt (1, 12)) $ \nInt ->
        let n = fromIntegral nInt
            idxs = replicate nInt 0
         in QC.counterexample ("n=" <> show n <> ", idxs=" <> show idxs) $
                seizeSucceeds (mkGlobalSeizeCtx n idxs) QC.=== True

prop_seizeAct_omitted_index_rejected :: QC.Property
prop_seizeAct_omitted_index_rejected =
    QC.forAll (QC.chooseInt (2, 12)) $ \nInt ->
        let n = fromIntegral nInt
            idxs = replicate (nInt - 1) 0
         in QC.counterexample ("n=" <> show n <> ", idxs=" <> show idxs) $
                seizeFails (mkGlobalSeizeCtx n idxs) QC.=== True

assertScriptSucceeds :: ScriptContext -> Assertion
assertScriptSucceeds ctx =
    assertBool "expected successful evaluation" (scriptSucceeds ctx)

assertScriptFails :: ScriptContext -> Assertion
assertScriptFails ctx =
    assertBool "expected script failure" (scriptFails ctx)

assertOutputsContainSucceeds :: Credential -> [TxOut] -> Value -> Assertion
assertOutputsContainSucceeds cred outputs expected =
    assertBool "expected outputsContain helper to succeed" (outputsContainSucceeds cred outputs expected)

assertOutputsContainFails :: Credential -> [TxOut] -> Value -> Assertion
assertOutputsContainFails cred outputs expected =
    assertBool "expected outputsContain helper to fail" (not (outputsContainSucceeds cred outputs expected))

scriptSucceeds :: ScriptContext -> Bool
scriptSucceeds ctx =
    let (res, _budget, _logs) = evalScript (applyArguments globalScript [PlutusTx.toData protocolParamsCS, PlutusTx.toData ctx])
     in isRight res

scriptFails :: ScriptContext -> Bool
scriptFails ctx =
    let (res, _budget, _logs) = evalScript (applyArguments globalScript [PlutusTx.toData protocolParamsCS, PlutusTx.toData ctx])
     in isLeft res

-- | Seize logic now lives in the standalone `mkProgrammableSeize` validator; the
-- seize unit tests drive it directly.
seizeScript :: Script
seizeScript = compileNoTracing mkProgrammableSeize

seizeSucceeds :: ScriptContext -> Bool
seizeSucceeds ctx =
    let (res, _budget, _logs) = evalScript (applyArguments seizeScript [PlutusTx.toData protocolParamsCS, PlutusTx.toData ctx])
     in isRight res

seizeFails :: ScriptContext -> Bool
seizeFails ctx =
    let (res, _budget, _logs) = evalScript (applyArguments seizeScript [PlutusTx.toData protocolParamsCS, PlutusTx.toData ctx])
     in isLeft res

assertSeizeSucceeds :: ScriptContext -> Assertion
assertSeizeSucceeds ctx =
    assertBool "expected successful seize evaluation" (seizeSucceeds ctx)

assertSeizeFails :: ScriptContext -> Assertion
assertSeizeFails ctx =
    assertBool "expected seize script failure" (seizeFails ctx)

outputsContainSucceeds :: Credential -> [TxOut] -> Value -> Bool
outputsContainSucceeds cred outputs expected =
    let ctx =
            buildScriptContext $
                mconcat
                    [ withOutput
                        ( withTxOutAddress (txOutAddress txOut)
                            <> withTxOutValue (txOutValue txOut)
                        )
                    | txOut <- outputs
                    ]
        (res, _budget, _logs) =
            evalScript
                ( applyArguments
                    (mkOutputsContainScript cred expected)
                    [PlutusTx.toData ctx]
                )
     in isRight res

compileNoTracing :: (forall s. Term s a) -> Script
compileNoTracing term =
    either (error . ("compile failed: " <>) . show) id (compile NoTracing term)

globalScript :: Script
globalScript = compileNoTracing mkProgrammableLogicGlobal

mkOutputsContainScript :: Credential -> Value -> Script
mkOutputsContainScript cred expectedValue =
    compileNoTracing $
        plam $ \ctx ->
            pmatch (pscriptContextTxInfo ctx) $ \txInfo ->
                let expectedValueTerm =
                        punsafeCoerce $
                            pconstant @PlutarchV3.PRawValue expectedValue
                 in pif
                        ( poutputsContainExpectedValueAtCred
                            (pconstant cred)
                            (pfromData $ PlutarchV3.ptxInfo'outputs txInfo)
                            expectedValueTerm
                        )
                        (pconstantInteger 0)
                        perror

mkValue :: [(CurrencySymbol, TokenName, Integer)] -> Value
mkValue = foldMap (\(cs, tn, amount) -> assetClassValue (assetClass cs tn) amount)

bs28 :: Word8 -> BuiltinByteString
bs28 w = PV1.toBuiltin (BS.replicate 28 w)

protocolParamsCS :: CurrencySymbol
protocolParamsCS = CurrencySymbol (bs28 0x10)

directoryNodeCS :: CurrencySymbol
directoryNodeCS = CurrencySymbol (bs28 0x11)

progLogicBaseHash :: ScriptHash
progLogicBaseHash = ScriptHash (bs28 0x12)

globalScriptHash :: ScriptHash
globalScriptHash = ScriptHash (bs28 0x13)

globalCred :: Credential
globalCred = ScriptCredential globalScriptHash

seizeScriptHash :: ScriptHash
seizeScriptHash = ScriptHash (bs28 0x1c)

seizeCred :: Credential
seizeCred = ScriptCredential seizeScriptHash

progLogicBaseCred :: Credential
progLogicBaseCred = ScriptCredential progLogicBaseHash

issuerLogicHash :: ScriptHash
issuerLogicHash = ScriptHash (bs28 0x14)

issuerCred :: Credential
issuerCred = ScriptCredential issuerLogicHash

programmableTransferCS :: CurrencySymbol
programmableTransferCS = CurrencySymbol (bs28 0x1b)

-- | Second registered policy; sorts after 'programmableTransferCS' so
-- two-policy fixtures exercise the multi-policy (canonically ordered)
-- aggregation and containment paths end to end.
programmableTransferCS2 :: CurrencySymbol
programmableTransferCS2 = CurrencySymbol (bs28 0x1c)

signerPkh :: PubKeyHash
signerPkh = PubKeyHash (bs28 0x01)

paramRef :: TxOutRef
paramRef = TxOutRef "aa00" 0

dirNodeRef :: TxOutRef
dirNodeRef = TxOutRef "bb00" 0

-- | Sorts after 'dirNodeRef' (reference inputs are ordered by TxOutRef), so
-- with 'paramRef' at index 0 the two directory nodes sit at ref indices 1, 2.
dirNode2Ref :: TxOutRef
dirNode2Ref = TxOutRef "bb01" 0

scriptAddressWithSignerStake :: ScriptHash -> PubKeyHash -> Address
scriptAddressWithSignerStake sh pkh =
    Address (ScriptCredential sh) (Just (StakingHash (PubKeyCredential pkh)))

pubKeyAddress :: PubKeyHash -> Address
pubKeyAddress pkh = Address (PubKeyCredential pkh) Nothing

txOutAt :: Address -> Value -> TxOut
txOutAt addr assets =
    TxOut
        { txOutAddress = addr
        , txOutValue = mkAdaValue 3_000_000 <> assets
        , txOutDatum = NoOutputDatum
        , txOutReferenceScript = Nothing
        }

withRefInputDatumValue :: TxOutRef -> Address -> Value -> BuiltinData -> ScriptContextBuilder
withRefInputDatumValue ref addr value dat =
    withReferenceInput
        ( withOutRef ref
            <> withAddress addr
            <> withValue value
            <> withInlineDatum dat
        )

protocolParamsDatum :: ProgrammableLogicGlobalParams
protocolParamsDatum =
    ProgrammableLogicGlobalParams directoryNodeCS (ScriptCredential progLogicBaseHash) globalCred seizeCred

directoryProgrammableNode :: DirectorySetNode
directoryProgrammableNode =
    DirectorySetNode
        programmableTransferCS
        (CurrencySymbol (bs28 0xff))
        (ScriptCredential (ScriptHash (bs28 0x15)))
        issuerCred
        (CurrencySymbol "")

directoryProgrammableNode2 :: DirectorySetNode
directoryProgrammableNode2 =
    DirectorySetNode
        programmableTransferCS2
        (CurrencySymbol (bs28 0xff))
        (ScriptCredential (ScriptHash (bs28 0x15)))
        issuerCred
        (CurrencySymbol "")

transferLogicHash :: ScriptHash
transferLogicHash = ScriptHash (bs28 0x15)

transferCred :: Credential
transferCred = ScriptCredential transferLogicHash

seizeInputAddr :: Address
seizeInputAddr = scriptAddressWithSignerStake progLogicBaseHash signerPkh

progWalletA :: Address
progWalletA = scriptAddressWithSignerStake progLogicBaseHash signerPkh

progWalletB :: Address
progWalletB = scriptAddressWithSignerStake progLogicBaseHash (PubKeyHash (bs28 0x02))

metadataDatumA :: BuiltinData
metadataDatumA = PlutusTx.toBuiltinData (1 :: Integer)

metadataDatumB :: BuiltinData
metadataDatumB = PlutusTx.toBuiltinData (2 :: Integer)

metadataRefScriptA :: ScriptHash
metadataRefScriptA = ScriptHash (bs28 0x31)

metadataRefScriptB :: ScriptHash
metadataRefScriptB = ScriptHash (bs28 0x32)

seizeInputValue :: Value
seizeInputValue =
    mkAdaValue 3_000_000
        <> mkValue [(programmableTransferCS, TokenName "0c", 1)]

seizeInputBuilder :: Integer -> ScriptContextBuilder
seizeInputBuilder idx =
    withScriptInput
        (PlutusTx.toBuiltinData ())
        ( withOutRef (TxOutRef "5e12" idx)
            <> withAddress seizeInputAddr
            <> withValue seizeInputValue
        )

seizeCorrespondingOutputBuilder :: ScriptContextBuilder
seizeCorrespondingOutputBuilder =
    withOutput
        ( withTxOutAddress seizeInputAddr
            <> withTxOutValue seizeInputValue
        )

seizeInputWithMetadataBuilder :: BuiltinData -> ScriptHash -> ScriptContextBuilder
seizeInputWithMetadataBuilder datum refScript =
    withScriptInput
        (PlutusTx.toBuiltinData ())
        ( withOutRef (TxOutRef "5e99" 0)
            <> withAddress seizeInputAddr
            <> withValue seizeInputValue
            <> withInlineDatum datum
            <> withReferenceScript refScript
        )

seizeCorrespondingOutputWithMetadataBuilder :: BuiltinData -> ScriptHash -> ScriptContextBuilder
seizeCorrespondingOutputWithMetadataBuilder datum refScript =
    withOutput
        ( withTxOutAddress seizeInputAddr
            <> withTxOutValue seizeInputValue
            <> withTxOutInlineDatum datum
            <> withTxOutReferenceScript refScript
        )

transferInputRef :: TxOutRef
transferInputRef = TxOutRef "7a00" 0

mkGlobalTransferMintCtx :: ProgrammableLogicGlobalRedeemer -> Integer -> Integer -> ScriptContext
mkGlobalTransferMintCtx globalRedeemer mintedQty transferOutputQty =
    buildBalancedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData globalRedeemer)
            globalCred
            0
            <> withSigner signerPkh
            <> withWithdrawal transferCred 0
            <> withScriptInput
                (PlutusTx.toBuiltinData ())
                ( withOutRef transferInputRef
                    <> withAddress seizeInputAddr
                    <> withValue (mkAdaValue 10_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 1)])
                )
            <> withOutput
                ( withTxOutAddress seizeInputAddr
                    <> withTxOutValue (mkAdaValue 10_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", transferOutputQty)])
                )
            <> withMint (mkValue [(programmableTransferCS, TokenName "0c", mintedQty)]) (PlutusTx.toBuiltinData ())
            <> withRefInputDatumValue
                paramRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue
                dirNodeRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                (PlutusTx.toBuiltinData directoryProgrammableNode)
        )

-- | Two-policy transfer fixture: one mini-ledger input carrying BOTH registered
-- policies (3x "0c" under CS1, 5x "1c" under CS2), one mini-ledger output whose
-- programmable value is caller-chosen, optional mint, positional proofs [1, 2].
-- Exercises the multi-policy aggregation accumulator, its canonical ordering,
-- the mint-delta union, and the multi-asset containment walk END TO END —
-- the paths a single-policy fixture cannot reach.
mkGlobalTransferTwoPoliciesCtx :: ProgrammableLogicGlobalRedeemer -> [(CurrencySymbol, TokenName, Integer)] -> Value -> ScriptContext
mkGlobalTransferTwoPoliciesCtx = mkGlobalTransferTwoPoliciesCtx' True

-- | As 'mkGlobalTransferTwoPoliciesCtx', but the signatory list can be omitted
-- entirely. A mini-ledger input whose stake credential is a PUBKEY is
-- authorised only by that key's signature, so with no signatories at all the
-- spend must be rejected -- including when the signatory list is EMPTY rather
-- than merely wrong.
mkGlobalTransferTwoPoliciesCtx' :: Bool -> ProgrammableLogicGlobalRedeemer -> [(CurrencySymbol, TokenName, Integer)] -> Value -> ScriptContext
mkGlobalTransferTwoPoliciesCtx' signed globalRedeemer mintEntries progOutputVal =
    buildBalancedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData globalRedeemer)
            globalCred
            0
            <> (if signed then withSigner signerPkh else mempty)
            <> withWithdrawal transferCred 0
            <> withScriptInput
                (PlutusTx.toBuiltinData ())
                ( withOutRef transferInputRef
                    <> withAddress seizeInputAddr
                    <> withValue
                        ( mkAdaValue 10_000_000
                            <> mkValue
                                [ (programmableTransferCS, TokenName "0c", 3)
                                , (programmableTransferCS2, TokenName "1c", 5)
                                ]
                        )
                )
            <> withOutput
                ( withTxOutAddress seizeInputAddr
                    <> withTxOutValue (mkAdaValue 10_000_000 <> progOutputVal)
                )
            <> (if null mintEntries then mempty else withMint (mkValue mintEntries) (PlutusTx.toBuiltinData ()))
            <> withRefInputDatumValue
                paramRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue
                dirNodeRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                (PlutusTx.toBuiltinData directoryProgrammableNode)
            <> withRefInputDatumValue
                dirNode2Ref
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                (PlutusTx.toBuiltinData directoryProgrammableNode2)
        )


-- | A key that owns nothing in these fixtures.
decoyPkh :: PubKeyHash
decoyPkh = PubKeyHash (bs28 0x09)

-- | Owner of 'progWalletB'.
ownerBPkh :: PubKeyHash
ownerBPkh = PubKeyHash (bs28 0x02)

transferInputRefB :: TxOutRef
transferInputRefB = TxOutRef "7a01" 0

-- | A script that owns a mini-ledger wallet, e.g. a vault or pool holding
-- programmable tokens on behalf of its users.
vaultOwnerCred :: Credential
vaultOwnerCred = ScriptCredential (ScriptHash (bs28 0x21))

progWalletVault :: Address
progWalletVault =
    Address (ScriptCredential progLogicBaseHash) (Just (StakingHash vaultOwnerCred))

-- | Position of a credential in the withdrawal map as the LEDGER orders it.
-- These fixtures use 'buildLedgerShapedScriptContext', which canonicalises the
-- map, so the redeemer's withdrawal index has to be derived rather than
-- guessed.
wdrlIndexOf :: [Credential] -> Credential -> Integer
wdrlIndexOf creds target =
    case elemIndex target (sortBy compareCredentialLedger (nub creds)) of
        Just idx -> fromIntegral idx
        Nothing -> error ("wdrlIndexOf: credential not in the withdrawal set: " <> show target)

mkGlobalTransferRefInputs :: ScriptContextBuilder
mkGlobalTransferRefInputs =
    withRefInputDatumValue
        paramRef
        (pubKeyAddress signerPkh)
        (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
        (PlutusTx.toBuiltinData protocolParamsDatum)
        <> withRefInputDatumValue
            dirNodeRef
            (pubKeyAddress signerPkh)
            (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
            (PlutusTx.toBuiltinData directoryProgrammableNode)

{- | Two mini-ledger inputs owned by DIFFERENT parties, consolidated into a
single output at owner A's wallet. Which owners signed is the parameter.
-}
mkGlobalTransferTwoOwnersCtx :: [PubKeyHash] -> ScriptContext
mkGlobalTransferTwoOwnersCtx signers =
    buildLedgerShapedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData (TransferAct [1] [wdrlIndexOf twoOwnerWdrls transferCred] [] [] 0))
            globalCred
            0
            <> foldMap withSigner signers
            <> withWithdrawal transferCred 0
            <> withScriptInput
                (PlutusTx.toBuiltinData ())
                ( withOutRef transferInputRef
                    <> withAddress progWalletA
                    <> withValue (mkAdaValue 10_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 3)])
                )
            <> withScriptInput
                (PlutusTx.toBuiltinData ())
                ( withOutRef transferInputRefB
                    <> withAddress progWalletB
                    <> withValue (mkAdaValue 10_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 5)])
                )
            <> withOutput
                ( withTxOutAddress progWalletA
                    <> withTxOutValue (mkAdaValue 15_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 8)])
                )
            <> mkGlobalTransferRefInputs
        )
  where
    twoOwnerWdrls = [globalCred, transferCred]

{- | One mini-ledger input owned by a SCRIPT. The flag selects whether that
script is actually invoked in the transaction; the witness list is derived.
-}
mkGlobalTransferScriptOwnerCtx :: Bool -> ScriptContext
mkGlobalTransferScriptOwnerCtx ownerInvoked =
    mkGlobalTransferScriptOwnerCtxWitnessed
        ownerInvoked
        [wdrlIndexOf scriptOwnerWdrls vaultOwnerCred | ownerInvoked]

scriptOwnerWdrls :: [Credential]
scriptOwnerWdrls = [globalCred, transferCred, vaultOwnerCred]

-- | As above, but the owner witness list is supplied by the caller so a test
-- can point it somewhere it does not belong.
mkGlobalTransferScriptOwnerCtxWitnessed :: Bool -> [Integer] -> ScriptContext
mkGlobalTransferScriptOwnerCtxWitnessed ownerInvoked ownerIdxs =
    buildLedgerShapedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData (TransferAct [1] [wdrlIndexOf wdrls transferCred] ownerIdxs [] 0))
            globalCred
            0
            <> withSigner signerPkh
            <> withWithdrawal transferCred 0
            <> (if ownerInvoked then withWithdrawal vaultOwnerCred 0 else mempty)
            <> withScriptInput
                (PlutusTx.toBuiltinData ())
                ( withOutRef transferInputRef
                    <> withAddress progWalletVault
                    <> withValue (mkAdaValue 10_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 4)])
                )
            <> withOutput
                ( withTxOutAddress progWalletVault
                    <> withTxOutValue (mkAdaValue 8_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 4)])
                )
            <> mkGlobalTransferRefInputs
        )
  where
    wdrls = [globalCred, transferCred] <> [vaultOwnerCred | ownerInvoked]

-- | A second owning script, sorting after 'vaultOwnerCred'.
vaultOwnerCred2 :: Credential
vaultOwnerCred2 = ScriptCredential (ScriptHash (bs28 0x22))

progWalletVault2 :: Address
progWalletVault2 =
    Address (ScriptCredential progLogicBaseHash) (Just (StakingHash vaultOwnerCred2))

{- | Two mini-ledger inputs under DIFFERENT owning scripts, both invoked. The
flag swaps the two witness entries so each input points at the other's owner.
-}
mkGlobalTransferTwoScriptOwnersCtx :: Bool -> ScriptContext
mkGlobalTransferTwoScriptOwnersCtx swapWitnesses =
    buildLedgerShapedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData (TransferAct [1] [wdrlIndexOf wdrls transferCred] ownerIdxs [] 0))
            globalCred
            0
            <> withSigner signerPkh
            <> withWithdrawal transferCred 0
            <> withWithdrawal vaultOwnerCred 0
            <> withWithdrawal vaultOwnerCred2 0
            <> withScriptInput
                (PlutusTx.toBuiltinData ())
                ( withOutRef transferInputRef
                    <> withAddress progWalletVault
                    <> withValue (mkAdaValue 10_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 4)])
                )
            <> withScriptInput
                (PlutusTx.toBuiltinData ())
                ( withOutRef transferInputRefB
                    <> withAddress progWalletVault2
                    <> withValue (mkAdaValue 10_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 6)])
                )
            <> withOutput
                ( withTxOutAddress progWalletVault
                    <> withTxOutValue (mkAdaValue 15_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 10)])
                )
            <> mkGlobalTransferRefInputs
        )
  where
    wdrls = [globalCred, transferCred, vaultOwnerCred, vaultOwnerCred2]
    -- Inputs are ordered by TxOutRef: transferInputRef ("7a00") then
    -- transferInputRefB ("7a01"), so vault 1 then vault 2.
    correct = [wdrlIndexOf wdrls vaultOwnerCred, wdrlIndexOf wdrls vaultOwnerCred2]
    ownerIdxs = if swapWitnesses then reverse correct else correct

{- | One mini-ledger input plus an ordinary pubkey input that also happens to
hold the same policy outside the mini-ledger. Only the mini-ledger input's 3
tokens may be required to remain at the base credential.
-}
mkGlobalTransferWithPubKeyInputCtx :: ScriptContext
mkGlobalTransferWithPubKeyInputCtx =
    buildLedgerShapedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData (TransferAct [1] [wdrlIndexOf [globalCred, transferCred] transferCred] [] [] 0))
            globalCred
            0
            <> withSigner signerPkh
            <> withWithdrawal transferCred 0
            <> withScriptInput
                (PlutusTx.toBuiltinData ())
                ( withOutRef transferInputRef
                    <> withAddress progWalletA
                    <> withValue (mkAdaValue 10_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 3)])
                )
            <> withInput
                ( withOutRef transferInputRefB
                    <> withAddress (pubKeyAddress signerPkh)
                    <> withValue (mkAdaValue 10_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 4)])
                )
            <> withOutput
                ( withTxOutAddress progWalletA
                    <> withTxOutValue (mkAdaValue 8_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 3)])
                )
            <> withOutput
                ( withTxOutAddress (pubKeyAddress signerPkh)
                    <> withTxOutValue (mkAdaValue 8_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 4)])
                )
            <> mkGlobalTransferRefInputs
        )

mkGlobalTransferUnstakedInputCtx :: ScriptContext
mkGlobalTransferUnstakedInputCtx =
    buildLedgerShapedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData (TransferAct [1] [wdrlIndexOf [globalCred, transferCred] transferCred] [] [] 0))
            globalCred
            0
            <> withSigner signerPkh
            <> withWithdrawal transferCred 0
            <> withScriptInput
                (PlutusTx.toBuiltinData ())
                ( withOutRef transferInputRef
                    <> withAddress (Address progLogicBaseCred Nothing)
                    <> withValue (mkAdaValue 10_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 4)])
                )
            <> withOutput
                ( withTxOutAddress (Address progLogicBaseCred Nothing)
                    <> withTxOutValue (mkAdaValue 8_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 4)])
                )
            <> mkGlobalTransferRefInputs
        )

mkGlobalTransferEscapeCtx :: ScriptContext
mkGlobalTransferEscapeCtx =
    buildBalancedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData $ TransferAct [1] [1] [] [] 0)
            globalCred
            0
            <> withSigner signerPkh
            <> withWithdrawal transferCred 0
            <> withScriptInput
                (PlutusTx.toBuiltinData ())
                ( withOutRef transferInputRef
                    <> withAddress seizeInputAddr
                    <> withValue (mkAdaValue 10_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 1)])
                )
            <> withOutput
                ( withTxOutAddress (pubKeyAddress signerPkh)
                    <> withTxOutValue (mkAdaValue 10_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 1)])
                )
            <> withRefInputDatumValue
                paramRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue
                dirNodeRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                (PlutusTx.toBuiltinData directoryProgrammableNode)
        )

mkGlobalSeizeCtx :: Integer -> [Integer] -> ScriptContext
mkGlobalSeizeCtx inputCount providedIdxs =
    let expectedIdxs = [0 .. (inputCount - 1)]
        seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 providedIdxs 0 0 1
        seizeInputsBuilder = mconcat (map seizeInputBuilder expectedIdxs)
        correspondingOutputsBuilder = mconcat (replicate (length providedIdxs) seizeCorrespondingOutputBuilder)
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData seizeRedeemer)
                globalCred
                0
                <> withWithdrawal issuerCred 0
                <> seizeInputsBuilder
                <> correspondingOutputsBuilder
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    (PlutusTx.toBuiltinData protocolParamsDatum)
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (PlutusTx.toBuiltinData directoryProgrammableNode)
            )

mkGlobalSeizeCtxWithLeadingPubKey :: Integer -> [Integer] -> ScriptContext
mkGlobalSeizeCtxWithLeadingPubKey inputCount providedIdxs =
    let expectedIdxs = [0 .. (inputCount - 1)]
        seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 providedIdxs 0 0 1
        leadingPubKeyInput =
            withInput
                ( withOutRef (TxOutRef "0000" 0)
                    <> withAddress (pubKeyAddress signerPkh)
                    <> withValue (mkAdaValue 2_000_000)
                )
        seizeInputsBuilder = mconcat (map seizeInputBuilder expectedIdxs)
        correspondingOutputsBuilder = mconcat (replicate (length providedIdxs) seizeCorrespondingOutputBuilder)
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData seizeRedeemer)
                globalCred
                0
                <> withWithdrawal issuerCred 0
                <> leadingPubKeyInput
                <> seizeInputsBuilder
                <> correspondingOutputsBuilder
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    (PlutusTx.toBuiltinData protocolParamsDatum)
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (PlutusTx.toBuiltinData directoryProgrammableNode)
            )

mkGlobalSeizeDatumMismatchCtx :: ScriptContext
mkGlobalSeizeDatumMismatchCtx =
    let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0 0 1
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData seizeRedeemer)
                globalCred
                0
                <> withWithdrawal issuerCred 0
                <> seizeInputWithMetadataBuilder metadataDatumA metadataRefScriptA
                <> seizeCorrespondingOutputWithMetadataBuilder metadataDatumB metadataRefScriptA
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    (PlutusTx.toBuiltinData protocolParamsDatum)
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (PlutusTx.toBuiltinData directoryProgrammableNode)
            )

mkGlobalSeizeReferenceScriptMismatchCtx :: ScriptContext
mkGlobalSeizeReferenceScriptMismatchCtx =
    let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0 0 1
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData seizeRedeemer)
                globalCred
                0
                <> withWithdrawal issuerCred 0
                <> seizeInputWithMetadataBuilder metadataDatumA metadataRefScriptA
                <> seizeCorrespondingOutputWithMetadataBuilder metadataDatumA metadataRefScriptB
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    (PlutusTx.toBuiltinData protocolParamsDatum)
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (PlutusTx.toBuiltinData directoryProgrammableNode)
            )

mkGlobalSeizeBurnCtx :: ScriptContext
mkGlobalSeizeBurnCtx =
    let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0 0 1
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData seizeRedeemer)
                globalCred
                0
                <> withWithdrawal issuerCred 0
                <> seizeInputBuilder 0
                -- corresponding output removes 1 programmable token from the input
                -- and tx mint burns exactly 1, so net required residual is zero.
                <> withOutput
                    ( withTxOutAddress seizeInputAddr
                        <> withTxOutValue (mkAdaValue 3_000_000)
                    )
                <> withMint (mkValue [(programmableTransferCS, TokenName "0c", -1)]) (PlutusTx.toBuiltinData ())
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    (PlutusTx.toBuiltinData protocolParamsDatum)
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (PlutusTx.toBuiltinData directoryProgrammableNode)
            )

mkGlobalSeizeMintContainedCtx :: ScriptContext
mkGlobalSeizeMintContainedCtx =
    let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0 0 1
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData seizeRedeemer)
                globalCred
                0
                <> withWithdrawal issuerCred 0
                <> seizeInputBuilder 0
                -- output ordering is reversed by the builder; this yields
                -- [corresponding, residual] in the final tx outputs.
                <> withOutput
                    ( withTxOutAddress seizeInputAddr
                        <> withTxOutValue (mkValue [(programmableTransferCS, TokenName "0c", 1)])
                    )
                <> withOutput
                    ( withTxOutAddress seizeInputAddr
                        <> withTxOutValue seizeInputValue
                    )
                <> withMint (mkValue [(programmableTransferCS, TokenName "0c", 1)]) (PlutusTx.toBuiltinData ())
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    (PlutusTx.toBuiltinData protocolParamsDatum)
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (PlutusTx.toBuiltinData directoryProgrammableNode)
            )

mkGlobalSeizeMintEscapeCtx :: ScriptContext
mkGlobalSeizeMintEscapeCtx =
    let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 1 0 1
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData seizeRedeemer)
                globalCred
                0
                <> withWithdrawal issuerCred 0
                <> seizeInputBuilder 0
                -- output ordering is reversed by the builder; this yields
                -- [pubkey token output, corresponding] in the final tx outputs.
                <> withOutput
                    ( withTxOutAddress seizeInputAddr
                        <> withTxOutValue seizeInputValue
                    )
                <> withOutput
                    ( withTxOutAddress (pubKeyAddress signerPkh)
                        <> withTxOutValue (mkValue [(programmableTransferCS, TokenName "0c", 1)])
                    )
                <> withMint (mkValue [(programmableTransferCS, TokenName "0c", 1)]) (PlutusTx.toBuiltinData ())
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    (PlutusTx.toBuiltinData protocolParamsDatum)
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (PlutusTx.toBuiltinData directoryProgrammableNode)
            )

seizeInputValue5 :: Value
seizeInputValue5 =
    mkAdaValue 3_000_000
        <> mkValue [(programmableTransferCS, TokenName "0c", 5)]

seizeInputValueMultiName :: Value
seizeInputValueMultiName =
    mkAdaValue 3_000_000
        <> mkValue
            [ (programmableTransferCS, TokenName "0c", 5)
            , (programmableTransferCS, TokenName "0d", 3)
            ]

-- | Full seizure: paired base output loses the progCS entry entirely, tokens
-- reappear at a pubkey output. Builder reverses output order, so listing the
-- pubkey output first yields final order [paired, pubkey]; the paired output is
-- the head consumed by pairing, leaving no base residual to cover the delta.
mkGlobalSeizeFullToPubKeyCtx :: ScriptContext
mkGlobalSeizeFullToPubKeyCtx =
    let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0 0 1
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData seizeRedeemer)
                globalCred
                0
                <> withWithdrawal issuerCred 0
                <> withScriptInput
                    (PlutusTx.toBuiltinData ())
                    ( withOutRef (TxOutRef "5e12" 0)
                        <> withAddress seizeInputAddr
                        <> withValue seizeInputValue5
                    )
                <> withOutput
                    ( withTxOutAddress (pubKeyAddress signerPkh)
                        <> withTxOutValue (mkValue [(programmableTransferCS, TokenName "0c", 5)])
                    )
                <> withOutput
                    ( withTxOutAddress seizeInputAddr
                        <> withTxOutValue (mkAdaValue 3_000_000)
                    )
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    (PlutusTx.toBuiltinData protocolParamsDatum)
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (PlutusTx.toBuiltinData directoryProgrammableNode)
            )

-- | Partial-name seizure: input holds progCS {0c:5, 0d:3}; paired base output
-- keeps {0c:5} but drops 0d; the 0d token reappears at a pubkey output.
mkGlobalSeizePartialNameToPubKeyCtx :: ScriptContext
mkGlobalSeizePartialNameToPubKeyCtx =
    let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0 0 1
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData seizeRedeemer)
                globalCred
                0
                <> withWithdrawal issuerCred 0
                <> withScriptInput
                    (PlutusTx.toBuiltinData ())
                    ( withOutRef (TxOutRef "5e12" 0)
                        <> withAddress seizeInputAddr
                        <> withValue seizeInputValueMultiName
                    )
                <> withOutput
                    ( withTxOutAddress (pubKeyAddress signerPkh)
                        <> withTxOutValue (mkValue [(programmableTransferCS, TokenName "0d", 3)])
                    )
                <> withOutput
                    ( withTxOutAddress seizeInputAddr
                        <> withTxOutValue (mkAdaValue 3_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 5)])
                    )
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    (PlutusTx.toBuiltinData protocolParamsDatum)
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (PlutusTx.toBuiltinData directoryProgrammableNode)
            )

-- | Positive control: full seizure relocating tokens to another base output.
-- Final output order [paired(ADA only), residual(progCS:5 at base)]; go2 sums
-- the residual so conservation holds. Must succeed before and after the fix.
mkGlobalSeizeFullToBaseCtx :: ScriptContext
mkGlobalSeizeFullToBaseCtx =
    let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0 0 1
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData seizeRedeemer)
                globalCred
                0
                <> withWithdrawal issuerCred 0
                <> withScriptInput
                    (PlutusTx.toBuiltinData ())
                    ( withOutRef (TxOutRef "5e12" 0)
                        <> withAddress seizeInputAddr
                        <> withValue seizeInputValue5
                    )
                <> withOutput
                    ( withTxOutAddress seizeInputAddr
                        <> withTxOutValue (mkValue [(programmableTransferCS, TokenName "0c", 5)])
                    )
                <> withOutput
                    ( withTxOutAddress seizeInputAddr
                        <> withTxOutValue (mkAdaValue 3_000_000)
                    )
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    (PlutusTx.toBuiltinData protocolParamsDatum)
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (PlutusTx.toBuiltinData directoryProgrammableNode)
            )

-- | Seize input holds only ADA (no seized programmable token); the paired output
-- is identical. Conservation is trivially satisfied, but the non-contamination
-- guard must reject the pairing because the input holds none of the seized policy.
mkGlobalSeizeNoProgCSInputCtx :: ScriptContext
mkGlobalSeizeNoProgCSInputCtx =
    let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0 0 1
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData seizeRedeemer)
                globalCred
                0
                <> withWithdrawal issuerCred 0
                <> withScriptInput
                    (PlutusTx.toBuiltinData ())
                    ( withOutRef (TxOutRef "5e12" 0)
                        <> withAddress seizeInputAddr
                        <> withValue (mkAdaValue 3_000_000)
                    )
                <> withOutput
                    ( withTxOutAddress seizeInputAddr
                        <> withTxOutValue (mkAdaValue 3_000_000)
                    )
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    (PlutusTx.toBuiltinData protocolParamsDatum)
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (PlutusTx.toBuiltinData directoryProgrammableNode)
            )

mkGlobalSeizeDirectEscapeCtx :: ScriptContext
mkGlobalSeizeDirectEscapeCtx =
    let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0 0 1
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData seizeRedeemer)
                globalCred
                0
                <> withWithdrawal issuerCred 0
                <> seizeInputBuilder 0
                <> withOutput
                    ( withTxOutAddress (pubKeyAddress signerPkh)
                        <> withTxOutValue seizeInputValue
                    )
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    (PlutusTx.toBuiltinData protocolParamsDatum)
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (PlutusTx.toBuiltinData directoryProgrammableNode)
            )

-- ---------------------------------------------------------------------------
-- P0 seize red-tests.
--
-- Each invariant below is enforced by a single line of the seize path, and each
-- is easy to lose when the per-pair loop is rewritten. Before these existed the
-- suite could be passed in full by a validator that permits outright theft:
-- nothing varied the staking credential across a pair, nothing altered a
-- non-seized policy, nothing forged the params or directory-node reference
-- input, and nothing omitted the issuer withdrawal.
-- ---------------------------------------------------------------------------

-- | A policy that is NOT the seized one; it must survive a seizure untouched.
noiseCS :: CurrencySymbol
noiseCS = CurrencySymbol (bs28 0x77)

noiseValue :: Value
noiseValue = mkValue [(noiseCS, TokenName "n1", 7)]

seizeInputValueWithNoise :: Value
seizeInputValueWithNoise = seizeInputValue <> noiseValue

-- | Protocol params at reference index 0, seized policy's directory node at 1 —
-- the layout every seize redeemer in this module addresses.
seizeRefInputsBuilder :: ScriptContextBuilder
seizeRefInputsBuilder =
    withRefInputDatumValue
        paramRef
        (pubKeyAddress signerPkh)
        (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
        (PlutusTx.toBuiltinData protocolParamsDatum)
        <> withRefInputDatumValue
            dirNodeRef
            (pubKeyAddress signerPkh)
            (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
            (PlutusTx.toBuiltinData directoryProgrammableNode)

-- | One seized input paired with a caller-supplied continuing output, with the
-- standard withdrawals and reference inputs. @extra@ funds any value the paired
-- output holds beyond the input (used by the injection case).
mkSeizePairCtxWith :: ScriptContextBuilder -> Value -> Address -> Value -> ScriptContext
mkSeizePairCtxWith extra inputValue outAddr outValue =
    let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0 0 1
     in buildBalancedScriptContext
            ( withRewardingScript (PlutusTx.toBuiltinData seizeRedeemer) globalCred 0
                <> withWithdrawal issuerCred 0
                <> withScriptInput
                    (PlutusTx.toBuiltinData ())
                    ( withOutRef (TxOutRef "5e12" 0)
                        <> withAddress seizeInputAddr
                        <> withValue inputValue
                    )
                <> extra
                <> withOutput (withTxOutAddress outAddr <> withTxOutValue outValue)
                <> seizeRefInputsBuilder
            )

mkSeizePairCtx :: Value -> Address -> Value -> ScriptContext
mkSeizePairCtx = mkSeizePairCtxWith mempty

-- | Positive control for the eight fixtures below: the same one-input pairing
-- with nothing tampered with must succeed, so a red result in any sibling test
-- is attributable to the tampering and not to the fixture shape.
unit_seizeAct_structural_pair_control_succeeds :: Assertion
unit_seizeAct_structural_pair_control_succeeds =
    assertSeizeSucceeds (mkSeizePairCtx seizeInputValue seizeInputAddr seizeInputValue)

-- | The continuing output must preserve the input's FULL address. Rewriting only
-- the staking credential keeps the payment credential (so the tokens are still
-- "in the mini-ledger") while re-assigning ownership of every non-seized token in
-- that UTxO to a third party — theft that a payment-credential-only check misses.
unit_seizeAct_paired_output_stake_rewrite_rejected :: Assertion
unit_seizeAct_paired_output_stake_rewrite_rejected =
    assertSeizeFails $
        mkSeizePairCtx
            seizeInputValue
            (scriptAddressWithSignerStake progLogicBaseHash (PubKeyHash (bs28 0x02)))
            seizeInputValue

-- | A seizure may only move the seized policy. Dropping a non-seized policy from
-- the continuing output drains it to the transaction's change output.
unit_seizeAct_non_seized_policy_drained_rejected :: Assertion
unit_seizeAct_non_seized_policy_drained_rejected =
    assertSeizeFails $
        mkSeizePairCtx seizeInputValueWithNoise seizeInputAddr seizeInputValue

-- | The mirror image: a non-seized policy must not be injected into the
-- continuing output either, or a seizure becomes a way to force arbitrary assets
-- into someone else's mini-ledger UTxO.
unit_seizeAct_non_seized_policy_injected_rejected :: Assertion
unit_seizeAct_non_seized_policy_injected_rejected =
    assertSeizeFails $
        mkSeizePairCtxWith
            ( withInput
                ( withOutRef (TxOutRef "f00d" 0)
                    <> withAddress (pubKeyAddress signerPkh)
                    <> withValue (mkAdaValue 2_000_000 <> noiseValue)
                )
            )
            seizeInputValue
            seizeInputAddr
            seizeInputValueWithNoise

-- | Ada is not the seized policy, so a seizure may not strip it: the continuing
-- output must carry at least the input's lovelace.
unit_seizeAct_paired_output_ada_reduced_rejected :: Assertion
unit_seizeAct_paired_output_ada_reduced_rejected =
    assertSeizeFails $
        mkSeizePairCtx
            seizeInputValue
            seizeInputAddr
            (mkAdaValue 2_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 1)])

-- | ...but it may ADD lovelace. A protocol-parameter change can raise the
-- min-UTxO requirement above what a UTxO already holds; if the continuing output
-- had to carry exactly the input's lovelace, every such UTxO would become
-- permanently unseizable, because the ledger would demand more ada than the
-- validator allowed. The extra ada comes from a separate funding input, as it
-- would on chain.
unit_seizeAct_paired_output_ada_topped_up_succeeds :: Assertion
unit_seizeAct_paired_output_ada_topped_up_succeeds =
    assertSeizeSucceeds $
        mkSeizePairCtxWith
            ( withInput
                ( withOutRef (TxOutRef "f00d" 1)
                    <> withAddress (pubKeyAddress signerPkh)
                    <> withValue (mkAdaValue 5_000_000)
                )
            )
            seizeInputValue
            seizeInputAddr
            (mkAdaValue 4_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 1)])

-- | The protocol-parameters reference input is trusted for the base credential
-- and directory policy; it is only legitimate because it carries the params NFT.
-- A datum-identical UTxO without that NFT must not be accepted.
unit_seizeAct_forged_params_ref_input_rejected :: Assertion
unit_seizeAct_forged_params_ref_input_rejected =
    assertSeizeFails $
        let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0 0 1
         in buildBalancedScriptContext
                ( withRewardingScript (PlutusTx.toBuiltinData seizeRedeemer) globalCred 0
                    <> withWithdrawal issuerCred 0
                    <> seizeInputBuilder 0
                    <> seizeCorrespondingOutputBuilder
                    <> withRefInputDatumValue
                        paramRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000)
                        (PlutusTx.toBuiltinData protocolParamsDatum)
                    <> withRefInputDatumValue
                        dirNodeRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                        (PlutusTx.toBuiltinData directoryProgrammableNode)
                )

-- | The directory node authorises WHICH policy may be seized and by whose issuer
-- logic. Without the directory NFT the datum is attacker-authored, so any policy
-- could be seized under any issuer credential.
unit_seizeAct_forged_directory_node_rejected :: Assertion
unit_seizeAct_forged_directory_node_rejected =
    assertSeizeFails $
        let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0 0 1
         in buildBalancedScriptContext
                ( withRewardingScript (PlutusTx.toBuiltinData seizeRedeemer) globalCred 0
                    <> withWithdrawal issuerCred 0
                    <> seizeInputBuilder 0
                    <> seizeCorrespondingOutputBuilder
                    <> withRefInputDatumValue
                        paramRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                        (PlutusTx.toBuiltinData protocolParamsDatum)
                    <> withRefInputDatumValue
                        dirNodeRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000)
                        (PlutusTx.toBuiltinData directoryProgrammableNode)
                )

-- | Seizure is authorised by the seized policy's issuer-logic script; with no
-- such withdrawal in the transaction, anyone able to build a seize redeemer could
-- confiscate tokens.
unit_seizeAct_issuer_logic_not_invoked_rejected :: Assertion
unit_seizeAct_issuer_logic_not_invoked_rejected =
    assertSeizeFails $
        let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0 0 0
         in buildBalancedScriptContext
                ( withRewardingScript (PlutusTx.toBuiltinData seizeRedeemer) globalCred 0
                    <> seizeInputBuilder 0
                    <> seizeCorrespondingOutputBuilder
                    <> seizeRefInputsBuilder
                )

-- | The issuer withdrawal index is a redeemer-supplied hint, so it must be
-- checked rather than trusted: pointing it at a withdrawal that is present but is
-- NOT the issuer-logic credential must not authorise the seizure.
unit_seizeAct_issuer_wdrl_index_wrong_credential_rejected :: Assertion
unit_seizeAct_issuer_wdrl_index_wrong_credential_rejected =
    assertSeizeFails $
        let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0 0 0
         in buildBalancedScriptContext
                ( withRewardingScript (PlutusTx.toBuiltinData seizeRedeemer) globalCred 0
                    <> withWithdrawal issuerCred 0
                    <> seizeInputBuilder 0
                    <> seizeCorrespondingOutputBuilder
                    <> seizeRefInputsBuilder
                )
