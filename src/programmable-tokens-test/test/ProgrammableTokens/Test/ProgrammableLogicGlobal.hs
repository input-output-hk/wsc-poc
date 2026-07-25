{-# LANGUAGE OverloadedStrings #-}

module ProgrammableTokens.Test.ProgrammableLogicGlobal (
    tests,
) where

import Data.ByteString qualified as BS
import Data.Either (isLeft, isRight)
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
    buildScriptContext,
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
        , testCase "unit_registrySpend_minting_own_key_rejected" unit_registrySpend_minting_own_key_rejected
        , testCase "unit_registrySpend_without_own_key_mint_succeeds" unit_registrySpend_without_own_key_mint_succeeds
        , testCase "unit_outputsContain_single_asset_split_across_prog_outputs_succeeds" unit_outputsContain_single_asset_split_across_prog_outputs_succeeds
        , testCase "unit_outputsContain_single_asset_pubkey_output_ignored" unit_outputsContain_single_asset_pubkey_output_ignored
        , testCase "unit_outputsContain_multi_asset_succeeds" unit_outputsContain_multi_asset_succeeds
        , testCase "unit_outputsContain_multi_asset_shortfall_rejected" unit_outputsContain_multi_asset_shortfall_rejected
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
            (TransferAct [1] [1] [Member] 0)
            (-1)
            0

unit_transferAct_burn_without_mint_proof_rejected :: Assertion
unit_transferAct_burn_without_mint_proof_rejected =
    assertScriptFails $
        mkGlobalTransferMintCtx
            (TransferAct [1] [1] [] 0)
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
            (TransferAct [1] [0] [Member] 0)
            1
            1

unit_transferAct_escape_to_pubkey_rejected :: Assertion
unit_transferAct_escape_to_pubkey_rejected =
    assertScriptFails mkGlobalTransferEscapeCtx

-- Regression guard: the transfer-proof walk cons-builds its multi-policy
-- accumulator, which once left the expected value in DESCENDING order and made
-- the (order-sensitive) containment walk reject valid two-policy transfers.
unit_transferAct_two_policies_wholesale_succeeds :: Assertion
unit_transferAct_two_policies_wholesale_succeeds =
    assertScriptSucceeds $
        mkGlobalTransferTwoPoliciesCtx
            (TransferAct [1, 2] [1, 1] [] 0)
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
            (TransferAct [1, 2] [1, 1] [] 0)
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
            (TransferAct [1, 2] [1, 1] [Member] 0)
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
            (TransferAct [1, 2] [1, 1] [Member] 0)
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
            (TransferAct [1] [1] [Member] 0)
            1
            1

unit_transferAct_mint_with_proof_and_containment_succeeds :: Assertion
unit_transferAct_mint_with_proof_and_containment_succeeds =
    assertScriptSucceeds $
        mkGlobalTransferMintCtx
            (TransferAct [1] [1] [Member] 0)
            1
            2

unit_transferAct_mint_without_mint_proof_rejected :: Assertion
unit_transferAct_mint_without_mint_proof_rejected =
    assertScriptFails $
        mkGlobalTransferMintCtx
            (TransferAct [1] [1] [] 0)
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
                            pconstant @(PlutarchV3.PValue 'PlutarchV3.Unsorted 'PlutarchV3.NoGuarantees) expectedValue
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
mkGlobalTransferTwoPoliciesCtx globalRedeemer mintEntries progOutputVal =
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

mkGlobalTransferEscapeCtx :: ScriptContext
mkGlobalTransferEscapeCtx =
    buildBalancedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData $ TransferAct [1] [1] [] 0)
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
