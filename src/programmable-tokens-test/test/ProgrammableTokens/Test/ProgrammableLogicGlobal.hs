{-# LANGUAGE OverloadedStrings #-}

module ProgrammableTokens.Test.ProgrammableLogicGlobal (
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
    withInput,
    withMint,
    withOutRef,
    withOutput,
    withReferenceInput,
    withRewardingScript,
    withScriptInput,
    withSigner,
    withTxOutAddress,
    withTxOutValue,
    withValue,
    withWithdrawal,
 )
import SmartTokens.Contracts.ProgrammableLogicBase (
    ProgrammableLogicGlobalRedeemer (TransferAct),
    TokenProof (TokenExists),
    mkProgrammableLogicGlobal,
    mkSeizeActRedeemerFromRelativeInputIdxs,
 )
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
        , testCase "unit_seizeAct_pubkey_index_rejected" unit_seizeAct_pubkey_index_rejected
        , testCase "unit_seizeAct_omitted_index_rejected" unit_seizeAct_omitted_index_rejected
        , testCase "unit_transferAct_mint_smuggle_rejected" unit_transferAct_mint_smuggle_rejected
        , testCase "unit_transferAct_mint_with_proof_and_containment_succeeds" unit_transferAct_mint_with_proof_and_containment_succeeds
        , testCase "unit_transferAct_mint_without_mint_proof_rejected" unit_transferAct_mint_without_mint_proof_rejected
        , testProperty "prop_seizeAct_complete_indices_succeeds" prop_seizeAct_complete_indices_succeeds
        , testProperty "prop_seizeAct_omitted_index_rejected" prop_seizeAct_omitted_index_rejected
        ]

unit_seizeAct_complete_indices_succeeds :: Assertion
unit_seizeAct_complete_indices_succeeds =
    assertScriptSucceeds $
        mkGlobalSeizeCtx 3 [0, 0, 0]

unit_seizeAct_pubkey_index_rejected :: Assertion
unit_seizeAct_pubkey_index_rejected =
    assertScriptFails $
        mkGlobalSeizeCtxWithLeadingPubKey 1 [0]

unit_seizeAct_omitted_index_rejected :: Assertion
unit_seizeAct_omitted_index_rejected =
    assertScriptFails $
        mkGlobalSeizeCtx 3 [0, 0]

unit_transferAct_mint_smuggle_rejected :: Assertion
unit_transferAct_mint_smuggle_rejected =
    assertScriptFails $
        mkGlobalTransferMintCtx
            (TransferAct [TokenExists 1] [TokenExists 1])
            1
            1

unit_transferAct_mint_with_proof_and_containment_succeeds :: Assertion
unit_transferAct_mint_with_proof_and_containment_succeeds =
    assertScriptSucceeds $
        mkGlobalTransferMintCtx
            (TransferAct [TokenExists 1] [TokenExists 1])
            1
            2

unit_transferAct_mint_without_mint_proof_rejected :: Assertion
unit_transferAct_mint_without_mint_proof_rejected =
    assertScriptFails $
        mkGlobalTransferMintCtx
            (TransferAct [TokenExists 1] [])
            1
            2

prop_seizeAct_complete_indices_succeeds :: QC.Property
prop_seizeAct_complete_indices_succeeds =
    QC.forAll (QC.chooseInt (1, 12)) $ \nInt ->
        let n = fromIntegral nInt
            idxs = replicate nInt 0
         in QC.counterexample ("n=" <> show n <> ", idxs=" <> show idxs) $
                scriptSucceeds (mkGlobalSeizeCtx n idxs) QC.=== True

prop_seizeAct_omitted_index_rejected :: QC.Property
prop_seizeAct_omitted_index_rejected =
    QC.forAll (QC.chooseInt (2, 12)) $ \nInt ->
        let n = fromIntegral nInt
            idxs = replicate (nInt - 1) 0
         in QC.counterexample ("n=" <> show n <> ", idxs=" <> show idxs) $
                scriptFails (mkGlobalSeizeCtx n idxs) QC.=== True

assertScriptSucceeds :: ScriptContext -> Assertion
assertScriptSucceeds ctx =
    assertBool "expected successful evaluation" (scriptSucceeds ctx)

assertScriptFails :: ScriptContext -> Assertion
assertScriptFails ctx =
    assertBool "expected script failure" (scriptFails ctx)

scriptSucceeds :: ScriptContext -> Bool
scriptSucceeds ctx =
    let (res, _budget, _logs) = evalScript (applyArguments globalScript [PlutusTx.toData protocolParamsCS, PlutusTx.toData ctx])
     in isRight res

scriptFails :: ScriptContext -> Bool
scriptFails ctx =
    let (res, _budget, _logs) = evalScript (applyArguments globalScript [PlutusTx.toData protocolParamsCS, PlutusTx.toData ctx])
     in isLeft res

compileNoTracing :: (forall s. Term s a) -> Script
compileNoTracing term =
    either (error . ("compile failed: " <>) . show) id (compile NoTracing term)

globalScript :: Script
globalScript = compileNoTracing mkProgrammableLogicGlobal

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

issuerLogicHash :: ScriptHash
issuerLogicHash = ScriptHash (bs28 0x14)

issuerCred :: Credential
issuerCred = ScriptCredential issuerLogicHash

programmableTransferCS :: CurrencySymbol
programmableTransferCS = CurrencySymbol (bs28 0x1b)

signerPkh :: PubKeyHash
signerPkh = PubKeyHash (bs28 0x01)

paramRef :: TxOutRef
paramRef = TxOutRef "aa00" 0

dirNodeRef :: TxOutRef
dirNodeRef = TxOutRef "bb00" 0

scriptAddressWithSignerStake :: ScriptHash -> PubKeyHash -> Address
scriptAddressWithSignerStake sh pkh =
    Address (ScriptCredential sh) (Just (StakingHash (PubKeyCredential pkh)))

pubKeyAddress :: PubKeyHash -> Address
pubKeyAddress pkh = Address (PubKeyCredential pkh) Nothing

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
    ProgrammableLogicGlobalParams directoryNodeCS (ScriptCredential progLogicBaseHash)

directoryProgrammableNode :: DirectorySetNode
directoryProgrammableNode =
    DirectorySetNode
        programmableTransferCS
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

mkGlobalSeizeCtx :: Integer -> [Integer] -> ScriptContext
mkGlobalSeizeCtx inputCount providedIdxs =
    let expectedIdxs = [0 .. (inputCount - 1)]
        seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 providedIdxs 0
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
        seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 providedIdxs 0
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
