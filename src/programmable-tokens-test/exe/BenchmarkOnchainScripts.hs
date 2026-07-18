{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import BenchmarkOnchain.CardanoScriptHelpers (scriptHashFromCardanoScript)
import BenchmarkOnchain.Compile (compileNoTracing)
import BenchmarkOnchain.MainnetDexFixture
import BenchmarkOnchain.ScriptFixtureIds
import BenchmarkOnchain.ScriptHelpers (bs28, mkValue, pubKeyAddress, scriptAddress, scriptAddressWithSignerStake, scriptAddressWithStakeCredential, stripZeroChangeOutput, withAuxiliaryRewardingScript, withPubKeyInputValue, withRefInputDatumValue)
import BenchmarkOnchain.ScriptRunner (BenchCase, EvalKind (..), EvalSpec (..), mkBenchCase, runScriptBenchmark)
import BenchmarkOnchain.ScriptScenario qualified as Scenario
import BenchmarkOnchain.TxD29Fixture
import Data.ByteString qualified as BS
import Plutarch.Internal.Term (Script, Term)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import ProgrammableTokens.OffChain.Scripts qualified as OffchainScripts
import ProgrammableTokens.Test.ScriptContext.Builder (ScriptContextBuilder, buildBalancedScriptContext, buildScriptContext, mkAdaValue, withAddress, withFee, withInlineDatum, withInput, withMint, withMintingScript, withOutRef, withOutput, withRedeemer, withRewardingScript, withScriptInput, withSigner, withTxOutAddress, withTxOutInlineDatum, withTxOutValue, withValue, withWithdrawal)
import SmartTokens.Contracts.AlwaysYields (palwaysSucceed)
import SmartTokens.Contracts.Issuance (mkProgrammableLogicMinting)
import SmartTokens.Contracts.IssuanceCborHex (IssuanceCborHex (IssuanceCborHex), mkIssuanceCborHexMinting)
import SmartTokens.Contracts.ProgrammableLogicBase (ProgrammableLogicGlobalRedeemer (TransferAct), mkProgrammableLogicBase, mkProgrammableLogicGlobal, mkProgrammableSeize, mkSeizeActRedeemerFromAbsoluteInputIdxs)
import SmartTokens.Contracts.ProtocolParams (mkProtocolParametersMinting)
import SmartTokens.Core.Scripts (ScriptTarget (Production))
import SmartTokens.LinkedList.MintDirectory (DirectoryNodeAction (InitDirectory, InsertDirectoryNode), mkDirectoryNodeMP)
import SmartTokens.LinkedList.SpendDirectory (pmkDirectorySpending)
import SmartTokens.Types.Constants (issuanceCborHexToken, protocolParamsToken)
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (DirectorySetNode))
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (ProgrammableLogicGlobalParams))

main :: IO ()
main =
    runScriptBenchmark
        "Onchain script benchmark (NoTracing, one case per redeemer path)"
        benchCases
        scenarioEvalSpecsFromCtx

compiledAlwaysSucceedsScript :: Script
compiledAlwaysSucceedsScript =
    compileNoTracing palwaysSucceed

compiledDirectorySpendingScript :: Script
compiledDirectorySpendingScript =
    compileNoTracing pmkDirectorySpending

mkCase :: String -> EvalKind -> (forall s. Term s a) -> [Data] -> ScriptContext -> BenchCase
mkCase name primaryKind term args ctx =
    mkBenchCase name primaryKind (compileNoTracing term) args ctx

scenarioEvalSpecsFromCtx :: ScriptContext -> [EvalSpec]
scenarioEvalSpecsFromCtx =
    Scenario.scenarioEvalSpecsFromCtx scenarioBackend scenarioEnv

scenarioBackend :: Scenario.ScriptScenarioBackend
scenarioBackend =
    Scenario.ScriptScenarioBackend
        { Scenario.ssbAlwaysSucceedsRewardSpec =
            \scriptName cred purposeCtx ->
                EvalSpec
                    (EvalAlwaysSucceedsReward scriptName cred)
                    compiledAlwaysSucceedsScript
                    [toData purposeCtx]
        , Scenario.ssbAlwaysSucceedsSpendSpec =
            \scriptName outRef purposeCtx ->
                EvalSpec
                    (EvalAlwaysSucceedsSpend scriptName outRef)
                    compiledAlwaysSucceedsScript
                    [toData purposeCtx]
        , Scenario.ssbBaseSpendSpec =
            \_ globalStakeCred outRef purposeCtx ->
                EvalSpec
                    (EvalBaseSpend outRef)
                    (compileNoTracing mkProgrammableLogicBase)
                    [toData globalStakeCred, toData seizeCredBench, toData purposeCtx]
        , Scenario.ssbDirectoryMintSpec =
            \env cs purposeCtx ->
                EvalSpec
                    (EvalDirectoryMint cs)
                    (compileNoTracing mkDirectoryNodeMP)
                    [toData (Scenario.sseInitRef env), toData (Scenario.sseIssuancePolicyCS env), toData purposeCtx]
        , Scenario.ssbDirectorySpendSpec =
            \_ paramsCs outRef purposeCtx ->
                EvalSpec
                    (EvalDirectorySpend outRef)
                    compiledDirectorySpendingScript
                    [toData paramsCs, toData purposeCtx]
        , Scenario.ssbGlobalRewardSpec =
            \_ paramsCs cred purposeCtx ->
                EvalSpec
                    (EvalGlobalReward cred)
                    (compileNoTracing mkProgrammableLogicGlobal)
                    [toData paramsCs, toData purposeCtx]
        , Scenario.ssbSeizeRewardSpec =
            \_ paramsCs cred purposeCtx ->
                EvalSpec
                    -- Labelled "programmableSeize" so it appears as its own row in
                    -- the breakdown/size tables. Runs the real standalone seize
                    -- validator so its CPU + serialised size are honestly measured.
                    (EvalAlwaysSucceedsReward "programmableSeize" cred)
                    (compileNoTracing mkProgrammableSeize)
                    [toData paramsCs, toData purposeCtx]
        , Scenario.ssbIssuanceMintSpec =
            \env cs purposeCtx ->
                EvalSpec
                    (EvalIssuanceMint cs)
                    (compileNoTracing mkIssuanceCborHexMinting)
                    [toData (Scenario.sseIssuanceInitRef env), toData purposeCtx]
        , Scenario.ssbProgrammableMintSpec =
            \env cs purposeCtx ->
                EvalSpec
                    (EvalProgrammableMint cs)
                    (compileNoTracing mkProgrammableLogicMinting)
                    [toData (Scenario.sseProgLogicBaseCred env), toData (Scenario.sseMintingLogicHash env), toData purposeCtx]
        , Scenario.ssbProtocolParamsMintSpec =
            \env cs purposeCtx ->
                EvalSpec
                    (EvalProtocolParamsMint cs)
                    (compileNoTracing mkProtocolParametersMinting)
                    [toData (Scenario.sseProtocolParamsInitRef env), toData purposeCtx]
        }

scenarioEnv :: Scenario.ScriptScenarioEnv
scenarioEnv =
    Scenario.ScriptScenarioEnv
        { Scenario.sseDirectoryPolicyCS = directoryPolicyCS
        , Scenario.sseDirectorySpendHash = ScriptHash (bs28 0x44)
        , Scenario.sseExternalAlwaysSucceedsHash = externalAlwaysSucceedsHash
        , Scenario.sseExternalAlwaysSucceedsHash2 = externalAlwaysSucceedsHash2
        , Scenario.sseGlobalCred = globalCred
        , Scenario.sseInitRef = initRef
        , Scenario.sseIssuanceInitRef = issuanceInitRef
        , Scenario.sseIssuancePolicyCS = issuancePolicyCS
        , Scenario.sseIssuerCred = issuerCred
        , Scenario.sseMintingLogicHash = mintingLogicHash
        , Scenario.sseMintingPolicyCS = mintingPolicyCS
        , Scenario.sseProgLogicBaseCred = progLogicBaseCred
        , Scenario.sseProgLogicBaseHash = progLogicBaseHash
        , Scenario.sseProtocolParamsCS = protocolParamsCS
        , Scenario.sseProtocolParamsInitRef = protocolParamsInitRef
        , Scenario.sseSeizeCred = seizeCredBench
        , Scenario.sseTransferLogicHash = transferLogicHash
        , Scenario.sseTxD29BaseScriptCred = txD29BaseScriptCred
        , Scenario.sseTxD29GlobalStakeCred = txD29GlobalStakeCred
        , Scenario.sseTxD29ProtocolParamsCS = txD29ProtocolParamsCS
        , Scenario.sseTxD29TransferLogicStakeCred = txD29TransferLogicStakeCred
        }

-- Common mini-ledger fixture constants shared by the synthetic contexts below.
maxBs28 :: BuiltinByteString
-- | Placeholder seize-validator credential for the global's new seizeLogicCred
-- script parameter. Transfer scenarios never exercise seize, so any fixed
-- credential suffices there. Seize scenarios that omit it from withdrawals will
-- (correctly) be rejected by the global's delegation check until the seize
-- validator is wired into those scenarios.
seizeCredBench :: Credential
seizeCredBench = ScriptCredential (ScriptHash (bs28 0x40))

maxBs28 = bs28 0xff

computeRegisteredCs :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> CurrencySymbol
computeRegisteredCs prefix postfix hashedParam =
    CurrencySymbol $
        BI.blake2b_224
            ( versionHeader
                <> prefix
                <> BI.serialiseData (PlutusTx.toBuiltinData hashedParam)
                <> postfix
            )
  where
    versionHeader = PV1.toBuiltin (BS.pack [3])

tailCS :: CurrencySymbol
tailCS = CurrencySymbol maxBs28

protocolParamsAlwaysFailHash :: ScriptHash
protocolParamsAlwaysFailHash =
    scriptHashFromCardanoScript (OffchainScripts.protocolParamsSpendingScript Production)

issuanceAlwaysFailHash :: ScriptHash
issuanceAlwaysFailHash =
    scriptHashFromCardanoScript (OffchainScripts.issuanceCborHexSpendingScript Production)

protocolParamsDatum :: ProgrammableLogicGlobalParams
protocolParamsDatum =
    ProgrammableLogicGlobalParams directoryNodeCS progLogicBaseCred

issuanceDatum :: BuiltinData
issuanceDatum =
    PlutusTx.toBuiltinData (IssuanceCborHex "0d" "0e")

directoryCoveringNode :: DirectorySetNode
directoryCoveringNode =
    DirectorySetNode
        (CurrencySymbol "")
        tailCS
        (ScriptCredential transferLogicHash)
        issuerCred
        (CurrencySymbol "")

directoryProgrammableNode :: DirectorySetNode
directoryProgrammableNode =
    DirectorySetNode
        programmableTransferCS
        tailCS
        (ScriptCredential transferLogicHash)
        issuerCred
        (CurrencySymbol "")

directoryMintingNode :: DirectorySetNode
directoryMintingNode =
    DirectorySetNode
        mintingPolicyCS
        tailCS
        (ScriptCredential transferLogicHash)
        issuerCred
        (CurrencySymbol "")

directoryProgrammableNode2 :: DirectorySetNode
directoryProgrammableNode2 =
    DirectorySetNode
        programmableTransferCS2
        tailCS
        (ScriptCredential transferLogicHash)
        issuerCred
        (CurrencySymbol "")

directoryProgrammableNode3 :: DirectorySetNode
directoryProgrammableNode3 =
    DirectorySetNode
        programmableTransferCS3
        tailCS
        (ScriptCredential transferLogicHash)
        issuerCred
        (CurrencySymbol "")

seizeInputIdxsFor :: Integer -> [Integer]
seizeInputIdxsFor n = [0 .. (n - 1)]

seizeInputAddr :: Address
seizeInputAddr = scriptAddressWithSignerStake progLogicBaseHash signerPkh

seizeInputValue :: Value
seizeInputValue =
    mkAdaValue 3_000_000
        <> mkValue [(programmableTransferCS, TokenName "0c", 1)]

seizeCorrespondingOutputValue :: Value
seizeCorrespondingOutputValue = mkAdaValue 3_000_000

seizeResidualOutputValueFor :: Integer -> Value
seizeResidualOutputValueFor n =
    mkValue [(programmableTransferCS, TokenName "0c", n)]

seizeInputBuilder :: Integer -> ScriptContextBuilder
seizeInputBuilder idx =
    withScriptInput
        (PlutusTx.toBuiltinData ())
        ( withOutRef (TxOutRef seizeInputTxId idx)
            <> withAddress seizeInputAddr
            <> withValue seizeInputValue
        )

leadingPubKeyInputBuilder :: Integer -> ScriptContextBuilder
leadingPubKeyInputBuilder idx =
    withInput
        ( withOutRef (TxOutRef leadingPubKeyInputTxId idx)
            <> withAddress (pubKeyAddress signerPkh)
            <> withValue (mkAdaValue 2_000_000)
        )

externalScriptInputBuilder :: ScriptContextBuilder
externalScriptInputBuilder =
    withScriptInput
        (PlutusTx.toBuiltinData ())
        ( withOutRef externalScriptInputRef
            <> withAddress (scriptAddress externalAlwaysSucceedsHash)
            <> withValue (mkAdaValue 2_000_000)
        )

seizeCorrespondingOutputBuilder :: ScriptContextBuilder
seizeCorrespondingOutputBuilder =
    withOutput
        ( withTxOutAddress seizeInputAddr
            <> withTxOutValue seizeCorrespondingOutputValue
        )

seizeResidualOutputBuilder :: Integer -> ScriptContextBuilder
seizeResidualOutputBuilder n =
    withOutput
        ( withTxOutAddress seizeInputAddr
            <> withTxOutValue (seizeResidualOutputValueFor n)
        )

-- Contexts / redeemer paths
baseSpendingCtx :: ScriptContext
baseSpendingCtx =
    let ScriptContext txInfo _ _ = globalTransferCtx
     in ScriptContext
            txInfo
            (Redeemer (PlutusTx.toBuiltinData ()))
            (SpendingScript progInputRef Nothing)

globalTransferCtx :: ScriptContext
globalTransferCtx =
    buildBalancedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData $ TransferAct [1] [])
            globalCred
            0
            <> withSigner signerPkh
            <> withAuxiliaryRewardingScript (ScriptCredential transferLogicHash) (PlutusTx.toBuiltinData ())
            <> withScriptInput
                (PlutusTx.toBuiltinData ())
                ( withOutRef progInputRef
                    <> withAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                    <> withValue
                        ( mkAdaValue 10_000_000
                            <> mkValue [(programmableTransferCS, TokenName "0c", 5)]
                        )
                )
            <> withOutput
                ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                    <> withTxOutValue (mkAdaValue 3_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 2)])
                )
            <> withOutput
                ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash recipientPkh)
                    <> withTxOutValue (mkAdaValue 3_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 3)])
                )
            <> withOutput
                ( withTxOutAddress (pubKeyAddress signerPkh)
                    <> withTxOutValue (mkAdaValue 2_000_000)
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

globalTransferDoesNotExistCtx :: ScriptContext
globalTransferDoesNotExistCtx =
    buildBalancedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData $ TransferAct [1] [])
            globalCred
            0
            <> withSigner signerPkh
            <> withScriptInput
                (PlutusTx.toBuiltinData ())
                ( withOutRef progInputRef
                    <> withAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                    <> withValue
                        ( mkAdaValue 10_000_000
                            <> mkValue
                                [(nonProgrammableCS, TokenName "np", 4)]
                        )
                )
            <> withOutput
                ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                    <> withTxOutValue (mkAdaValue 3_000_000)
                )
            <> withOutput
                ( withTxOutAddress (pubKeyAddress signerPkh)
                    <> withTxOutValue (mkAdaValue 2_000_000 <> mkValue [(nonProgrammableCS, TokenName "np", 4)])
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
                (PlutusTx.toBuiltinData directoryCoveringNode)
        )

globalTransferMixedManyCtx :: ScriptContext
globalTransferMixedManyCtx =
    buildBalancedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData $ TransferAct [1, 2, 3, 4, 1] [])
            globalCred
            0
            <> withSigner signerPkh
            <> withAuxiliaryRewardingScript (ScriptCredential transferLogicHash) (PlutusTx.toBuiltinData ())
            <> withScriptInput
                (PlutusTx.toBuiltinData ())
                ( withOutRef progInputRef
                    <> withAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                    <> withValue
                        ( mkAdaValue 10_000_000
                            <> mkValue
                                [ (nonProgrammableCS, TokenName "np1", 4)
                                , (programmableTransferCS, TokenName "0c", 5)
                                , (programmableTransferCS2, TokenName "1c", 7)
                                , (programmableTransferCS3, TokenName "2c", 9)
                                , (nonProgrammableCS2, TokenName "np2", 6)
                                ]
                        )
                )
            <> withOutput
                ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                    <> withTxOutValue
                        ( mkAdaValue 3_000_000
                            <> mkValue
                                [ (programmableTransferCS, TokenName "0c", 5)
                                , (programmableTransferCS2, TokenName "1c", 7)
                                , (programmableTransferCS3, TokenName "2c", 9)
                                ]
                        )
                )
            <> withOutput
                ( withTxOutAddress (pubKeyAddress signerPkh)
                    <> withTxOutValue
                        ( mkAdaValue 2_000_000
                            <> mkValue
                                [ (nonProgrammableCS, TokenName "np1", 4)
                                , (nonProgrammableCS2, TokenName "np2", 6)
                                ]
                        )
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
                (PlutusTx.toBuiltinData directoryCoveringNode)
            <> withRefInputDatumValue
                directoryProgrammableNodeRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                (PlutusTx.toBuiltinData directoryProgrammableNode)
            <> withRefInputDatumValue
                directoryProgrammableNode2Ref
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                (PlutusTx.toBuiltinData directoryProgrammableNode2)
            <> withRefInputDatumValue
                directoryProgrammableNode3Ref
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                (PlutusTx.toBuiltinData directoryProgrammableNode3)
        )

transferManyInputAddr :: Address
transferManyInputAddr = scriptAddressWithSignerStake progLogicBaseHash signerPkh

transferManyInputValue :: Value
transferManyInputValue =
    mkAdaValue 3_000_000
        <> mkValue
            [ (nonProgrammableCS, TokenName "np", 1)
            , (programmableTransferCS, TokenName "0c", 1)
            ]

transferManyInputBuilder :: Integer -> ScriptContextBuilder
transferManyInputBuilder idx =
    withScriptInput
        (PlutusTx.toBuiltinData ())
        ( withOutRef (TxOutRef transferManyInputTxId idx)
            <> withAddress transferManyInputAddr
            <> withValue transferManyInputValue
        )

mkGlobalTransferManyCtx :: Integer -> ScriptContext
mkGlobalTransferManyCtx inputCount =
    let inputRefs = [0 .. (inputCount - 1)]
        scriptInputsBuilder = mconcat (map transferManyInputBuilder inputRefs)
        qtyInFirstOutput = inputCount `div` 2
        qtyInSecondOutput = inputCount - qtyInFirstOutput
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData $ TransferAct [1, 2] [])
                globalCred
                0
                <> withSigner signerPkh
                <> withAuxiliaryRewardingScript (ScriptCredential transferLogicHash) (PlutusTx.toBuiltinData ())
                <> scriptInputsBuilder
                <> withOutput
                    ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                        <> withTxOutValue (mkAdaValue 3_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", qtyInFirstOutput)])
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash recipientPkh)
                        <> withTxOutValue (mkAdaValue 3_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", qtyInSecondOutput)])
                    )
                <> withOutput
                    ( withTxOutAddress (pubKeyAddress signerPkh)
                        <> withTxOutValue
                            ( mkAdaValue 2_000_000
                                <> mkValue [(nonProgrammableCS, TokenName "np", inputCount)]
                            )
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
                    (PlutusTx.toBuiltinData directoryCoveringNode)
                <> withRefInputDatumValue
                    directoryProgrammableNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (PlutusTx.toBuiltinData directoryProgrammableNode)
            )

globalTransfer5Ctx :: ScriptContext
globalTransfer5Ctx = mkGlobalTransferManyCtx 5

globalTransfer10Ctx :: ScriptContext
globalTransfer10Ctx = mkGlobalTransferManyCtx 10

globalTransfer15Ctx :: ScriptContext
globalTransfer15Ctx = mkGlobalTransferManyCtx 15

globalTransfer25Ctx :: ScriptContext
globalTransfer25Ctx = mkGlobalTransferManyCtx 25

globalTransfer50Ctx :: ScriptContext
globalTransfer50Ctx = mkGlobalTransferManyCtx 50

globalTransfer100Ctx :: ScriptContext
globalTransfer100Ctx = mkGlobalTransferManyCtx 100

-- | Mirrors the Aiken bench @many_tokens@ axis: one mini-ledger UTxO carrying
-- many asset names under a single programmable policy moves wholesale to one
-- recipient output.
mkGlobalTransferManyTokensCtx :: Integer -> ScriptContext
mkGlobalTransferManyTokensCtx tokenCount =
    let manyTokensValue = mkValue [(programmableTransferCS, manyTokensTokenName i, 2) | i <- [0 .. (tokenCount - 1)]]
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData $ TransferAct [1] [])
                globalCred
                0
                <> withSigner signerPkh
                <> withAuxiliaryRewardingScript (ScriptCredential transferLogicHash) (PlutusTx.toBuiltinData ())
                <> withScriptInput
                    (PlutusTx.toBuiltinData ())
                    ( withOutRef manyTokensInputRef
                        <> withAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                        <> withValue (mkAdaValue 10_000_000 <> manyTokensValue)
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash recipientPkh)
                        <> withTxOutValue (mkAdaValue 3_000_000 <> manyTokensValue)
                    )
                <> withOutput
                    ( withTxOutAddress (pubKeyAddress signerPkh)
                        <> withTxOutValue (mkAdaValue 2_000_000)
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

globalTransferManyTokens50Ctx :: ScriptContext
globalTransferManyTokens50Ctx = mkGlobalTransferManyTokensCtx manyTokensCount

-- | Mirrors the Aiken bench @many_outputs@ axis: one mini-ledger input fans
-- out to many recipient outputs (airdrop shape).
mkGlobalTransferManyOutputsCtx :: Integer -> ScriptContext
mkGlobalTransferManyOutputsCtx outputCount =
    let recipientOutputBuilder =
            withOutput
                ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash recipientPkh)
                    <> withTxOutValue (mkAdaValue 3_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 1)])
                )
        recipientOutputsBuilder = mconcat (replicate (fromIntegral outputCount) recipientOutputBuilder)
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData $ TransferAct [1] [])
                globalCred
                0
                <> withSigner signerPkh
                <> withAuxiliaryRewardingScript (ScriptCredential transferLogicHash) (PlutusTx.toBuiltinData ())
                <> withScriptInput
                    (PlutusTx.toBuiltinData ())
                    ( withOutRef manyOutputsInputRef
                        <> withAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                        -- Enough ada to fund every fan-out output (outputCount x 3 ada
                        -- + change) so the balanced context stays ledger-realizable.
                        <> withValue (mkAdaValue 65_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", outputCount)])
                    )
                <> recipientOutputsBuilder
                <> withOutput
                    ( withTxOutAddress (pubKeyAddress signerPkh)
                        <> withTxOutValue (mkAdaValue 2_000_000)
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

globalTransferManyOutputs20Ctx :: ScriptContext
globalTransferManyOutputs20Ctx = mkGlobalTransferManyOutputsCtx manyOutputsCount

-- | Directory node registering the synthetic policy @manyPolicyCS i@ for the
-- many-policies axis.
manyPolicyNode :: Integer -> DirectorySetNode
manyPolicyNode i =
    DirectorySetNode
        (manyPolicyCS i)
        tailCS
        (ScriptCredential transferLogicHash)
        issuerCred
        (CurrencySymbol "")

-- | Mirrors the Aiken bench @many_policies@ axis: one input carries many
-- distinct programmable policies, each needing its own positional proof and
-- its own directory-node reference input (worst-case proof-list walk).
mkGlobalTransferManyPoliciesCtx :: Integer -> ScriptContext
mkGlobalTransferManyPoliciesCtx policyCount =
    let idxs = [0 .. (policyCount - 1)]
        manyPoliciesValue = mkValue [(manyPolicyCS i, TokenName "0c", 1) | i <- idxs]
        -- Reference inputs are insertion-sorted by TxOutRef in the builder
        -- (composition order is NOT what places them). paramRef's txId (0xaa…)
        -- sorts before every manyPolicyNodeRef txId (0xbb 0x20+i…), and the
        -- node txIds ascend with i, so paramRef sits at ref index 0 and the
        -- node for policy i lands at ref index 1 + i, matching the
        -- TransferAct proof indices. Adding a ref input with a differently
        -- sorting TxOutRef would shift these indices.
        policyNodeRefsBuilder =
            mconcat
                [ withRefInputDatumValue
                    (manyPolicyNodeRef i)
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (PlutusTx.toBuiltinData (manyPolicyNode i))
                | i <- idxs
                ]
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData $ TransferAct [1 + i | i <- idxs] [])
                globalCred
                0
                <> withSigner signerPkh
                <> withAuxiliaryRewardingScript (ScriptCredential transferLogicHash) (PlutusTx.toBuiltinData ())
                <> withScriptInput
                    (PlutusTx.toBuiltinData ())
                    ( withOutRef manyPoliciesInputRef
                        <> withAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                        <> withValue (mkAdaValue 10_000_000 <> manyPoliciesValue)
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash recipientPkh)
                        <> withTxOutValue (mkAdaValue 3_000_000 <> manyPoliciesValue)
                    )
                <> withOutput
                    ( withTxOutAddress (pubKeyAddress signerPkh)
                        <> withTxOutValue (mkAdaValue 2_000_000)
                    )
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    (PlutusTx.toBuiltinData protocolParamsDatum)
                <> policyNodeRefsBuilder
            )

globalTransferManyPolicies10Ctx :: ScriptContext
globalTransferManyPolicies10Ctx = mkGlobalTransferManyPoliciesCtx manyPoliciesCount

mkGlobalSeizeCtx :: Integer -> ScriptContext
mkGlobalSeizeCtx seizeInputCount =
    let seizeInputIdxs = seizeInputIdxsFor seizeInputCount
        seizeInputRefs = [0 .. (seizeInputCount - 1)]
        seizeRedeemer = mkSeizeActRedeemerFromAbsoluteInputIdxs 1 seizeInputIdxs 0
        seizeInputsBuilder = mconcat (map seizeInputBuilder seizeInputRefs)
        correspondingOutputsBuilder = mconcat (replicate (fromIntegral seizeInputCount) seizeCorrespondingOutputBuilder)
     in stripZeroChangeOutput $
            buildBalancedScriptContext
                ( withRewardingScript
                    (PlutusTx.toBuiltinData seizeRedeemer)
                    seizeCredBench
                    0
                    <> withAuxiliaryRewardingScript issuerCred (PlutusTx.toBuiltinData ())
                    <> seizeInputsBuilder
                    -- put the residual output first so it is last in tx output order
                    -- (withOutput prepends in the builder)
                    <> seizeResidualOutputBuilder seizeInputCount
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

globalSeize1Ctx :: ScriptContext
globalSeize1Ctx =
    mkGlobalSeizeCtx 1

globalSeize5Ctx :: ScriptContext
globalSeize5Ctx =
    mkGlobalSeizeCtx 5

globalSeize10Ctx :: ScriptContext
globalSeize10Ctx =
    mkGlobalSeizeCtx 10

globalSeize20Ctx :: ScriptContext
globalSeize20Ctx =
    mkGlobalSeizeCtx 20

globalSeize50Ctx :: ScriptContext
globalSeize50Ctx =
    mkGlobalSeizeCtx 50

globalSeize100Ctx :: ScriptContext
globalSeize100Ctx =
    mkGlobalSeizeCtx 100

globalSeize150Ctx :: ScriptContext
globalSeize150Ctx =
    mkGlobalSeizeCtx 150

-- | Mirrors the Aiken bench @baseline_3rd_party_2@ shape: two mini-ledger
-- inputs carrying unrelated noise tokens beside the seized policy; input 0 is
-- only partially seized. Clawback needs no owner authorization, so no signer.
globalSeizeNoiseCtx :: ScriptContext
globalSeizeNoiseCtx =
    let seizeRedeemer = mkSeizeActRedeemerFromAbsoluteInputIdxs 1 [0, 1] 0
     in stripZeroChangeOutput $
            buildBalancedScriptContext
                ( withRewardingScript
                    (PlutusTx.toBuiltinData seizeRedeemer)
                    seizeCredBench
                    0
                    <> withAuxiliaryRewardingScript issuerCred (PlutusTx.toBuiltinData ())
                    <> withScriptInput
                        (PlutusTx.toBuiltinData ())
                        ( withOutRef (TxOutRef seizeNoiseInputTxId 0)
                            <> withAddress seizeInputAddr
                            <> withValue
                                ( mkAdaValue 3_000_000
                                    <> mkValue
                                        [ (programmableTransferCS, TokenName "0c", 2)
                                        , (nonProgrammableCS, TokenName "np", 1)
                                        ]
                                )
                        )
                    <> withScriptInput
                        (PlutusTx.toBuiltinData ())
                        ( withOutRef (TxOutRef seizeNoiseInputTxId 1)
                            <> withAddress seizeInputAddr
                            <> withValue
                                ( mkAdaValue 3_000_000
                                    <> mkValue
                                        [ (programmableTransferCS, TokenName "0c", 3)
                                        , (nonProgrammableCS2, TokenName "np2", 5)
                                        ]
                                )
                        )
                    -- withOutput prepends, so compose the residual first: the
                    -- final tx output order must be [paired-with-in0,
                    -- paired-with-in1, residual].
                    <> withOutput
                        ( withTxOutAddress seizeInputAddr
                            <> withTxOutValue (mkValue [(programmableTransferCS, TokenName "0c", 4)])
                        )
                    <> withOutput
                        ( withTxOutAddress seizeInputAddr
                            <> withTxOutValue (mkAdaValue 3_000_000 <> mkValue [(nonProgrammableCS2, TokenName "np2", 5)])
                        )
                    <> withOutput
                        ( withTxOutAddress seizeInputAddr
                            <> withTxOutValue
                                ( mkAdaValue 3_000_000
                                    <> mkValue
                                        [ (programmableTransferCS, TokenName "0c", 1)
                                        , (nonProgrammableCS, TokenName "np", 1)
                                        ]
                                )
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

mkGlobalSeizeExternalScriptAndManyPubKeyCtx :: Integer -> ScriptContext
mkGlobalSeizeExternalScriptAndManyPubKeyCtx pubKeyInputCount =
    let pubKeyInputIdxs = [0 .. (pubKeyInputCount - 1)]
        seizeRedeemer =
            mkSeizeActRedeemerFromAbsoluteInputIdxs
                1
                -- The Plutarch validator accounts for every spending redeemer in
                -- the transaction, so the external script spend is listed too.
                [pubKeyInputCount, pubKeyInputCount + 1]
                0
        pubKeyInputsBuilder = mconcat (map leadingPubKeyInputBuilder pubKeyInputIdxs)
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData seizeRedeemer)
                seizeCredBench
                0
                <> withSigner signerPkh
                <> withAuxiliaryRewardingScript issuerCred (PlutusTx.toBuiltinData ())
                <> pubKeyInputsBuilder
                <> seizeInputBuilder 0
                <> externalScriptInputBuilder
                <> seizeResidualOutputBuilder 1
                <> seizeCorrespondingOutputBuilder
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

globalSeize1ExternalScript50PubKeyCtx :: ScriptContext
globalSeize1ExternalScript50PubKeyCtx =
    mkGlobalSeizeExternalScriptAndManyPubKeyCtx manyPubKeyInputCount

directoryInitCtx :: ScriptContext
directoryInitCtx =
    let mintValue = mkValue [(directoryPolicyCS, TokenName "", 1)]
        emptyNodeDatum =
            PlutusTx.dataToBuiltinData $
                PlutusTx.List
                    [ PlutusTx.B ""
                    , PlutusTx.B (BS.replicate 30 0xff)
                    , PlutusTx.Constr 0 [PlutusTx.B ""]
                    , PlutusTx.Constr 0 [PlutusTx.B ""]
                    , PlutusTx.B ""
                    ]
     in buildScriptContext
            ( withRedeemer (PlutusTx.toBuiltinData InitDirectory)
                <> withMintingScript mintValue (PlutusTx.toBuiltinData InitDirectory)
                <> withSigner signerPkh
                <> withInput
                    ( withOutRef initRef
                        <> withAddress (pubKeyAddress signerPkh)
                        <> withValue (mkAdaValue 10_000_000)
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddress (ScriptHash (bs28 0x44)))
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue)
                        <> withTxOutInlineDatum emptyNodeDatum
                    )
                <> withOutput
                    ( withTxOutAddress (pubKeyAddress signerPkh)
                        <> withTxOutValue (mkAdaValue 8_000_000)
                    )
            )

directoryInsertCtx :: ScriptContext
directoryInsertCtx =
    let issuancePrefix = "0d"
        issuancePostfix = "0e"
        hashedMintingParam = bs28 0x55
        insertProtocolParamsDatum =
            ProgrammableLogicGlobalParams directoryPolicyCS progLogicBaseCred
        insertedCs = computeRegisteredCs issuancePrefix issuancePostfix hashedMintingParam
        insertedCsBs = case insertedCs of
            CurrencySymbol bs -> bs
        insertedToken = TokenName insertedCsBs
        insertRedeemer = InsertDirectoryNode insertedCs (ScriptHash hashedMintingParam)
        nodeMintValue = mkValue [(directoryPolicyCS, insertedToken, 1)]
        registeredAssetMintValue = mkValue [(insertedCs, TokenName "0b", 1)]
        registeredAssetRedeemer = PlutusTx.toBuiltinData (ScriptCredential (ScriptHash hashedMintingParam))
        coveringNode =
            DirectorySetNode
                (CurrencySymbol "")
                tailCS
                (ScriptCredential transferLogicHash)
                issuerCred
                (CurrencySymbol "")
        coveringOutput =
            DirectorySetNode
                (CurrencySymbol "")
                insertedCs
                (ScriptCredential transferLogicHash)
                issuerCred
                (CurrencySymbol "")
        insertedNode =
            DirectorySetNode
                insertedCs
                tailCS
                (ScriptCredential transferLogicHash)
                issuerCred
                (CurrencySymbol "")
     in buildScriptContext
            ( withRedeemer (PlutusTx.toBuiltinData insertRedeemer)
                <> withMintingScript nodeMintValue (PlutusTx.toBuiltinData insertRedeemer)
                <> withMint registeredAssetMintValue registeredAssetRedeemer
                <> withSigner signerPkh
                <> withAuxiliaryRewardingScript (ScriptCredential (ScriptHash hashedMintingParam)) registeredAssetRedeemer
                <> withScriptInput
                    (PlutusTx.toBuiltinData ())
                    ( withOutRef insertNodeInRef
                        <> withAddress (scriptAddress (ScriptHash (bs28 0x44)))
                        <> withValue (mkAdaValue 2_000_000 <> mkValue [(directoryPolicyCS, TokenName "", 1)])
                        <> withInlineDatum (PlutusTx.toBuiltinData coveringNode)
                    )
                <> withPubKeyInputValue signerPkh directoryInsertFundingRef 6_000_000
                <> withOutput
                    ( withTxOutAddress (scriptAddress (ScriptHash (bs28 0x44)))
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mkValue [(directoryPolicyCS, TokenName "", 1)])
                        <> withTxOutInlineDatum (PlutusTx.toBuiltinData coveringOutput)
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddress (ScriptHash (bs28 0x44)))
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mkValue [(directoryPolicyCS, insertedToken, 1)])
                        <> withTxOutInlineDatum (PlutusTx.toBuiltinData insertedNode)
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                        <> withTxOutValue (mkAdaValue 2_000_000 <> registeredAssetMintValue)
                    )
                <> withOutput
                    ( withTxOutAddress (pubKeyAddress signerPkh)
                        <> withTxOutValue (mkAdaValue 2_000_000)
                    )
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    (PlutusTx.toBuiltinData insertProtocolParamsDatum)
                <> withRefInputDatumValue
                    issuanceRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 2_000_000 <> mkValue [(issuancePolicyCS, issuanceCborHexToken, 1)])
                    (PlutusTx.toBuiltinData $ IssuanceCborHex issuancePrefix issuancePostfix)
            )

programmableMintCtx :: ScriptContext
programmableMintCtx =
    let scriptRedeemer = PlutusTx.toBuiltinData (ScriptCredential mintingLogicHash)
        mintValue = mkValue [(mintingPolicyCS, TokenName "0c", 1)]
     in buildBalancedScriptContext
            ( withFee 0
                <> withRedeemer scriptRedeemer
                <> withMintingScript mintValue scriptRedeemer
                <> withSigner signerPkh
                <> withAuxiliaryRewardingScript (ScriptCredential mintingLogicHash) scriptRedeemer
                <> withPubKeyInputValue signerPkh programmableMintFundingRef 4_000_000
                <> withOutput
                    ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue)
                    )
            )

programmableBurnCtx :: ScriptContext
programmableBurnCtx =
    let scriptRedeemer = PlutusTx.toBuiltinData (ScriptCredential mintingLogicHash)
        burnValue = mkValue [(mintingPolicyCS, TokenName "0c", -1)]
        remainingValue = mkValue [(mintingPolicyCS, TokenName "0c", 1)]
        burnInputValue = mkAdaValue 10_000_000 <> mkValue [(mintingPolicyCS, TokenName "0c", 2)]
        globalRedeemer = PlutusTx.toBuiltinData $ TransferAct [1] [1]
     in stripZeroChangeOutput $
            buildBalancedScriptContext
                ( withRedeemer scriptRedeemer
                    <> withMintingScript burnValue scriptRedeemer
                    <> withSigner signerPkh
                    <> withAuxiliaryRewardingScript globalCred globalRedeemer
                    <> withAuxiliaryRewardingScript (ScriptCredential transferLogicHash) (PlutusTx.toBuiltinData ())
                    <> withAuxiliaryRewardingScript (ScriptCredential mintingLogicHash) scriptRedeemer
                    <> withScriptInput
                        (PlutusTx.toBuiltinData ())
                        ( withOutRef programmableBurnInputRef
                            <> withAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                            <> withValue burnInputValue
                        )
                    <> withOutput
                        ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                            <> withTxOutValue (mkAdaValue 10_000_000 <> remainingValue)
                        )
                    <> withRefInputDatumValue
                        paramRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                        (PlutusTx.toBuiltinData protocolParamsDatum)
                    <> withRefInputDatumValue
                        directoryMintingNodeRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                        (PlutusTx.toBuiltinData directoryMintingNode)
                )

burnRedeemInputBuilder :: Integer -> ScriptContextBuilder
burnRedeemInputBuilder idx =
    withScriptInput
        (PlutusTx.toBuiltinData ())
        ( withOutRef (TxOutRef burnRedeemInputTxId idx)
            <> withAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
            <> withValue (mkAdaValue 3_000_000 <> mkValue [(mintingPolicyCS, TokenName "0c", 2)])
        )

-- | Batch redemption: burn 10 tokens collected across 10 mini-ledger UTxOs
-- (2 per input, half of each redeemed), remainder continuing at the base
-- credential. The realistic issuer redemption flow — the existing Burn bench
-- is the 1-input degenerate case.
programmableBurnRedeem10Ctx :: ScriptContext
programmableBurnRedeem10Ctx =
    let scriptRedeemer = PlutusTx.toBuiltinData (ScriptCredential mintingLogicHash)
        burnValue = mkValue [(mintingPolicyCS, TokenName "0c", -10)]
        remainingValue = mkValue [(mintingPolicyCS, TokenName "0c", 10)]
        globalRedeemer = PlutusTx.toBuiltinData $ TransferAct [1] [1]
        inputsBuilder = mconcat (map burnRedeemInputBuilder [0 .. 9])
     in stripZeroChangeOutput $
            buildBalancedScriptContext
                ( withRedeemer scriptRedeemer
                    <> withMintingScript burnValue scriptRedeemer
                    <> withSigner signerPkh
                    <> withAuxiliaryRewardingScript globalCred globalRedeemer
                    <> withAuxiliaryRewardingScript (ScriptCredential transferLogicHash) (PlutusTx.toBuiltinData ())
                    <> withAuxiliaryRewardingScript (ScriptCredential mintingLogicHash) scriptRedeemer
                    <> inputsBuilder
                    <> withOutput
                        ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                            <> withTxOutValue (mkAdaValue 30_000_000 <> remainingValue)
                        )
                    <> withRefInputDatumValue
                        paramRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                        (PlutusTx.toBuiltinData protocolParamsDatum)
                    <> withRefInputDatumValue
                        directoryMintingNodeRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                        (PlutusTx.toBuiltinData directoryMintingNode)
                )

-- | Supply top-up: mint additional tokens alongside an existing treasury
-- input. The minting validator requires the FIRST output to hold exactly the
-- minted amount, so the pre-existing treasury tokens continue in the second
-- output; the global validator checks the combined containment (input +
-- mint delta).
programmableMintTopUpCtx :: ScriptContext
programmableMintTopUpCtx =
    let scriptRedeemer = PlutusTx.toBuiltinData (ScriptCredential mintingLogicHash)
        mintValue = mkValue [(mintingPolicyCS, TokenName "0c", 5)]
        existingValue = mkValue [(mintingPolicyCS, TokenName "0c", 5)]
        globalRedeemer = PlutusTx.toBuiltinData $ TransferAct [1] [1]
     in stripZeroChangeOutput $
            buildBalancedScriptContext
                ( withRedeemer scriptRedeemer
                    <> withMintingScript mintValue scriptRedeemer
                    <> withSigner signerPkh
                    <> withAuxiliaryRewardingScript globalCred globalRedeemer
                    <> withAuxiliaryRewardingScript (ScriptCredential transferLogicHash) (PlutusTx.toBuiltinData ())
                    <> withAuxiliaryRewardingScript (ScriptCredential mintingLogicHash) scriptRedeemer
                    <> withScriptInput
                        (PlutusTx.toBuiltinData ())
                        ( withOutRef topUpInputRef
                            <> withAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                            <> withValue (mkAdaValue 6_000_000 <> existingValue)
                        )
                    -- withOutput prepends: the continuing-treasury output is
                    -- composed first (landing second), the minted-to output is
                    -- composed last (landing first, as the validator requires).
                    <> withOutput
                        ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                            <> withTxOutValue (mkAdaValue 3_000_000 <> existingValue)
                        )
                    <> withOutput
                        ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                            <> withTxOutValue (mkAdaValue 3_000_000 <> mintValue)
                        )
                    <> withRefInputDatumValue
                        paramRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                        (PlutusTx.toBuiltinData protocolParamsDatum)
                    <> withRefInputDatumValue
                        directoryMintingNodeRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                        (PlutusTx.toBuiltinData directoryMintingNode)
                )

-- | Mixed-ownership batch: five mini-ledger inputs owned by five DIFFERENT
-- stake credentials — three pubkey owners (all signing) and two script owners
-- (both invoked via withdrawals). Exercises both per-input authorization walks
-- of the input aggregation, which same-owner Spend-N fixtures never do.
-- Realistic for custodial/exchange sweep transactions.
globalTransferMixedOwners5Ctx :: ScriptContext
globalTransferMixedOwners5Ctx =
    let ownerStakes =
            [ PubKeyCredential signerPkh
            , PubKeyCredential recipientPkh
            , PubKeyCredential thirdSignerPkh
            , ScriptCredential externalAlwaysSucceedsHash
            , ScriptCredential externalAlwaysSucceedsHash2
            ]
        inputBuilder (idx, stakeCred) =
            withScriptInput
                (PlutusTx.toBuiltinData ())
                ( withOutRef (TxOutRef mixedOwnersInputTxId idx)
                    <> withAddress (scriptAddressWithStakeCredential progLogicBaseHash stakeCred)
                    <> withValue (mkAdaValue 3_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 1)])
                )
        inputsBuilder = mconcat (map inputBuilder (zip [0 ..] ownerStakes))
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData $ TransferAct [1] [])
                globalCred
                0
                <> withSigner signerPkh
                <> withSigner recipientPkh
                <> withSigner thirdSignerPkh
                <> withAuxiliaryRewardingScript (ScriptCredential transferLogicHash) (PlutusTx.toBuiltinData ())
                <> withAuxiliaryRewardingScript (ScriptCredential externalAlwaysSucceedsHash) (PlutusTx.toBuiltinData ())
                <> withAuxiliaryRewardingScript (ScriptCredential externalAlwaysSucceedsHash2) (PlutusTx.toBuiltinData ())
                <> inputsBuilder
                <> withOutput
                    ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
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

-- | Capacity showcase: 120 same-token inputs — near Aiken's per-tx memory
-- ceiling (~89% of the 14M limit) while this implementation sits at ~63%,
-- making the throughput headroom difference explicit while both still pass.
globalTransfer120Ctx :: ScriptContext
globalTransfer120Ctx = mkGlobalTransferManyCtx 120

-- | Mint inside a "busy" transaction: the full mint lands in the FIRST output
-- (the validator's single-output mint constraint) while 19 unrelated pubkey
-- outputs follow. Production analog of the Aiken @no_delegate_many_outputs@
-- issuance axis: both validators pay their output-handling cost on the same
-- output-heavy tx shape.
programmableMintBusyTxCtx :: ScriptContext
programmableMintBusyTxCtx =
    let scriptRedeemer = PlutusTx.toBuiltinData (ScriptCredential mintingLogicHash)
        mintValue = mkValue [(mintingPolicyCS, TokenName "0c", 1)]
        extraOutputBuilder =
            withOutput
                ( withTxOutAddress (pubKeyAddress signerPkh)
                    <> withTxOutValue (mkAdaValue 2_000_000)
                )
        -- withOutput prepends: compose the filler outputs FIRST so the
        -- minted-to output (composed last) stays at tx-output index 0.
        extraOutputsBuilder = mconcat (replicate 19 extraOutputBuilder)
     in buildBalancedScriptContext
            ( withFee 0
                <> withRedeemer scriptRedeemer
                <> withMintingScript mintValue scriptRedeemer
                <> withSigner signerPkh
                <> withAuxiliaryRewardingScript (ScriptCredential mintingLogicHash) scriptRedeemer
                <> withPubKeyInputValue signerPkh programmableMintFundingRef 42_000_000
                <> extraOutputsBuilder
                <> withOutput
                    ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue)
                    )
            )

-- | Mint alongside ten unrelated (pubkey reward-account) withdrawals.
-- Production analog of the Aiken @delegate_transferact_many_proofs@ issuance
-- axis: this validator's scope checks (minting-logic lookup over the
-- withdrawal list, single-mint walk over the redeemer map) and Aiken's
-- delegation detection both pay their cost on a withdrawal-cluttered tx.
programmableMintManyWithdrawalsCtx :: ScriptContext
programmableMintManyWithdrawalsCtx =
    let scriptRedeemer = PlutusTx.toBuiltinData (ScriptCredential mintingLogicHash)
        mintValue = mkValue [(mintingPolicyCS, TokenName "0c", 1)]
        unrelatedWithdrawalsBuilder =
            mconcat
                [ withWithdrawal (PubKeyCredential (PubKeyHash (bs28 w))) 0
                | w <- [0x70 .. 0x79]
                ]
     in buildBalancedScriptContext
            ( withFee 0
                <> withRedeemer scriptRedeemer
                <> withMintingScript mintValue scriptRedeemer
                <> withSigner signerPkh
                <> withAuxiliaryRewardingScript (ScriptCredential mintingLogicHash) scriptRedeemer
                <> unrelatedWithdrawalsBuilder
                <> withPubKeyInputValue signerPkh programmableMintFundingRef 4_000_000
                <> withOutput
                    ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue)
                    )
            )

protocolParamsMintCtx :: ScriptContext
protocolParamsMintCtx =
    let mintValue = mkValue [(protocolParamsCS, protocolParamsToken, 1)]
     in buildBalancedScriptContext
            ( withMintingScript mintValue (PlutusTx.toBuiltinData ())
                <> withSigner signerPkh
                <> withInput
                    ( withOutRef protocolParamsInitRef
                        <> withAddress (pubKeyAddress signerPkh)
                        <> withValue (mkAdaValue 4_000_000)
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddress protocolParamsAlwaysFailHash)
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue)
                        <> withTxOutInlineDatum (PlutusTx.toBuiltinData protocolParamsDatum)
                    )
            )

issuanceMintCtx :: ScriptContext
issuanceMintCtx =
    let mintValue = mkValue [(issuancePolicyCS, issuanceCborHexToken, 1)]
     in buildBalancedScriptContext
            ( withMintingScript mintValue (PlutusTx.toBuiltinData ())
                <> withSigner signerPkh
                <> withInput
                    ( withOutRef issuanceInitRef
                        <> withAddress (pubKeyAddress signerPkh)
                        <> withValue (mkAdaValue 4_000_000)
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddress issuanceAlwaysFailHash)
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue)
                        <> withTxOutInlineDatum issuanceDatum
                    )
            )

-- Mainnet-inspired DEX transaction benchmark fixture from:
-- c3111df78f97a8f7ed4934510e04d9c777339a00d5eac26e123471ff4ab954ff
--
-- The original tx processes 16 swap requests against a NIGHT/ADA pool. Here we
-- model the same broad shape on the programmable-token mini-ledger:
-- - 16 request inputs at programmableLogicBase with an always-succeeds stake script
-- - 1 pool input at programmableLogicBase with a second always-succeeds stake script
-- - 1 pubkey input that is consumed entirely as fees
-- - 16 request outputs at programmableLogicBase with pubkey stake credentials
-- - 1 pool continuation output carrying the pool state NFT
mainnetDexSwapStakeCred :: Credential
mainnetDexSwapStakeCred = ScriptCredential externalAlwaysSucceedsHash

mainnetDexPoolStakeCred :: Credential
mainnetDexPoolStakeCred = ScriptCredential externalAlwaysSucceedsHash2

mainnetDexSwapInputAddr :: Address
mainnetDexSwapInputAddr =
    scriptAddressWithStakeCredential progLogicBaseHash mainnetDexSwapStakeCred

mainnetDexPoolAddr :: Address
mainnetDexPoolAddr =
    scriptAddressWithStakeCredential progLogicBaseHash mainnetDexPoolStakeCred

mainnetDexSwapInputBuilder :: Integer -> Integer -> ScriptContextBuilder
mainnetDexSwapInputBuilder idx nightQty =
    withScriptInput
        (PlutusTx.toBuiltinData ())
        ( withOutRef (TxOutRef mainnetDexSwapInputTxId idx)
            <> withAddress mainnetDexSwapInputAddr
            <> withValue
                ( mkAdaValue (fromIntegral mainnetDexSwapInputAda)
                    <> mkValue [(programmableTransferCS, mainnetDexNightToken, nightQty)]
                )
        )

mainnetDexPoolInputBuilder :: ScriptContextBuilder
mainnetDexPoolInputBuilder =
    withScriptInput
        (PlutusTx.toBuiltinData ())
        ( withOutRef mainnetDexPoolInputRef
            <> withAddress mainnetDexPoolAddr
            <> withValue
                ( mkAdaValue (fromIntegral mainnetDexPoolInputAda)
                    <> mkValue
                        [ (programmableTransferCS, mainnetDexNightToken, mainnetDexPoolInputNight)
                        , (nonProgrammableCS, mainnetDexPoolStateToken, 1)
                        ]
                )
        )

mainnetDexFeeInputBuilder :: ScriptContextBuilder
mainnetDexFeeInputBuilder =
    withInput
        ( withOutRef mainnetDexFeeInputRef
            <> withAddress (pubKeyAddress signerPkh)
            <> withValue (mkAdaValue (fromIntegral mainnetDexFeeAda))
        )

mainnetDexSwapOutputBuilder :: Integer -> Integer -> Integer -> ScriptContextBuilder
mainnetDexSwapOutputBuilder idx adaQty nightQty =
    withOutput $
        withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash (mainnetDexOutputStakePkhAt idx))
            <> withTxOutValue
                ( mkAdaValue (fromIntegral adaQty)
                    <> mkValue [(programmableTransferCS, mainnetDexNightToken, nightQty) | nightQty > 0]
                )

mainnetDexPoolOutputBuilder :: ScriptContextBuilder
mainnetDexPoolOutputBuilder =
    withOutput $
        withTxOutAddress mainnetDexPoolAddr
            <> withTxOutValue
                ( mkAdaValue (fromIntegral mainnetDexPoolOutputAda)
                    <> mkValue
                        [ (programmableTransferCS, mainnetDexNightToken, mainnetDexPoolOutputNight)
                        , (nonProgrammableCS, mainnetDexPoolStateToken, 1)
                        ]
                )

mainnetDexGlobalTransferCtx :: ScriptContext
mainnetDexGlobalTransferCtx =
    let swapInputsBuilder =
            mconcat $
                zipWith
                    mainnetDexSwapInputBuilder
                    [0 .. 15]
                    mainnetDexSwapInputNightQtys
        swapOutputsBuilder =
            mconcat $
                zipWith3
                    mainnetDexSwapOutputBuilder
                    [0 .. 15]
                    mainnetDexSwapOutputAdaQtys
                    mainnetDexSwapOutputNightQtys
     in stripZeroChangeOutput $
            buildBalancedScriptContext
                ( withFee mainnetDexFeeAda
                    <> withRewardingScript
                        -- Proofs are positional over the aggregated programmable input
                        -- value in CANONICAL (ledger) currency-symbol order:
                        -- nonProgrammableCS (0x1a) then programmableTransferCS (0x1b).
                        -- So proof[0]=1 (covering/does-not-exist for 0x1a) and
                        -- proof[1]=2 (exists for the registered 0x1b node).
                        (PlutusTx.toBuiltinData $ TransferAct [1, 2] [])
                        globalCred
                        0
                    <> withSigner signerPkh
                    <> withAuxiliaryRewardingScript (ScriptCredential transferLogicHash) (PlutusTx.toBuiltinData ())
                    <> withAuxiliaryRewardingScript mainnetDexSwapStakeCred (PlutusTx.toBuiltinData ())
                    <> withAuxiliaryRewardingScript mainnetDexPoolStakeCred (PlutusTx.toBuiltinData ())
                    <> swapInputsBuilder
                    <> mainnetDexPoolInputBuilder
                    <> mainnetDexFeeInputBuilder
                    <> swapOutputsBuilder
                    <> mainnetDexPoolOutputBuilder
                    <> withRefInputDatumValue
                        paramRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                        (PlutusTx.toBuiltinData protocolParamsDatum)
                    <> withRefInputDatumValue
                        dirNodeRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                        (PlutusTx.toBuiltinData directoryCoveringNode)
                    <> withRefInputDatumValue
                        directoryProgrammableNodeRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                        (PlutusTx.toBuiltinData directoryProgrammableNode)
                )

mainnetDexBaseSpendingCtx :: ScriptContext
mainnetDexBaseSpendingCtx =
    let ScriptContext txInfo _ _ = mainnetDexGlobalTransferCtx
     in ScriptContext
            txInfo
            (Redeemer (PlutusTx.toBuiltinData ()))
            (SpendingScript mainnetDexBaseInputRef Nothing)

-- Exact transaction benchmark fixture from:
-- https://preview.cexplorer.io/tx/d29ce2a9f79a70a91d83a40e0e1cf346ab94979b6f0ba001de8a89895aa518df?tab=content
-- The context is reconstructed from chain data (inputs/outputs/ref-inputs/redeemers/withdrawals).
-- Tx d29c... replay fixture retained to benchmark a realistic spending path.
txD29GlobalStakeRedeemer :: BuiltinData
txD29GlobalStakeRedeemer =
    PlutusTx.toBuiltinData $ TransferAct [2] []

txD29ProtocolParamsDatumData :: BuiltinData
txD29ProtocolParamsDatumData =
    PlutusTx.dataToBuiltinData $
        PlutusTx.List
            [ PlutusTx.toData txD29RegistryNodeCS
            , PlutusTx.toData txD29BaseScriptCred
            ]

txD29RefDatum1Data :: BuiltinData
txD29RefDatum1Data =
    PlutusTx.dataToBuiltinData $
        PlutusTx.List
            [ PlutusTx.toData txD29IssuancePrefix
            , PlutusTx.toData txD29IssuancePostfix
            ]

txD29DirectoryNodeDatumData :: BuiltinData
txD29DirectoryNodeDatumData =
    PlutusTx.dataToBuiltinData $
        PlutusTx.List
            [ PlutusTx.toData txD29ProgrammableAssetCS
            , PlutusTx.toData txD29DirectoryNodeNext
            , PlutusTx.toData txD29TransferLogicStakeCred
            , PlutusTx.toData txD29DirectoryNodeIssuerCred
            ]

txD29Payloads :: TxD29Payloads
txD29Payloads =
    TxD29Payloads
        { txD29PayloadDirectoryNodeDatumData = txD29DirectoryNodeDatumData
        , txD29PayloadGlobalStakeRedeemer = txD29GlobalStakeRedeemer
        , txD29PayloadProtocolParamsDatumData = txD29ProtocolParamsDatumData
        , txD29PayloadRefDatum1Data = txD29RefDatum1Data
        }

txD29ProgrammableLogicBaseSpendingCtx :: ScriptContext
txD29ProgrammableLogicBaseSpendingCtx =
    mkTxD29ProgrammableLogicBaseSpendingCtx txD29Payloads

txD29ProgrammableLogicBaseStakeCtx :: ScriptContext
txD29ProgrammableLogicBaseStakeCtx =
    mkTxD29ProgrammableLogicBaseStakeCtx txD29Payloads

-- Benchmark catalogue: keep this list close to the contexts it exercises.
benchCases :: [BenchCase]
benchCases =
    [ mkCase "programmableLogicBase" (EvalBaseSpend progInputRef) mkProgrammableLogicBase [toData globalCred, toData baseSpendingCtx] baseSpendingCtx
    , mkCase "programmableLogicGlobal.TransferAct" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransferCtx] globalTransferCtx
    , mkCase "programmableLogicGlobal.TransferAct.TokenDoesNotExist" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransferDoesNotExistCtx] globalTransferDoesNotExistCtx
    , mkCase "programmableLogicGlobal.TransferAct.MixedMany" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransferMixedManyCtx] globalTransferMixedManyCtx
    , mkCase "programmableLogicGlobal.TransferAct.ManyTokens50" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransferManyTokens50Ctx] globalTransferManyTokens50Ctx
    , mkCase "programmableLogicGlobal.TransferAct.ManyOutputs20" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransferManyOutputs20Ctx] globalTransferManyOutputs20Ctx
    , mkCase "programmableLogicGlobal.TransferAct.ManyPolicies10" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransferManyPolicies10Ctx] globalTransferManyPolicies10Ctx
    , mkCase "programmableLogicGlobal.TransferAct.Spend5Utxos" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransfer5Ctx] globalTransfer5Ctx
    , mkCase "programmableLogicGlobal.TransferAct.Spend10Utxos" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransfer10Ctx] globalTransfer10Ctx
    , mkCase "programmableLogicGlobal.TransferAct.Spend15Utxos" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransfer15Ctx] globalTransfer15Ctx
    , mkCase "programmableLogicGlobal.TransferAct.Spend25Utxos" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransfer25Ctx] globalTransfer25Ctx
    , mkCase "programmableLogicGlobal.TransferAct.Spend50Utxos" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransfer50Ctx] globalTransfer50Ctx
    , mkCase "programmableLogicGlobal.TransferAct.Spend100Utxos" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransfer100Ctx] globalTransfer100Ctx
    , mkCase "programmableLogicGlobal.TransferAct.Spend120Utxos" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransfer120Ctx] globalTransfer120Ctx
    , mkCase "programmableLogicGlobal.TransferAct.MixedOwners5" (EvalGlobalReward globalCred) mkProgrammableLogicGlobal [toData protocolParamsCS, toData globalTransferMixedOwners5Ctx] globalTransferMixedOwners5Ctx
    , mkCase "programmableLogicGlobal.SeizeAct1" (EvalAlwaysSucceedsReward "programmableSeize" seizeCredBench) mkProgrammableSeize [toData protocolParamsCS, toData globalSeize1Ctx] globalSeize1Ctx
    , mkCase
        "programmableLogicGlobal.SeizeAct1.ExternalScriptAnd50PubKeyInputs"
        (EvalAlwaysSucceedsReward "programmableSeize" seizeCredBench)
        mkProgrammableSeize
        [toData protocolParamsCS, toData globalSeize1ExternalScript50PubKeyCtx]
        globalSeize1ExternalScript50PubKeyCtx
    , mkCase "programmableLogicGlobal.SeizeAct5" (EvalAlwaysSucceedsReward "programmableSeize" seizeCredBench) mkProgrammableSeize [toData protocolParamsCS, toData globalSeize5Ctx] globalSeize5Ctx
    , mkCase "programmableLogicGlobal.SeizeAct10" (EvalAlwaysSucceedsReward "programmableSeize" seizeCredBench) mkProgrammableSeize [toData protocolParamsCS, toData globalSeize10Ctx] globalSeize10Ctx
    , mkCase "programmableLogicGlobal.SeizeAct20" (EvalAlwaysSucceedsReward "programmableSeize" seizeCredBench) mkProgrammableSeize [toData protocolParamsCS, toData globalSeize20Ctx] globalSeize20Ctx
    , mkCase "programmableLogicGlobal.SeizeAct50" (EvalAlwaysSucceedsReward "programmableSeize" seizeCredBench) mkProgrammableSeize [toData protocolParamsCS, toData globalSeize50Ctx] globalSeize50Ctx
    , mkCase "programmableLogicGlobal.SeizeAct100" (EvalAlwaysSucceedsReward "programmableSeize" seizeCredBench) mkProgrammableSeize [toData protocolParamsCS, toData globalSeize100Ctx] globalSeize100Ctx
    , mkCase "programmableLogicGlobal.SeizeAct150" (EvalAlwaysSucceedsReward "programmableSeize" seizeCredBench) mkProgrammableSeize [toData protocolParamsCS, toData globalSeize150Ctx] globalSeize150Ctx
    , mkCase "programmableLogicGlobal.SeizeAct2.PartialSeizeWithNoise" (EvalAlwaysSucceedsReward "programmableSeize" seizeCredBench) mkProgrammableSeize [toData protocolParamsCS, toData globalSeizeNoiseCtx] globalSeizeNoiseCtx
    , mkCase "directoryNodeMinting.InitDirectory" (EvalDirectoryMint directoryPolicyCS) mkDirectoryNodeMP [toData initRef, toData issuancePolicyCS, toData directoryInitCtx] directoryInitCtx
    , mkCase "directoryNodeMinting.InsertDirectoryNode" (EvalDirectoryMint directoryPolicyCS) mkDirectoryNodeMP [toData initRef, toData issuancePolicyCS, toData directoryInsertCtx] directoryInsertCtx
    , mkCase "programmableLogicMinting.Mint" (EvalProgrammableMint mintingPolicyCS) mkProgrammableLogicMinting [toData progLogicBaseCred, toData mintingLogicHash, toData programmableMintCtx] programmableMintCtx
    , mkCase "programmableLogicMinting.Mint.BusyTx20Outputs" (EvalProgrammableMint mintingPolicyCS) mkProgrammableLogicMinting [toData progLogicBaseCred, toData mintingLogicHash, toData programmableMintBusyTxCtx] programmableMintBusyTxCtx
    , mkCase "programmableLogicMinting.Mint.TenUnrelatedWithdrawals" (EvalProgrammableMint mintingPolicyCS) mkProgrammableLogicMinting [toData progLogicBaseCred, toData mintingLogicHash, toData programmableMintManyWithdrawalsCtx] programmableMintManyWithdrawalsCtx
    , mkCase "programmableLogicMinting.Mint.TopUpExistingTreasury" (EvalProgrammableMint mintingPolicyCS) mkProgrammableLogicMinting [toData progLogicBaseCred, toData mintingLogicHash, toData programmableMintTopUpCtx] programmableMintTopUpCtx
    , mkCase "programmableLogicMinting.Burn" (EvalProgrammableMint mintingPolicyCS) mkProgrammableLogicMinting [toData progLogicBaseCred, toData mintingLogicHash, toData programmableBurnCtx] programmableBurnCtx
    , mkCase "programmableLogicMinting.Burn.Redeem10Utxos" (EvalProgrammableMint mintingPolicyCS) mkProgrammableLogicMinting [toData progLogicBaseCred, toData mintingLogicHash, toData programmableBurnRedeem10Ctx] programmableBurnRedeem10Ctx
    , mkCase "protocolParamsMinting" (EvalProtocolParamsMint protocolParamsCS) mkProtocolParametersMinting [toData protocolParamsInitRef, toData protocolParamsMintCtx] protocolParamsMintCtx
    , mkCase "issuanceCborHexMinting" (EvalIssuanceMint issuancePolicyCS) mkIssuanceCborHexMinting [toData issuanceInitRef, toData issuanceMintCtx] issuanceMintCtx
    , mkCase
        ("programmableLogicBase.Tx." <> take 8 mainnetDexTxHash <> ".Spending")
        (EvalBaseSpend mainnetDexBaseInputRef)
        mkProgrammableLogicBase
        [ toData globalCred
        , toData mainnetDexBaseSpendingCtx
        ]
        mainnetDexBaseSpendingCtx
    , mkCase
        ("programmableLogicGlobal.TransferAct.Tx." <> take 8 mainnetDexTxHash <> ".NightAdaDex16Swaps")
        (EvalGlobalReward globalCred)
        mkProgrammableLogicGlobal
        [ toData protocolParamsCS
        , toData mainnetDexGlobalTransferCtx
        ]
        mainnetDexGlobalTransferCtx
    , mkCase
        ("programmableLogicBase.Tx." <> take 8 txD29Hash <> ".Spending")
        (EvalBaseSpend txD29ScriptInputRef)
        mkProgrammableLogicBase
        [ toData txD29GlobalStakeCred
        , toData txD29ProgrammableLogicBaseSpendingCtx
        ]
        txD29ProgrammableLogicBaseSpendingCtx
    , mkCase
        ("programmableLogicBase.Tx." <> take 8 txD29Hash <> ".Stake")
        (EvalGlobalReward txD29GlobalStakeCred)
        mkProgrammableLogicGlobal
        [ toData txD29ProtocolParamsCS
        , toData txD29ProgrammableLogicBaseStakeCtx
        ]
        txD29ProgrammableLogicBaseStakeCtx
    ]
