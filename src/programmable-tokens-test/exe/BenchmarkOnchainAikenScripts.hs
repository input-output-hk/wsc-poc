{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BenchmarkOnchain.CardanoScriptHelpers (scriptHashFromCardanoScript)
import BenchmarkOnchain.MainnetDexFixture
import BenchmarkOnchain.ScriptFixtureIds
import BenchmarkOnchain.ScriptHelpers (bs28, mkValue, pubKeyAddress, scriptAddress, scriptAddressWithSignerStake, scriptAddressWithStakeCredential, stripZeroChangeOutput, withAuxiliaryRewardingScript, withPubKeyInputValue, withRefInputDatumValue)
import BenchmarkOnchain.ScriptRunner (BenchCase, EvalKind (..), EvalSpec (..), mkBenchCase, runScriptBenchmark)
import BenchmarkOnchain.ScriptScenario qualified as Scenario
import BenchmarkOnchain.TxD29Fixture
import Cardano.Api qualified as C
import Data.ByteString qualified as BS
import Plutarch.Script (Script, deserialiseScript)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V3 hiding (deserialiseScript)
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import ProgrammableTokens.OffChain.AikenProgrammableTokenScripts qualified as Aiken
import ProgrammableTokens.OffChain.Scripts qualified as OffchainScripts
import ProgrammableTokens.Test.ScriptContext.Builder (ScriptContextBuilder, buildBalancedScriptContext, mkAdaValue, withAddress, withFee, withInlineDatum, withInput, withMint, withMintingScript, withOutRef, withOutput, withRedeemer, withRewardingScript, withScriptInput, withSigner, withTxOutAddress, withTxOutInlineDatum, withTxOutValue, withValue, withWithdrawal)
import SmartTokens.Contracts.ProgrammableLogicBase (mkSeizeActRedeemerFromAbsoluteInputIdxs)
import SmartTokens.Core.Scripts (ScriptTarget (Production))
import SmartTokens.LinkedList.MintDirectory (DirectoryNodeAction (InsertDirectoryNode))
import SmartTokens.Types.Constants (issuanceCborHexToken, protocolParamsToken)

data AikenTokenProof
    = TokenExists Integer
    | TokenDoesNotExist Integer

main :: IO ()
main =
    runScriptBenchmark
        "Onchain Aiken script benchmark (NoTracing, one case per redeemer path)"
        benchCases
        scenarioEvalSpecsFromCtx

compiledAlwaysSucceedsScript :: Script
compiledAlwaysSucceedsScript =
    cardanoApiScriptToScript (OffchainScripts.alwaysSucceedsScript Production)

mkAikenCase :: String -> EvalKind -> C.PlutusScript C.PlutusScriptV3 -> [Data] -> ScriptContext -> BenchCase
mkAikenCase name primaryKind script args ctx =
    mkBenchCase name primaryKind (cardanoApiScriptToScript script) args ctx

cardanoApiScriptToScript :: C.PlutusScript C.PlutusScriptV3 -> Script
cardanoApiScriptToScript (C.PlutusScriptSerialised serialisedBytes) =
    deserialiseScript serialisedBytes

aikenMintArgs :: ScriptContext -> [Data]
aikenMintArgs ctx = [toData ctx]

aikenRewardArgs :: ScriptContext -> [Data]
aikenRewardArgs ctx = [toData ctx]

aikenSpendArgs :: TxOutRef -> ScriptContext -> [Data]
aikenSpendArgs _ ctx = [toData ctx]

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
                    (cardanoApiScriptToScript (Aiken.aikenProgrammableLogicBaseScript globalStakeCred))
                    (aikenSpendArgs outRef purposeCtx)
        , Scenario.ssbDirectoryMintSpec =
            \env cs purposeCtx ->
                EvalSpec
                    (EvalDirectoryMint cs)
                    (cardanoApiScriptToScript (Aiken.aikenDirectoryNodeMintingScript (Scenario.sseInitRef env) (Scenario.sseIssuanceInitRef env)))
                    (aikenMintArgs purposeCtx)
        , Scenario.ssbDirectorySpendSpec =
            \_ paramsCs outRef purposeCtx ->
                EvalSpec
                    (EvalDirectorySpend outRef)
                    (cardanoApiScriptToScript (Aiken.aikenDirectoryNodeSpendingScript paramsCs))
                    (aikenSpendArgs outRef purposeCtx)
        , Scenario.ssbGlobalRewardSpec =
            \_ paramsCs cred purposeCtx ->
                EvalSpec
                    (EvalGlobalReward cred)
                    (cardanoApiScriptToScript (Aiken.aikenProgrammableLogicGlobalScript paramsCs))
                    (aikenRewardArgs purposeCtx)
        , -- Aiken performs seize monolithically inside its global validator, so
          -- there is no separate seize script and this hook is never invoked (the
          -- Aiken seize contexts carry no seize withdrawal). Provided only to
          -- satisfy the shared backend record.
          Scenario.ssbSeizeRewardSpec =
            \_ paramsCs cred purposeCtx ->
                EvalSpec
                    (EvalGlobalReward cred)
                    (cardanoApiScriptToScript (Aiken.aikenProgrammableLogicGlobalScript paramsCs))
                    (aikenRewardArgs purposeCtx)
        , Scenario.ssbIssuanceMintSpec =
            \env cs purposeCtx ->
                EvalSpec
                    (EvalIssuanceMint cs)
                    (cardanoApiScriptToScript (Aiken.aikenIssuanceCborHexMintingScript (Scenario.sseIssuanceInitRef env)))
                    (aikenMintArgs purposeCtx)
        , Scenario.ssbProgrammableMintSpec =
            \env cs purposeCtx ->
                EvalSpec
                    (EvalProgrammableMint cs)
                    (cardanoApiScriptToScript (Aiken.aikenProgrammableLogicMintingScript (Scenario.sseProgLogicBaseCred env) (ScriptCredential (Scenario.sseMintingLogicHash env))))
                    (aikenMintArgs purposeCtx)
        , Scenario.ssbProtocolParamsMintSpec =
            \env cs purposeCtx ->
                EvalSpec
                    (EvalProtocolParamsMint cs)
                    (cardanoApiScriptToScript (Aiken.aikenProtocolParamsMintingScript (Scenario.sseProtocolParamsInitRef env)))
                    (aikenMintArgs purposeCtx)
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
        , -- No separate Aiken seize script; this credential never appears in an
          -- Aiken seize context's withdrawals, so the seize hook is never taken.
          Scenario.sseSeizeCred = ScriptCredential (ScriptHash (bs28 0x40))
        , Scenario.sseTransferLogicHash = transferLogicHash
        , Scenario.sseTxD29BaseScriptCred = txD29BaseScriptCred
        , Scenario.sseTxD29GlobalStakeCred = txD29GlobalStakeCred
        , Scenario.sseTxD29ProtocolParamsCS = txD29ProtocolParamsCS
        , Scenario.sseTxD29TransferLogicStakeCred = txD29TransferLogicStakeCred
        }

-- Aiken-specific datum encoders used by the synthetic benchmark fixtures below.
aikenConstr :: Integer -> [PlutusTx.Data] -> BuiltinData
aikenConstr tag fields =
    PlutusTx.dataToBuiltinData (PlutusTx.Constr tag fields)

aikenProtocolParamsDatumData :: CurrencySymbol -> Credential -> BuiltinData
aikenProtocolParamsDatumData registryNodeCs progLogicCredential =
    aikenConstr
        0
        [ PlutusTx.toData registryNodeCs
        , PlutusTx.toData progLogicCredential
        ]

aikenIssuanceDatumData :: BuiltinByteString -> BuiltinByteString -> BuiltinData
aikenIssuanceDatumData prefix postfix =
    aikenConstr
        0
        [ PlutusTx.toData prefix
        , PlutusTx.toData postfix
        ]

aikenCurrencySymbolBytes :: CurrencySymbol -> BuiltinByteString
aikenCurrencySymbolBytes (CurrencySymbol bs) = bs

aikenDirectoryNodeDatumData ::
    BuiltinByteString ->
    BuiltinByteString ->
    Credential ->
    Credential ->
    BuiltinByteString ->
    BuiltinData
aikenDirectoryNodeDatumData key next transferLogicScript issuerLogicScript globalStateCs =
    aikenConstr
        0
        [ PlutusTx.toData key
        , PlutusTx.toData next
        , PlutusTx.toData transferLogicScript
        , PlutusTx.toData issuerLogicScript
        , PlutusTx.toData globalStateCs
        ]

aikenTransferProofData :: AikenTokenProof -> PlutusTx.Data
aikenTransferProofData = \case
    TokenExists nodeIdx ->
        PlutusTx.Constr 0 [PlutusTx.I nodeIdx]
    TokenDoesNotExist nodeIdx ->
        PlutusTx.Constr 1 [PlutusTx.I nodeIdx]

aikenTransferActRedeemerData :: [AikenTokenProof] -> BuiltinData
aikenTransferActRedeemerData proofs =
    aikenConstr 0 [PlutusTx.List (fmap aikenTransferProofData proofs)]

aikenMintingActionRedeemerData :: Credential -> BuiltinData
aikenMintingActionRedeemerData mintingLogicCredential =
    aikenConstr 0 [PlutusTx.toData mintingLogicCredential]

policyIdCurrencySymbol :: C.PolicyId -> CurrencySymbol
policyIdCurrencySymbol =
    CurrencySymbol . PV1.toBuiltin . C.serialiseToRawBytes

scriptPolicyCurrencySymbol :: C.PlutusScript C.PlutusScriptV3 -> CurrencySymbol
scriptPolicyCurrencySymbol =
    policyIdCurrencySymbol
        . C.scriptPolicyId
        . C.PlutusScript C.PlutusScriptV3

computeRegisteredCs :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> CurrencySymbol
computeRegisteredCs prefix postfix hashedParam =
    CurrencySymbol $
        BI.blake2b_224
            ( versionHeader
                <> prefix
                <> hashedParam
                <> postfix
            )
  where
    versionHeader = PV1.toBuiltin (BS.pack [3])

tailNodeBs :: BuiltinByteString
tailNodeBs = PV1.toBuiltin (BS.replicate 30 0xff)

protocolParamsDatum :: BuiltinData
protocolParamsDatum =
    aikenProtocolParamsDatumData directoryNodeCS progLogicBaseCred

directoryCoveringNode :: BuiltinData
directoryCoveringNode =
    aikenDirectoryNodeDatumData
        ""
        tailNodeBs
        (ScriptCredential transferLogicHash)
        issuerCred
        ""

directoryProgrammableNode :: BuiltinData
directoryProgrammableNode =
    aikenDirectoryNodeDatumData
        (aikenCurrencySymbolBytes programmableTransferCS)
        tailNodeBs
        (ScriptCredential transferLogicHash)
        issuerCred
        ""

directoryMintingNode :: BuiltinData
directoryMintingNode =
    aikenDirectoryNodeDatumData
        (aikenCurrencySymbolBytes mintingPolicyCS)
        tailNodeBs
        (ScriptCredential transferLogicHash)
        issuerCred
        ""

directoryProgrammableNode2 :: BuiltinData
directoryProgrammableNode2 =
    aikenDirectoryNodeDatumData
        (aikenCurrencySymbolBytes programmableTransferCS2)
        tailNodeBs
        (ScriptCredential transferLogicHash)
        issuerCred
        ""

directoryProgrammableNode3 :: BuiltinData
directoryProgrammableNode3 =
    aikenDirectoryNodeDatumData
        (aikenCurrencySymbolBytes programmableTransferCS3)
        tailNodeBs
        (ScriptCredential transferLogicHash)
        issuerCred
        ""

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
            (aikenTransferActRedeemerData [TokenExists 1])
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
                protocolParamsDatum
            <> withRefInputDatumValue
                dirNodeRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                directoryProgrammableNode
        )

globalTransferDoesNotExistCtx :: ScriptContext
globalTransferDoesNotExistCtx =
    buildBalancedScriptContext
        ( withRewardingScript
            (aikenTransferActRedeemerData [TokenDoesNotExist 1])
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
                protocolParamsDatum
            <> withRefInputDatumValue
                dirNodeRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                directoryCoveringNode
        )

globalTransferMixedManyCtx :: ScriptContext
globalTransferMixedManyCtx =
    buildBalancedScriptContext
        ( withRewardingScript
            (aikenTransferActRedeemerData [TokenDoesNotExist 1, TokenExists 2, TokenExists 3, TokenExists 4, TokenDoesNotExist 1])
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
                protocolParamsDatum
            <> withRefInputDatumValue
                dirNodeRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                directoryCoveringNode
            <> withRefInputDatumValue
                directoryProgrammableNodeRef
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                directoryProgrammableNode
            <> withRefInputDatumValue
                directoryProgrammableNode2Ref
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                directoryProgrammableNode2
            <> withRefInputDatumValue
                directoryProgrammableNode3Ref
                (pubKeyAddress signerPkh)
                (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                directoryProgrammableNode3
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
                (aikenTransferActRedeemerData [TokenDoesNotExist 1, TokenExists 2])
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
                    protocolParamsDatum
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    directoryCoveringNode
                <> withRefInputDatumValue
                    directoryProgrammableNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    directoryProgrammableNode
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

-- | Mirrors the Aiken bench @many_tokens@ axis: a single mini-ledger UTxO
-- carrying @tokenCount@ asset names under one programmable policy moves
-- wholesale to a single recipient output.
mkGlobalTransferManyTokensCtx :: Integer -> ScriptContext
mkGlobalTransferManyTokensCtx tokenCount =
    let manyTokensValue = mkValue [(programmableTransferCS, manyTokensTokenName i, 2) | i <- [0 .. (tokenCount - 1)]]
     in buildBalancedScriptContext
            ( withRewardingScript
                (aikenTransferActRedeemerData [TokenExists 1])
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
                    protocolParamsDatum
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    directoryProgrammableNode
            )

globalTransferManyTokens50Ctx :: ScriptContext
globalTransferManyTokens50Ctx = mkGlobalTransferManyTokensCtx manyTokensCount

-- | Mirrors the Aiken bench @many_outputs@ axis: a single mini-ledger input
-- fans out to @outputCount@ mini-ledger recipient outputs (airdrop shape).
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
                (aikenTransferActRedeemerData [TokenExists 1])
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
                    protocolParamsDatum
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    directoryProgrammableNode
            )

globalTransferManyOutputs20Ctx :: ScriptContext
globalTransferManyOutputs20Ctx = mkGlobalTransferManyOutputsCtx manyOutputsCount

-- | Directory-node datum for the synthetic programmable policy at index @i@
-- (see 'manyPolicyCS'), used by 'mkGlobalTransferManyPoliciesCtx'.
manyPolicyNodeDatum :: Integer -> BuiltinData
manyPolicyNodeDatum i =
    aikenDirectoryNodeDatumData
        (aikenCurrencySymbolBytes (manyPolicyCS i))
        tailNodeBs
        (ScriptCredential transferLogicHash)
        issuerCred
        ""

-- | Mirrors the Aiken bench @many_policies@ axis: one input carries
-- @policyCount@ distinct programmable policies, each needing its own
-- positional proof and its own directory-node reference input (worst-case
-- proof-list walk).
mkGlobalTransferManyPoliciesCtx :: Integer -> ScriptContext
mkGlobalTransferManyPoliciesCtx policyCount =
    let idxs = [0 .. (policyCount - 1)]
        manyPoliciesValue = mkValue [(manyPolicyCS i, TokenName "0c", 1) | i <- idxs]
        -- Reference inputs are insertion-sorted by TxOutRef in the builder
        -- (composition order is NOT what places them). paramRef's txId (0xaa…)
        -- sorts before every manyPolicyNodeRef txId (0xbb 0x20+i…), and the
        -- node txIds ascend with i, so paramRef sits at ref index 0 and the
        -- node for policy i lands at ref index 1 + i, matching the positional
        -- proofs. Adding a ref input with a differently sorting TxOutRef would
        -- shift these indices.
        policyNodeRefsBuilder =
            mconcat
                [ withRefInputDatumValue
                    (manyPolicyNodeRef i)
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    (manyPolicyNodeDatum i)
                | i <- idxs
                ]
     in buildBalancedScriptContext
            ( withRewardingScript
                (aikenTransferActRedeemerData [TokenExists (1 + i) | i <- idxs])
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
                    protocolParamsDatum
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
                    globalCred
                    0
                    <> withAuxiliaryRewardingScript issuerCred (PlutusTx.toBuiltinData ())
                    <> seizeInputsBuilder
                    <> seizeResidualOutputBuilder seizeInputCount
                    <> correspondingOutputsBuilder
                    <> withRefInputDatumValue
                        paramRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                        protocolParamsDatum
                    <> withRefInputDatumValue
                        dirNodeRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                        directoryProgrammableNode
                )

globalSeize1Ctx :: ScriptContext
globalSeize1Ctx = mkGlobalSeizeCtx 1

globalSeize5Ctx :: ScriptContext
globalSeize5Ctx = mkGlobalSeizeCtx 5

globalSeize10Ctx :: ScriptContext
globalSeize10Ctx = mkGlobalSeizeCtx 10

globalSeize20Ctx :: ScriptContext
globalSeize20Ctx = mkGlobalSeizeCtx 20

globalSeize50Ctx :: ScriptContext
globalSeize50Ctx = mkGlobalSeizeCtx 50

globalSeize100Ctx :: ScriptContext
globalSeize100Ctx = mkGlobalSeizeCtx 100

globalSeize150Ctx :: ScriptContext
globalSeize150Ctx = mkGlobalSeizeCtx 150

-- | Mirrors the Aiken bench @baseline_3rd_party_2@ shape: two mini-ledger
-- inputs carrying unrelated noise tokens beside the seized policy, with input
-- 0 only partially seized (1 of its 2 seized tokens kept by the owner).
globalSeizeNoiseCtx :: ScriptContext
globalSeizeNoiseCtx =
    let seizeRedeemer = mkSeizeActRedeemerFromAbsoluteInputIdxs 1 [0, 1] 0
     in stripZeroChangeOutput $
            buildBalancedScriptContext
                ( withRewardingScript
                    (PlutusTx.toBuiltinData seizeRedeemer)
                    globalCred
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
                    -- withOutput prepends, so the residual is composed first to
                    -- land the final tx-output order at
                    -- [paired-with-in0, paired-with-in1, residual].
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
                        protocolParamsDatum
                    <> withRefInputDatumValue
                        dirNodeRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                        directoryProgrammableNode
                )

mkGlobalSeizeExternalScriptAndManyPubKeyCtx :: Integer -> ScriptContext
mkGlobalSeizeExternalScriptAndManyPubKeyCtx pubKeyInputCount =
    let pubKeyInputIdxs = [0 .. (pubKeyInputCount - 1)]
        seizeRedeemer =
            mkSeizeActRedeemerFromAbsoluteInputIdxs
                1
                -- The Aiken ThirdPartyAct validator only allows indexed inputs to
                -- be programmable inputs. The external script spend is present in
                -- the transaction, but it is not listed in input_idxs.
                [pubKeyInputCount]
                0
        pubKeyInputsBuilder = mconcat (map leadingPubKeyInputBuilder pubKeyInputIdxs)
     in buildBalancedScriptContext
            ( withRewardingScript
                (PlutusTx.toBuiltinData seizeRedeemer)
                globalCred
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
                    protocolParamsDatum
                <> withRefInputDatumValue
                    dirNodeRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                    directoryProgrammableNode
            )

globalSeize1ExternalScript50PubKeyCtx :: ScriptContext
globalSeize1ExternalScript50PubKeyCtx =
    mkGlobalSeizeExternalScriptAndManyPubKeyCtx manyPubKeyInputCount

directoryInitCtx :: ScriptContext
directoryInitCtx =
    let mintValue = mkValue [(directoryPolicyCS, TokenName "", 1)]
        emptyNodeDatum =
            aikenDirectoryNodeDatumData
                ""
                tailNodeBs
                (PubKeyCredential (PubKeyHash ""))
                (PubKeyCredential (PubKeyHash ""))
                ""
     in buildBalancedScriptContext
            ( withRedeemer (aikenConstr 0 [])
                <> withMintingScript mintValue (aikenConstr 0 [])
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
            )

programmableMintCtx :: ScriptContext
programmableMintCtx =
    let scriptRedeemer = aikenMintingActionRedeemerData (ScriptCredential mintingLogicHash)
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

-- | Mint inside a "busy" transaction: the full mint lands in the FIRST output
-- while 19 unrelated pubkey outputs follow. Production analog of the Aiken
-- @no_delegate_many_outputs@ issuance axis. Twin of the Plutarch-side fixture.
programmableMintBusyTxCtx :: ScriptContext
programmableMintBusyTxCtx =
    let scriptRedeemer = aikenMintingActionRedeemerData (ScriptCredential mintingLogicHash)
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
-- axis. Twin of the Plutarch-side fixture.
programmableMintManyWithdrawalsCtx :: ScriptContext
programmableMintManyWithdrawalsCtx =
    let scriptRedeemer = aikenMintingActionRedeemerData (ScriptCredential mintingLogicHash)
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

programmableBurnCtx :: ScriptContext
programmableBurnCtx =
    let scriptRedeemer = aikenMintingActionRedeemerData (ScriptCredential mintingLogicHash)
        burnValue = mkValue [(mintingPolicyCS, TokenName "0c", -1)]
        remainingValue = mkValue [(mintingPolicyCS, TokenName "0c", 1)]
        burnInputValue = mkAdaValue 10_000_000 <> mkValue [(mintingPolicyCS, TokenName "0c", 2)]
        globalRedeemer = aikenTransferActRedeemerData [TokenExists 1]
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
                        protocolParamsDatum
                    <> withRefInputDatumValue
                        directoryMintingNodeRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                        directoryMintingNode
                )

aikenIssuanceDatum :: BuiltinData
aikenIssuanceDatum = aikenIssuanceDatumData "0d" "0e"

aikenProtocolParamsAlwaysFailHash :: ScriptHash
aikenProtocolParamsAlwaysFailHash =
    scriptHashFromCardanoScript Aiken.aikenProtocolParamsSpendingScript

aikenIssuanceAlwaysFailHash :: ScriptHash
aikenIssuanceAlwaysFailHash =
    scriptHashFromCardanoScript Aiken.aikenIssuanceCborHexSpendingScript

aikenIssuancePolicyCS :: CurrencySymbol
aikenIssuancePolicyCS =
    scriptPolicyCurrencySymbol (Aiken.aikenIssuanceCborHexMintingScript issuanceInitRef)

aikenProtocolParamsMintCtx :: ScriptContext
aikenProtocolParamsMintCtx =
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
                    ( withTxOutAddress (scriptAddress aikenProtocolParamsAlwaysFailHash)
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue)
                        <> withTxOutInlineDatum protocolParamsDatum
                    )
            )

aikenIssuanceMintCtx :: ScriptContext
aikenIssuanceMintCtx =
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
                    ( withTxOutAddress (scriptAddress aikenIssuanceAlwaysFailHash)
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue)
                        <> withTxOutInlineDatum aikenIssuanceDatum
                    )
            )

aikenDirectoryInsertCtx :: ScriptContext
aikenDirectoryInsertCtx =
    let issuancePrefix = "0d"
        issuancePostfix = "0e"
        hashedMintingParam = bs28 0x55
        insertProtocolParamsDatum =
            aikenProtocolParamsDatumData directoryPolicyCS progLogicBaseCred
        insertedCs = computeRegisteredCs issuancePrefix issuancePostfix hashedMintingParam
        insertedCsBs = case insertedCs of
            CurrencySymbol bs -> bs
        insertedToken = TokenName insertedCsBs
        insertRedeemer = InsertDirectoryNode insertedCs (ScriptHash hashedMintingParam)
        nodeMintValue = mkValue [(directoryPolicyCS, insertedToken, 1)]
        registeredAssetMintValue = mkValue [(insertedCs, TokenName "0b", 1)]
        registeredAssetRedeemer = aikenMintingActionRedeemerData (ScriptCredential (ScriptHash hashedMintingParam))
        coveringNode =
            aikenDirectoryNodeDatumData
                ""
                tailNodeBs
                (ScriptCredential transferLogicHash)
                issuerCred
                ""
        coveringOutput =
            aikenDirectoryNodeDatumData
                ""
                (aikenCurrencySymbolBytes insertedCs)
                (ScriptCredential transferLogicHash)
                issuerCred
                ""
        insertedNode =
            aikenDirectoryNodeDatumData
                (aikenCurrencySymbolBytes insertedCs)
                tailNodeBs
                (ScriptCredential transferLogicHash)
                issuerCred
                ""
     in buildBalancedScriptContext
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
                        <> withInlineDatum coveringNode
                    )
                <> withPubKeyInputValue signerPkh directoryInsertFundingRef 6_000_000
                <> withOutput
                    ( withTxOutAddress (scriptAddress (ScriptHash (bs28 0x44)))
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mkValue [(directoryPolicyCS, TokenName "", 1)])
                        <> withTxOutInlineDatum coveringOutput
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddress (ScriptHash (bs28 0x44)))
                        <> withTxOutValue (mkAdaValue 2_000_000 <> mkValue [(directoryPolicyCS, insertedToken, 1)])
                        <> withTxOutInlineDatum insertedNode
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddressWithSignerStake progLogicBaseHash signerPkh)
                        <> withTxOutValue (mkAdaValue 2_000_000 <> registeredAssetMintValue)
                    )
                <> withRefInputDatumValue
                    paramRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
                    insertProtocolParamsDatum
                <> withRefInputDatumValue
                    issuanceRef
                    (pubKeyAddress signerPkh)
                    (mkAdaValue 2_000_000 <> mkValue [(aikenIssuancePolicyCS, issuanceCborHexToken, 1)])
                    aikenIssuanceDatum
            )

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
                        -- Proofs positional over the aggregated programmable input value
                        -- in CANONICAL currency-symbol order: nonProgrammableCS (0x1a)
                        -- then programmableTransferCS (0x1b).
                        (aikenTransferActRedeemerData [TokenDoesNotExist 1, TokenExists 2])
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
                        protocolParamsDatum
                    <> withRefInputDatumValue
                        dirNodeRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                        directoryCoveringNode
                    <> withRefInputDatumValue
                        directoryProgrammableNodeRef
                        (pubKeyAddress signerPkh)
                        (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
                        directoryProgrammableNode
                )

mainnetDexBaseSpendingCtx :: ScriptContext
mainnetDexBaseSpendingCtx =
    let ScriptContext txInfo _ _ = mainnetDexGlobalTransferCtx
     in ScriptContext
            txInfo
            (Redeemer (PlutusTx.toBuiltinData ()))
            (SpendingScript mainnetDexBaseInputRef Nothing)

-- Tx d29c... replay fixture retained to benchmark a realistic spending path.
txD29GlobalStakeRedeemer :: BuiltinData
txD29GlobalStakeRedeemer =
    aikenTransferActRedeemerData [TokenExists 2]

txD29ProtocolParamsDatumData :: BuiltinData
txD29ProtocolParamsDatumData =
    aikenProtocolParamsDatumData
        txD29RegistryNodeCS
        txD29BaseScriptCred

txD29RefDatum1Data :: BuiltinData
txD29RefDatum1Data =
    aikenIssuanceDatumData
        txD29IssuancePrefix
        txD29IssuancePostfix

txD29DirectoryNodeDatumData :: BuiltinData
txD29DirectoryNodeDatumData =
    aikenConstr
        0
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
    [ mkAikenCase
        "programmableLogicBase"
        (EvalBaseSpend progInputRef)
        (Aiken.aikenProgrammableLogicBaseScript globalCred)
        (aikenSpendArgs progInputRef baseSpendingCtx)
        baseSpendingCtx
    , mkAikenCase
        "programmableLogicGlobal.TransferAct"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalTransferCtx)
        globalTransferCtx
    , mkAikenCase
        "programmableLogicGlobal.TransferAct.TokenDoesNotExist"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalTransferDoesNotExistCtx)
        globalTransferDoesNotExistCtx
    , mkAikenCase
        "programmableLogicGlobal.TransferAct.MixedMany"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalTransferMixedManyCtx)
        globalTransferMixedManyCtx
    , mkAikenCase
        "programmableLogicGlobal.TransferAct.ManyTokens50"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalTransferManyTokens50Ctx)
        globalTransferManyTokens50Ctx
    , mkAikenCase
        "programmableLogicGlobal.TransferAct.ManyOutputs20"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalTransferManyOutputs20Ctx)
        globalTransferManyOutputs20Ctx
    , mkAikenCase
        "programmableLogicGlobal.TransferAct.ManyPolicies10"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalTransferManyPolicies10Ctx)
        globalTransferManyPolicies10Ctx
    , mkAikenCase
        "programmableLogicGlobal.TransferAct.Spend5Utxos"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalTransfer5Ctx)
        globalTransfer5Ctx
    , mkAikenCase
        "programmableLogicGlobal.TransferAct.Spend10Utxos"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalTransfer10Ctx)
        globalTransfer10Ctx
    , mkAikenCase
        "programmableLogicGlobal.TransferAct.Spend15Utxos"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalTransfer15Ctx)
        globalTransfer15Ctx
    , mkAikenCase
        "programmableLogicGlobal.TransferAct.Spend25Utxos"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalTransfer25Ctx)
        globalTransfer25Ctx
    , mkAikenCase
        "programmableLogicGlobal.TransferAct.Spend50Utxos"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalTransfer50Ctx)
        globalTransfer50Ctx
    , mkAikenCase
        "programmableLogicGlobal.TransferAct.Spend100Utxos"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalTransfer100Ctx)
        globalTransfer100Ctx
    , mkAikenCase
        "programmableLogicGlobal.SeizeAct1"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalSeize1Ctx)
        globalSeize1Ctx
    , mkAikenCase
        "programmableLogicGlobal.SeizeAct1.ExternalScriptAnd50PubKeyInputs"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalSeize1ExternalScript50PubKeyCtx)
        globalSeize1ExternalScript50PubKeyCtx
    , mkAikenCase
        "programmableLogicGlobal.SeizeAct5"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalSeize5Ctx)
        globalSeize5Ctx
    , mkAikenCase
        "programmableLogicGlobal.SeizeAct10"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalSeize10Ctx)
        globalSeize10Ctx
    , mkAikenCase
        "programmableLogicGlobal.SeizeAct20"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalSeize20Ctx)
        globalSeize20Ctx
    , mkAikenCase
        "programmableLogicGlobal.SeizeAct50"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalSeize50Ctx)
        globalSeize50Ctx
    , mkAikenCase
        "programmableLogicGlobal.SeizeAct100"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalSeize100Ctx)
        globalSeize100Ctx
    , mkAikenCase
        "programmableLogicGlobal.SeizeAct150"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalSeize150Ctx)
        globalSeize150Ctx
    , mkAikenCase
        "programmableLogicGlobal.SeizeAct2.PartialSeizeWithNoise"
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs globalSeizeNoiseCtx)
        globalSeizeNoiseCtx
    , mkAikenCase
        "directoryNodeMinting.InitDirectory"
        (EvalDirectoryMint directoryPolicyCS)
        (Aiken.aikenDirectoryNodeMintingScript initRef issuanceInitRef)
        (aikenMintArgs directoryInitCtx)
        directoryInitCtx
    , mkAikenCase
        "directoryNodeMinting.InsertDirectoryNode"
        (EvalDirectoryMint directoryPolicyCS)
        (Aiken.aikenDirectoryNodeMintingScript initRef issuanceInitRef)
        (aikenMintArgs aikenDirectoryInsertCtx)
        aikenDirectoryInsertCtx
    , mkAikenCase
        "programmableLogicMinting.Mint"
        (EvalProgrammableMint mintingPolicyCS)
        (Aiken.aikenProgrammableLogicMintingScript progLogicBaseCred (ScriptCredential mintingLogicHash))
        (aikenMintArgs programmableMintCtx)
        programmableMintCtx
    , mkAikenCase
        "programmableLogicMinting.Mint.BusyTx20Outputs"
        (EvalProgrammableMint mintingPolicyCS)
        (Aiken.aikenProgrammableLogicMintingScript progLogicBaseCred (ScriptCredential mintingLogicHash))
        (aikenMintArgs programmableMintBusyTxCtx)
        programmableMintBusyTxCtx
    , mkAikenCase
        "programmableLogicMinting.Mint.TenUnrelatedWithdrawals"
        (EvalProgrammableMint mintingPolicyCS)
        (Aiken.aikenProgrammableLogicMintingScript progLogicBaseCred (ScriptCredential mintingLogicHash))
        (aikenMintArgs programmableMintManyWithdrawalsCtx)
        programmableMintManyWithdrawalsCtx
    , mkAikenCase
        "programmableLogicMinting.Burn"
        (EvalProgrammableMint mintingPolicyCS)
        (Aiken.aikenProgrammableLogicMintingScript progLogicBaseCred (ScriptCredential mintingLogicHash))
        (aikenMintArgs programmableBurnCtx)
        programmableBurnCtx
    , mkAikenCase
        "protocolParamsMinting"
        (EvalProtocolParamsMint protocolParamsCS)
        (Aiken.aikenProtocolParamsMintingScript protocolParamsInitRef)
        (aikenMintArgs aikenProtocolParamsMintCtx)
        aikenProtocolParamsMintCtx
    , mkAikenCase
        "issuanceCborHexMinting"
        (EvalIssuanceMint issuancePolicyCS)
        (Aiken.aikenIssuanceCborHexMintingScript issuanceInitRef)
        (aikenMintArgs aikenIssuanceMintCtx)
        aikenIssuanceMintCtx
    , mkAikenCase
        ("programmableLogicBase.Tx." <> take 8 mainnetDexTxHash <> ".Spending")
        (EvalBaseSpend mainnetDexBaseInputRef)
        (Aiken.aikenProgrammableLogicBaseScript globalCred)
        (aikenSpendArgs mainnetDexBaseInputRef mainnetDexBaseSpendingCtx)
        mainnetDexBaseSpendingCtx
    , mkAikenCase
        ("programmableLogicGlobal.TransferAct.Tx." <> take 8 mainnetDexTxHash <> ".NightAdaDex16Swaps")
        (EvalGlobalReward globalCred)
        (Aiken.aikenProgrammableLogicGlobalScript protocolParamsCS)
        (aikenRewardArgs mainnetDexGlobalTransferCtx)
        mainnetDexGlobalTransferCtx
    , mkAikenCase
        ("programmableLogicBase.Tx." <> take 8 txD29Hash <> ".Spending")
        (EvalBaseSpend txD29ScriptInputRef)
        (Aiken.aikenProgrammableLogicBaseScript txD29GlobalStakeCred)
        (aikenSpendArgs txD29ScriptInputRef txD29ProgrammableLogicBaseSpendingCtx)
        txD29ProgrammableLogicBaseSpendingCtx
    , mkAikenCase
        ("programmableLogicBase.Tx." <> take 8 txD29Hash <> ".Stake")
        (EvalGlobalReward txD29GlobalStakeCred)
        (Aiken.aikenProgrammableLogicGlobalScript txD29ProtocolParamsCS)
        (aikenRewardArgs txD29ProgrammableLogicBaseStakeCtx)
        txD29ProgrammableLogicBaseStakeCtx
    ]
