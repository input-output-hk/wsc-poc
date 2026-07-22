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
    withTxOutAddress,
    withTxOutValue,
    withValue,
    withWithdrawal,
 )
import SmartTokens.Contracts.Issuance (MintRedeemer (..), RegistrationWitness (..), mkProgrammableLogicMinting)
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
