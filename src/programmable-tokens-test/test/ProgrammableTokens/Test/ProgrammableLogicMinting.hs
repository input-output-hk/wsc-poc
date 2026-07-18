{-# LANGUAGE OverloadedStrings #-}

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
import SmartTokens.Contracts.Issuance (mkProgrammableLogicMinting)
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (DirectorySetNode))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "ProgrammableLogicMinting unit tests"
        [ testCase "unit_mint_to_programmable_logic_output_succeeds" unit_mint_to_programmable_logic_output_succeeds
        , testCase "unit_mint_to_pubkey_output_rejected" unit_mint_to_pubkey_output_rejected
        , testCase "unit_mint_split_between_base_and_pubkey_rejected" unit_mint_split_between_base_and_pubkey_rejected
        , testCase "unit_mint_to_base_without_stake_cred_rejected" unit_mint_to_base_without_stake_cred_rejected
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

-- | Item 4: minting to a base-credential output that carries NO staking
-- credential must be rejected — such a UTxO can never be attributed to an owner
-- and would be permanently locked.
unit_mint_to_base_without_stake_cred_rejected :: Assertion
unit_mint_to_base_without_stake_cred_rejected =
    assertScriptFails programmableMintNoStakeCtx

assertScriptSucceeds :: ScriptContext -> Assertion
assertScriptSucceeds ctx =
    assertBool "expected successful evaluation" (scriptSucceeds ctx)

assertScriptFails :: ScriptContext -> Assertion
assertScriptFails ctx =
    assertBool "expected script failure" (scriptFails ctx)

mintArgs :: ScriptContext -> [Data]
mintArgs ctx =
    [ PlutusTx.toData progLogicBaseCred
    , PlutusTx.toData directoryNodeCS
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

mintRedeemer :: BuiltinData
mintRedeemer = PlutusTx.toBuiltinData (ScriptCredential mintingLogicHash)

-- | Base address WITH an inline stake credential (the owner). Programmable-token
-- outputs must carry one so the transfer path can attribute ownership.
baseAddrWithStake :: Address
baseAddrWithStake =
    Address progLogicBaseCred (Just (StakingHash (PubKeyCredential signerPkh)))

programmableMintCtx :: ScriptContext
programmableMintCtx =
    buildBalancedScriptContext
        ( withRedeemer mintRedeemer
            <> withMintingScript (mintValue 1) mintRedeemer
            <> withWithdrawal (ScriptCredential mintingLogicHash) 0
            <> registryNodeRefBuilder
            <> withOutput
                ( withTxOutAddress baseAddrWithStake
                    <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                )
        )

-- | Security S2: identical to the success case but with NO registry-node
-- reference input — minting a policy that is not registered in the directory.
-- Such tokens could later escape the mini-ledger via a covering-node transfer
-- proof, so the mint must be rejected.
programmableMintUnregisteredCtx :: ScriptContext
programmableMintUnregisteredCtx =
    buildBalancedScriptContext
        ( withRedeemer mintRedeemer
            <> withMintingScript (mintValue 1) mintRedeemer
            <> withWithdrawal (ScriptCredential mintingLogicHash) 0
            <> withOutput
                ( withTxOutAddress baseAddrWithStake
                    <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                )
        )

-- | Same as the success case but the base output has no staking credential.
programmableMintNoStakeCtx :: ScriptContext
programmableMintNoStakeCtx =
    buildBalancedScriptContext
        ( withRedeemer mintRedeemer
            <> withMintingScript (mintValue 1) mintRedeemer
            <> withWithdrawal (ScriptCredential mintingLogicHash) 0
            <> registryNodeRefBuilder
            <> withOutput
                ( withTxOutAddress (Address progLogicBaseCred Nothing)
                    <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                )
        )

programmableMintEscapeCtx :: ScriptContext
programmableMintEscapeCtx =
    buildBalancedScriptContext
        ( withRedeemer mintRedeemer
            <> withMintingScript (mintValue 1) mintRedeemer
            <> withWithdrawal (ScriptCredential mintingLogicHash) 0
            <> registryNodeRefBuilder
            <> withOutput
                ( withTxOutAddress (pubKeyAddress signerPkh)
                    <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                )
        )

programmableMintSplitEscapeCtx :: ScriptContext
programmableMintSplitEscapeCtx =
    buildBalancedScriptContext
        ( withRedeemer mintRedeemer
            <> withMintingScript (mintValue 2) mintRedeemer
            <> withWithdrawal (ScriptCredential mintingLogicHash) 0
            <> registryNodeRefBuilder
            <> withOutput
                ( withTxOutAddress (Address progLogicBaseCred Nothing)
                    <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                )
            <> withOutput
                ( withTxOutAddress (pubKeyAddress signerPkh)
                    <> withTxOutValue (mkAdaValue 2_000_000 <> mintValue 1)
                )
        )
