{-# LANGUAGE OverloadedStrings #-}

-- | Red/green tests for the directory-node minting policy's registration
-- authorization (security S1). Post-hardening, inserting a directory node for a
-- programmable-token policy requires the substandard's minting-logic script to
-- be invoked (its credential present in @tx.withdrawals@) — proof of instance.
-- Without it, ANY party could register a policy id (a public blake2b of the
-- minting-logic hash) with attacker-chosen transfer/issuer logic and
-- permanently hijack it (there is no node-update path). Mirrors the Aiken
-- @registry_mint@ Finding-03 authorization.
module ProgrammableTokens.Test.DirectoryMint (
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
import PlutusTx.Builtins qualified as BI
import ProgrammableTokens.Test.ScriptContext.Builder (
    ScriptContextBuilder,
    buildScriptContext,
    mkAdaValue,
    withAddress,
    withInlineDatum,
    withMintingScript,
    withOutRef,
    withOutput,
    withRedeemer,
    withReferenceInput,
    withScriptInput,
    withSigner,
    withTxOutAddress,
    withTxOutInlineDatum,
    withTxOutValue,
    withValue,
    withWithdrawal,
 )
import SmartTokens.Contracts.IssuanceCborHex (IssuanceCborHex (IssuanceCborHex))
import SmartTokens.LinkedList.MintDirectory (
    DirectoryNodeAction (InsertDirectoryNode),
    mkDirectoryNodeMP,
 )
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (DirectorySetNode))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "DirectoryMint registration-authorization tests"
        [ testCase "unit_directoryInsert_with_minting_logic_withdrawal_succeeds" unit_directoryInsert_authorized_succeeds
        , testCase "unit_directoryInsert_without_minting_logic_withdrawal_rejected" unit_directoryInsert_unauthorized_rejected
        ]

-- | Green: the substandard's minting-logic withdrawal is present.
unit_directoryInsert_authorized_succeeds :: Assertion
unit_directoryInsert_authorized_succeeds =
    assertBool "expected authorized registration to succeed" (insertSucceeds (insertCtx True))

-- | Red: same registration WITHOUT the minting-logic withdrawal — a
-- front-runner/squatter registering a policy they do not control.
unit_directoryInsert_unauthorized_rejected :: Assertion
unit_directoryInsert_unauthorized_rejected =
    assertBool "expected unauthorized registration to fail" (insertFails (insertCtx False))

insertSucceeds :: ScriptContext -> Bool
insertSucceeds ctx =
    let (res, _budget, _logs) = evalScript (applyArguments directoryScript (insertArgs ctx))
     in isRight res

insertFails :: ScriptContext -> Bool
insertFails ctx =
    let (res, _budget, _logs) = evalScript (applyArguments directoryScript (insertArgs ctx))
     in isLeft res

insertArgs :: ScriptContext -> [Data]
insertArgs ctx =
    [ PlutusTx.toData initRef
    , PlutusTx.toData issuanceCborHexCS
    , PlutusTx.toData ctx
    ]

compileNoTracing :: (forall s. Term s a) -> Script
compileNoTracing term =
    either (error . (("compile failed: " ++) . show)) id (compile NoTracing term)

directoryScript :: Script
directoryScript = compileNoTracing mkDirectoryNodeMP

-- Fixture constants -----------------------------------------------------------

bs28 :: Word8 -> BuiltinByteString
bs28 w = PV1.toBuiltin (BS.replicate 28 w)

directoryPolicyCS :: CurrencySymbol
directoryPolicyCS = CurrencySymbol (bs28 0x11)

issuanceCborHexCS :: CurrencySymbol
issuanceCborHexCS = CurrencySymbol (bs28 0x17)

transferLogicHash :: ScriptHash
transferLogicHash = ScriptHash (bs28 0x15)

issuerCred :: Credential
issuerCred = ScriptCredential (ScriptHash (bs28 0x18))

nodeScriptHash :: ScriptHash
nodeScriptHash = ScriptHash (bs28 0x44)

signerPkh :: PubKeyHash
signerPkh = PubKeyHash (bs28 0x01)

pubKeyAddress :: PubKeyHash -> Address
pubKeyAddress pkh = Address (PubKeyCredential pkh) Nothing

scriptAddr :: ScriptHash -> Address
scriptAddr sh = Address (ScriptCredential sh) Nothing

bs32 :: Word8 -> BuiltinByteString
bs32 w = PV1.toBuiltin (BS.replicate 32 w)

-- The init reference is unused on the insert path; any value works.
initRef :: TxOutRef
initRef = TxOutRef (TxId (bs32 0x1a)) 0

insertNodeInRef :: TxOutRef
insertNodeInRef = TxOutRef (TxId (bs32 0xc0)) 0

issuanceRef :: TxOutRef
issuanceRef = TxOutRef (TxId (bs32 0xbb)) 1

issuancePrefix, issuancePostfix :: BuiltinByteString
issuancePrefix = "0d"
issuancePostfix = "0e"

hashedMintingParam :: BuiltinByteString
hashedMintingParam = bs28 0x55

mintingLogicCred :: Credential
mintingLogicCred = ScriptCredential (ScriptHash hashedMintingParam)

-- | Reproduces the on-chain '_papplyHashedParameter' derivation.
insertedCs :: CurrencySymbol
insertedCs =
    CurrencySymbol $
        BI.blake2b_224
            ( PV1.toBuiltin (BS.pack [3])
                <> issuancePrefix
                <> BI.serialiseData (PlutusTx.toBuiltinData hashedMintingParam)
                <> issuancePostfix
            )

insertedCsBs :: BuiltinByteString
insertedCsBs = case insertedCs of CurrencySymbol bs -> bs

tailCS :: CurrencySymbol
tailCS = CurrencySymbol (PV1.toBuiltin (BS.replicate 28 0xff))

coveringNode :: DirectorySetNode
coveringNode =
    DirectorySetNode (CurrencySymbol "") tailCS (ScriptCredential transferLogicHash) issuerCred (CurrencySymbol "")

coveringOutput :: DirectorySetNode
coveringOutput =
    DirectorySetNode (CurrencySymbol "") insertedCs (ScriptCredential transferLogicHash) issuerCred (CurrencySymbol "")

insertedNode :: DirectorySetNode
insertedNode =
    DirectorySetNode insertedCs tailCS (ScriptCredential transferLogicHash) issuerCred (CurrencySymbol "")

insertRedeemer :: BuiltinData
insertRedeemer = PlutusTx.toBuiltinData (InsertDirectoryNode insertedCs (ScriptHash hashedMintingParam))

-- | Directory-insert context. @authorized@ toggles the minting-logic
-- withdrawal that S1 requires.
insertCtx :: Bool -> ScriptContext
insertCtx authorized =
    let nodeMintValue = assetClassValue (assetClass directoryPolicyCS (TokenName insertedCsBs)) 1
        withdrawalBuilder :: ScriptContextBuilder
        withdrawalBuilder = if authorized then withWithdrawal mintingLogicCred 0 else mempty
     in buildScriptContext
            ( withRedeemer insertRedeemer
                <> withMintingScript nodeMintValue insertRedeemer
                <> withSigner signerPkh
                <> withdrawalBuilder
                <> withScriptInput
                    (PlutusTx.toBuiltinData ())
                    ( withOutRef insertNodeInRef
                        <> withAddress (scriptAddr nodeScriptHash)
                        <> withValue (mkAdaValue 2_000_000 <> assetClassValue (assetClass directoryPolicyCS (TokenName "")) 1)
                        <> withInlineDatum (PlutusTx.toBuiltinData coveringNode)
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddr nodeScriptHash)
                        <> withTxOutValue (mkAdaValue 2_000_000 <> assetClassValue (assetClass directoryPolicyCS (TokenName "")) 1)
                        <> withTxOutInlineDatum (PlutusTx.toBuiltinData coveringOutput)
                    )
                <> withOutput
                    ( withTxOutAddress (scriptAddr nodeScriptHash)
                        <> withTxOutValue (mkAdaValue 2_000_000 <> nodeMintValue)
                        <> withTxOutInlineDatum (PlutusTx.toBuiltinData insertedNode)
                    )
                <> withReferenceInput
                    ( withOutRef issuanceRef
                        <> withAddress (pubKeyAddress signerPkh)
                        <> withValue (mkAdaValue 2_000_000 <> assetClassValue (assetClass issuanceCborHexCS (TokenName "IssuanceCborHex")) 1)
                        <> withInlineDatum (PlutusTx.toBuiltinData (IssuanceCborHex issuancePrefix issuancePostfix))
                    )
            )
