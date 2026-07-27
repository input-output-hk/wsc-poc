{-# LANGUAGE OverloadedStrings #-}

-- | Negative coverage for the base spending validator.
--
-- The base validator is the payment credential every programmable UTxO sits
-- at. It authorizes nothing itself: it exists solely to force the spend to be
-- accompanied by one of the two stake validators (the global transfer validator
-- or the seize validator) through the withdraw-zero pattern. Whichever of those
-- runs then enforces the real invariants over the whole transaction.
--
-- That makes this validator a single point of failure in the most literal
-- sense. If it can be satisfied WITHOUT one of those two withdrawals present,
-- every programmable UTxO in the system can be spent with no further checks at
-- all -- no directory proof, no transfer-logic invocation, no containment. The
-- tests below are therefore all framed as attempts to spend a programmable UTxO
-- while dodging that delegation.
--
-- The spend redeemer witnesses BOTH which validator it delegates to and that
-- validator's index in the (credential-sorted) withdrawal map, so there are
-- three distinct ways to lie: name the right validator at a position it does
-- not occupy, name the other validator, or point past the map entirely.
module ProgrammableTokens.Test.ProgrammableLogicBase (
    tests,
) where

import Data.ByteString qualified as BS
import Data.Either (isLeft, isRight)
import Data.List (elemIndex, nub, sortBy)
import Data.Word (Word8)
import Plutarch.Evaluate (applyArguments, evalScript)
import Plutarch.Internal.Term (Config (NoTracing), Script, compile)
import PlutusLedgerApi.V1.Value (assetClass, assetClassValue)
import PlutusLedgerApi.V3
import PlutusTx qualified
import ProgrammableTokens.Test.ScriptContext.Builder (
    ScriptContextBuilder,
    buildLedgerShapedScriptContext,
    compareCredentialLedger,
    mkAdaValue,
    withAddress,
    withOutRef,
    withSigner,
    withSpendingScript,
    withValue,
    withWithdrawal,
 )
import SmartTokens.Contracts.ProgrammableLogicBase (
    BaseSpendRedeemer (SpendViaGlobal, SpendViaSeize),
    mkProgrammableLogicBase,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "ProgrammableLogicBase"
        [ testCase "unit_baseSpend_via_global_succeeds" unit_baseSpend_via_global_succeeds
        , testCase "unit_baseSpend_via_seize_succeeds" unit_baseSpend_via_seize_succeeds
        , testCase "unit_baseSpend_via_global_with_noise_withdrawals_succeeds" unit_baseSpend_via_global_with_noise_withdrawals_succeeds
        , testCase "unit_baseSpend_no_delegation_withdrawal_rejected" unit_baseSpend_no_delegation_withdrawal_rejected
        , testCase "unit_baseSpend_attacker_withdrawal_at_witnessed_index_rejected" unit_baseSpend_attacker_withdrawal_at_witnessed_index_rejected
        , testCase "unit_baseSpend_global_arm_witnessing_seize_rejected" unit_baseSpend_global_arm_witnessing_seize_rejected
        , testCase "unit_baseSpend_seize_arm_witnessing_global_rejected" unit_baseSpend_seize_arm_witnessing_global_rejected
        , testCase "unit_baseSpend_stale_index_after_map_reorder_rejected" unit_baseSpend_stale_index_after_map_reorder_rejected
        , testCase "unit_baseSpend_index_past_end_rejected" unit_baseSpend_index_past_end_rejected
        ]

-- ---------------------------------------------------------------------------
-- Controls: the two legitimate ways to spend a programmable UTxO.
-- ---------------------------------------------------------------------------

unit_baseSpend_via_global_succeeds :: Assertion
unit_baseSpend_via_global_succeeds =
    assertBaseSucceeds $
        mkBaseSpendCtx (SpendViaGlobal (wdrlIndexOf transferWdrls globalCred)) transferWdrls

unit_baseSpend_via_seize_succeeds :: Assertion
unit_baseSpend_via_seize_succeeds =
    assertBaseSucceeds $
        mkBaseSpendCtx (SpendViaSeize (wdrlIndexOf seizeWdrls seizeCred)) seizeWdrls

{- | The witnessed index must stay correct when the transaction carries
unrelated withdrawals that shift the delegated validator's position in the
credential-sorted map. This is the case the index encoding exists to make
cheap, so it needs a control proving it still validates.
-}
unit_baseSpend_via_global_with_noise_withdrawals_succeeds :: Assertion
unit_baseSpend_via_global_with_noise_withdrawals_succeeds =
    assertBaseSucceeds $
        mkBaseSpendCtx (SpendViaGlobal (wdrlIndexOf noisyWdrls globalCred)) noisyWdrls

-- ---------------------------------------------------------------------------
-- The attack this validator exists to stop.
-- ---------------------------------------------------------------------------

{- | Spend a programmable UTxO in a transaction that invokes NEITHER stake
validator. Nothing else in the protocol is watching this spend, so if the base
validator accepts it the tokens leave the mini-ledger unconditionally.
-}
unit_baseSpend_no_delegation_withdrawal_rejected :: Assertion
unit_baseSpend_no_delegation_withdrawal_rejected =
    assertBaseFails $
        mkBaseSpendCtx (SpendViaGlobal 0) [attackerCred]

{- | The same attack dressed up: the attacker registers a withdrawal for a
script they control and points the witness at it. A validator that merely
checked "some withdrawal exists at the witnessed index" rather than checking
WHICH credential is there would accept this.
-}
unit_baseSpend_attacker_withdrawal_at_witnessed_index_rejected :: Assertion
unit_baseSpend_attacker_withdrawal_at_witnessed_index_rejected =
    assertBaseFails $
        mkBaseSpendCtx
            (SpendViaGlobal (wdrlIndexOf attackerWdrls attackerCred))
            attackerWdrls

-- ---------------------------------------------------------------------------
-- The witness is two independent claims; both must bind.
-- ---------------------------------------------------------------------------

{- | Claiming the global arm while pointing at the seize validator's entry. The
seize validator enforces a completely different set of invariants -- it never
checks transfer logic or directory membership for the moved policies -- so
letting a spend claim one arm and be covered by the other would let a seize
transaction authorize arbitrary transfers.
-}
unit_baseSpend_global_arm_witnessing_seize_rejected :: Assertion
unit_baseSpend_global_arm_witnessing_seize_rejected =
    assertBaseFails $
        mkBaseSpendCtx (SpendViaGlobal (wdrlIndexOf bothWdrls seizeCred)) bothWdrls

-- | The mirror image: the seize arm pointing at the global validator's entry.
unit_baseSpend_seize_arm_witnessing_global_rejected :: Assertion
unit_baseSpend_seize_arm_witnessing_global_rejected =
    assertBaseFails $
        mkBaseSpendCtx (SpendViaSeize (wdrlIndexOf bothWdrls globalCred)) bothWdrls

{- | A correct index for a DIFFERENT withdrawal set. The ledger sorts
withdrawals by credential, so adding one unrelated withdrawal can shift the
delegated validator's position; an index computed against the wrong map must
not validate. This is the realistic failure mode of index-carrying redeemers
and it must fail closed.
-}
unit_baseSpend_stale_index_after_map_reorder_rejected :: Assertion
unit_baseSpend_stale_index_after_map_reorder_rejected =
    let staleIndex = wdrlIndexOf transferWdrls globalCred
        freshIndex = wdrlIndexOf lowNoiseWdrls globalCred
     in do
            assertBool
                "fixture must actually reorder the map, otherwise this test is vacuous"
                (staleIndex /= freshIndex)
            assertBaseFails (mkBaseSpendCtx (SpendViaGlobal staleIndex) lowNoiseWdrls)

-- | An index past the end of the withdrawal map must error, not wrap or pass.
unit_baseSpend_index_past_end_rejected :: Assertion
unit_baseSpend_index_past_end_rejected =
    assertBaseFails $
        mkBaseSpendCtx (SpendViaGlobal 7) transferWdrls

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | Withdrawal sets. 'lowNoiseWdrls' deliberately contains a credential that
-- sorts BEFORE the global validator so it shifts the global's index.
transferWdrls :: [Credential]
transferWdrls = [globalCred, transferLogicCred]

seizeWdrls :: [Credential]
seizeWdrls = [seizeCred, issuerLogicCred]

bothWdrls :: [Credential]
bothWdrls = [globalCred, seizeCred, transferLogicCred]

noisyWdrls :: [Credential]
noisyWdrls = [globalCred, transferLogicCred, lowSortingCred, attackerCred]

lowNoiseWdrls :: [Credential]
lowNoiseWdrls = [globalCred, transferLogicCred, lowSortingCred]

attackerWdrls :: [Credential]
attackerWdrls = [attackerCred, transferLogicCred]

globalCred :: Credential
globalCred = ScriptCredential (ScriptHash (bs28 0x13))

seizeCred :: Credential
seizeCred = ScriptCredential (ScriptHash (bs28 0x1c))

transferLogicCred :: Credential
transferLogicCred = ScriptCredential (ScriptHash (bs28 0x15))

issuerLogicCred :: Credential
issuerLogicCred = ScriptCredential (ScriptHash (bs28 0x14))

-- | Sorts before 'globalCred', so including it shifts the global validator one
-- position later in the credential-sorted withdrawal map.
lowSortingCred :: Credential
lowSortingCred = ScriptCredential (ScriptHash (bs28 0x02))

-- | A script the attacker controls and can freely add a zero withdrawal for.
attackerCred :: Credential
attackerCred = ScriptCredential (ScriptHash (bs28 0x77))

progLogicBaseHash :: ScriptHash
progLogicBaseHash = ScriptHash (bs28 0x12)

ownerPkh :: PubKeyHash
ownerPkh = PubKeyHash (bs28 0x01)

programmableCS :: CurrencySymbol
programmableCS = CurrencySymbol (bs28 0x1b)

baseInputRef :: TxOutRef
baseInputRef = TxOutRef "7a00" 0

-- | A programmable UTxO: base script payment credential, owner in the staking
-- credential.
baseWalletAddr :: Address
baseWalletAddr =
    Address (ScriptCredential progLogicBaseHash) (Just (StakingHash (PubKeyCredential ownerPkh)))

mkBaseSpendCtx :: BaseSpendRedeemer -> [Credential] -> ScriptContext
mkBaseSpendCtx redeemer wdrlCreds =
    buildLedgerShapedScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData redeemer)
            ( withOutRef baseInputRef
                <> withAddress baseWalletAddr
                <> withValue (mkAdaValue 10_000_000 <> mkValue [(programmableCS, TokenName "0c", 7)])
            )
            <> foldMap (`withWithdrawal` 0) wdrlCreds
            <> withSigner ownerPkh
        )

-- | Position of a credential in the withdrawal map as the LEDGER orders it.
-- Never write these out by hand: the map is credential-sorted, so a position is
-- a function of every participating script hash.
wdrlIndexOf :: [Credential] -> Credential -> Integer
wdrlIndexOf creds target =
    case elemIndex target (sortBy compareCredentialLedger (nub creds)) of
        Just idx -> fromIntegral idx
        Nothing -> error ("wdrlIndexOf: credential not in the withdrawal set: " <> show target)

mkValue :: [(CurrencySymbol, TokenName, Integer)] -> Value
mkValue entries =
    mconcat [assetClassValue (assetClass cs tn) qty | (cs, tn, qty) <- entries]

bs28 :: Word8 -> BuiltinByteString
bs28 w = toBuiltin (BS.pack (replicate 28 w))

-- ---------------------------------------------------------------------------
-- Evaluation
-- ---------------------------------------------------------------------------

baseScript :: Script
baseScript =
    either (error . show) id (compile NoTracing mkProgrammableLogicBase)

evalBase :: ScriptContext -> Either String ()
evalBase ctx =
    let (res, _budget, _logs) =
            evalScript
                ( applyArguments
                    baseScript
                    [ PlutusTx.toData globalCred
                    , PlutusTx.toData seizeCred
                    , PlutusTx.toData ctx
                    ]
                )
     in if isRight res then Right () else Left "script failed"

assertBaseSucceeds :: ScriptContext -> Assertion
assertBaseSucceeds ctx =
    assertBool "expected the base spend to validate" (isRight (evalBase ctx))

assertBaseFails :: ScriptContext -> Assertion
assertBaseFails ctx =
    assertBool "expected the base spend to be rejected" (isLeft (evalBase ctx))
