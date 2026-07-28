{-# LANGUAGE OverloadedStrings #-}

{- | The offchain side of the mini-ledger owner witness.

'TransferAct' carries one withdrawal index per SCRIPT-owned input, and the
validator consumes that list positionally as it walks the transaction's inputs.
Two independent things therefore have to be right, and neither is visible in a
transaction that happens to validate:

* which inputs contribute an entry (script owners only), and
* what order they appear in (ledger input order, i.e. by 'TxIn').

Get the filter wrong and every later index points at the wrong input; get the
order wrong and two script owners cover for each other. Both failures are
rejected onchain -- there are validator tests for exactly that -- but they would
be produced by our own builder, so they belong under test here too.
-}
module ProgrammableTokens.Test.OwnerWitness (
    tests,
) where

import Cardano.Api qualified as C
import Data.ByteString qualified as BS
import Data.Word (Word8)
import ProgrammableTokens.OffChain.UTxODat (ownerWitnessOrder)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "OwnerWitness"
        [ testCase "unit_ownerWitness_pubkey_owners_take_no_entry" unit_ownerWitness_pubkey_owners_take_no_entry
        , testCase "unit_ownerWitness_orders_by_txin_not_by_argument_order" unit_ownerWitness_orders_by_txin_not_by_argument_order
        , testCase "unit_ownerWitness_skips_pubkeys_between_script_owners" unit_ownerWitness_skips_pubkeys_between_script_owners
        , testCase "unit_ownerWitness_keeps_repeated_owners_once_per_input" unit_ownerWitness_keeps_repeated_owners_once_per_input
        , testCase "unit_ownerWitness_all_pubkeys_is_empty" unit_ownerWitness_all_pubkeys_is_empty
        ]

-- | A pubkey owner is witnessed by its signature, so it must not consume an
-- index. If it did, every script owner after it would be shifted.
unit_ownerWitness_pubkey_owners_take_no_entry :: Assertion
unit_ownerWitness_pubkey_owners_take_no_entry =
    ownerWitnessOrder [(txIn 0, pubkeyOwner 0x01), (txIn 1, scriptOwner 0x02)]
        @?= [scriptOwner 0x02]

{- | The ledger presents inputs in 'TxIn' order regardless of how the builder
assembled them, and the validator consumes the witness list in that same walk.
Passing the inputs in reverse must not reverse the witnesses.
-}
unit_ownerWitness_orders_by_txin_not_by_argument_order :: Assertion
unit_ownerWitness_orders_by_txin_not_by_argument_order =
    ownerWitnessOrder [(txIn 2, scriptOwner 0x03), (txIn 0, scriptOwner 0x01), (txIn 1, scriptOwner 0x02)]
        @?= [scriptOwner 0x01, scriptOwner 0x02, scriptOwner 0x03]

-- | The realistic mixed shape: a fee-paying pubkey owner sitting between two
-- script owners must not appear, and must not disturb their order.
unit_ownerWitness_skips_pubkeys_between_script_owners :: Assertion
unit_ownerWitness_skips_pubkeys_between_script_owners =
    ownerWitnessOrder
        [ (txIn 0, scriptOwner 0x0a)
        , (txIn 1, pubkeyOwner 0xbb)
        , (txIn 2, scriptOwner 0x0c)
        ]
        @?= [scriptOwner 0x0a, scriptOwner 0x0c]

{- | Several inputs under the SAME owning script each need their own entry --
the validator consumes one per script-owned input, not one per distinct owner.
-}
unit_ownerWitness_keeps_repeated_owners_once_per_input :: Assertion
unit_ownerWitness_keeps_repeated_owners_once_per_input =
    ownerWitnessOrder [(txIn 0, scriptOwner 0x07), (txIn 1, scriptOwner 0x07)]
        @?= [scriptOwner 0x07, scriptOwner 0x07]

-- | The common case: a wallet transfer of its own UTxOs carries no entries.
unit_ownerWitness_all_pubkeys_is_empty :: Assertion
unit_ownerWitness_all_pubkeys_is_empty =
    ownerWitnessOrder [(txIn 0, pubkeyOwner 0x01), (txIn 1, pubkeyOwner 0x02)]
        @?= []

txIn :: Word -> C.TxIn
txIn ix =
    C.TxIn
        (either (error . show) id (C.deserialiseFromRawBytes C.AsTxId (BS.pack (replicate 32 0x00))))
        (C.TxIx ix)

scriptOwner :: Word8 -> C.StakeCredential
scriptOwner w =
    C.StakeCredentialByScript
        . either (error . show) id
        $ C.deserialiseFromRawBytes C.AsScriptHash (BS.pack (replicate 28 w))

pubkeyOwner :: Word8 -> C.StakeCredential
pubkeyOwner w =
    C.StakeCredentialByKey
        . either (error . show) id
        $ C.deserialiseFromRawBytes (C.AsHash C.AsStakeKey) (BS.pack (replicate 28 w))
