{-# LANGUAGE OverloadedStrings #-}

-- | Golden encoding tests that freeze the on-chain @Data@ layout of the
-- consensus-critical types (spec §6, §4). The redesigned issuance policy and the
-- companion validators read these datums/redeemers by RAW field access with no
-- shape validation, so field ORDER and the list-vs-constr shape are consensus
-- rules. If any of these assertions changes, a deployed instance's datums stop
-- decoding — so these tests must fail loudly on any accidental layout change.
module ProgrammableTokens.Test.GoldenEncoding (tests) where

import Data.ByteString qualified as BS
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import PlutusLedgerApi.V3 (Credential (..), Data (..), ScriptHash (..), toData)
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

bs28 :: Builtins.BuiltinByteString -> Builtins.BuiltinByteString
bs28 = id

mkCS :: BS.ByteString -> CurrencySymbol
mkCS = CurrencySymbol . Builtins.toBuiltin

mkScriptCred :: BS.ByteString -> Credential
mkScriptCred = ScriptCredential . ScriptHash . Builtins.toBuiltin

-- Distinguishable fixture values, one per field, so a field swap changes the
-- golden bytes.
sampleDirCS :: CurrencySymbol
sampleDirCS = mkCS (BS.replicate 28 0x01)

sampleBaseCred :: Credential
sampleBaseCred = mkScriptCred (BS.replicate 28 0x02)

sampleGlobalCred :: Credential
sampleGlobalCred = mkScriptCred (BS.replicate 28 0x03)

sampleSeizeCred :: Credential
sampleSeizeCred = mkScriptCred (BS.replicate 28 0x04)

sampleParams :: ProgrammableLogicGlobalParams
sampleParams =
  ProgrammableLogicGlobalParams
    { directoryNodeCS = sampleDirCS
    , progLogicCred = sampleBaseCred
    , globalLogicCred = sampleGlobalCred
    , seizeLogicCred = sampleSeizeCred
    }

tests :: TestTree
tests =
  testGroup
    "GoldenEncoding"
    [ testCase "params datum is a 4-element List in the normative field order" $
        -- The datum MUST be a bare Data List (NOT a Constr), with fields in the
        -- exact order 0=directory CS, 1=base cred, 2=global cred, 3=seize cred.
        -- Fields 0/1 remain byte-identical to the pre-redesign 2-field datum.
        toData sampleParams
          @?= List
            [ toData sampleDirCS
            , toData sampleBaseCred
            , toData sampleGlobalCred
            , toData sampleSeizeCred
            ]
    , testCase "params datum round-trips through toData/fromData" $
        PlutusTx.fromData (toData sampleParams) @?= Just sampleParams
    , testCase "fields 0/1 are byte-compatible with the legacy 2-field prefix" $
        -- A consumer that reads only the first two list elements sees exactly the
        -- old datum's two fields — the coexistence guarantee for existing readers.
        take 2 (case toData sampleParams of List xs -> xs; _ -> [])
          @?= [toData sampleDirCS, toData sampleBaseCred]
    ]
