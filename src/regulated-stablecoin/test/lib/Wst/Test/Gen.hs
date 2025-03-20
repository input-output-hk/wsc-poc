{-| Generators for testing the POC
-}
module Wst.Test.Gen(
  genGlobalParams,
  genUTxODat,
  genUTxO,
  genTxIn,
  genAddress,
  genPaymentKeyHash,

  -- * Plutus types
  -- TODO: move to sc-tools?
  genCurrencySymbol,
  genCredential
) where

import Cardano.Api qualified as C
import Convex.PlutusLedger.V1 qualified as PL
import PlutusLedgerApi.V1.Credential (Credential)
import PlutusLedgerApi.V1.Value (CurrencySymbol)
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (..))
import Test.Gen.Cardano.Api.Typed qualified as Gen
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Hedgehog (hedgehog)
import Wst.Offchain.Query (UTxODat (..))

genGlobalParams :: Gen ProgrammableLogicGlobalParams
genGlobalParams =
  ProgrammableLogicGlobalParams
    <$> genCurrencySymbol
    <*> genCredential

genCurrencySymbol :: Gen CurrencySymbol
genCurrencySymbol = PL.transPolicyId <$> hedgehog Gen.genPolicyId

genCredential :: Gen Credential
genCredential = PL.transCredential <$> hedgehog Gen.genPaymentCredential

genUTxODat :: C.IsShelleyBasedEra era => Gen a -> Gen (UTxODat era a)
genUTxODat a =
  UTxODat
    <$> hedgehog Gen.genTxIn
    <*> genUTxO
    <*> a

genUTxO ::  C.IsShelleyBasedEra era => Gen (C.TxOut C.CtxUTxO era)
genUTxO = hedgehog (Gen.genTxOutUTxOContext C.shelleyBasedEra)

genTxIn :: Gen C.TxIn
genTxIn = hedgehog Gen.genTxIn

genAddress :: Gen (C.Address C.ShelleyAddr)
genAddress = hedgehog Gen.genAddressShelley

genPaymentKeyHash :: Gen (C.Hash C.PaymentKey)
genPaymentKeyHash = hedgehog (Gen.genVerificationKeyHash C.AsPaymentKey)
