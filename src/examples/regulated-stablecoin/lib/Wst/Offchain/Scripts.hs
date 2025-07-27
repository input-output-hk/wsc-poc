{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Wst.Offchain.Scripts (
  -- * Core scripts

  -- * Transfer logic
  permissionedMintingScript,
  permissionedSpendingScript,
  freezeTransferScript,
  blacklistMintingScript,
  blacklistSpendingScript,

  -- Utils
  scriptPolicyIdV3
  )
  where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Convex.PlutusLedger.V1 (transCredential, transPolicyId, transPubKeyHash)
import Convex.PlutusLedger.V3 (transTxOutRef)
import Plutarch.Prelude
import Plutarch.Script (serialiseScript)
import SmartTokens.Contracts.AlwaysYields (palwaysSucceed)
import SmartTokens.Contracts.ExampleTransferLogic (mkFreezeAndSeizeTransfer,
                                                   mkPermissionedTransfer)
import SmartTokens.Contracts.IssuanceCborHex (mkIssuanceCborHexMinting)
import SmartTokens.Contracts.ProtocolParams (alwaysFailScript,
                                             mkPermissionedMinting)
import SmartTokens.Core.Scripts (ScriptTarget (..))
import SmartTokens.Core.Scripts qualified as Scripts

-- Protocol params
-- | The minting script for the issuance cbor hex NFT, takes initial TxIn for
-- one shot mint
issuanceCborHexMintingScript :: ScriptTarget -> C.TxIn -> C.PlutusScript C.PlutusScriptV3
issuanceCborHexMintingScript target txIn =
  let script = Scripts.tryCompile target $ mkIssuanceCborHexMinting # pdata (pconstant $ transTxOutRef txIn)
  in C.PlutusScriptSerialised $ serialiseScript script

-- | The spending script for the issuance cbor hex NFT parameterized by the nonce "deadbeef"
issuanceCborHexSpendingScript :: ScriptTarget -> C.PlutusScript C.PlutusScriptV3
issuanceCborHexSpendingScript target =
  let script = Scripts.tryCompile target $ alwaysFailScript # pforgetData (pdata (pconstant "deadbeef" :: ClosedTerm PByteString))
  in C.PlutusScriptSerialised $ serialiseScript script

freezeTransferScript :: ScriptTarget -> C.PaymentCredential -> C.PolicyId -> C.PlutusScript C.PlutusScriptV3
freezeTransferScript target progLogicBaseSpndingCred blacklistPolicyId =
  let script = Scripts.tryCompile target $ mkFreezeAndSeizeTransfer # pdata (pconstant $ transCredential progLogicBaseSpndingCred) # pdata (pconstant $ transPolicyId blacklistPolicyId)
  in C.PlutusScriptSerialised $ serialiseScript script

{-| 'C.PlutusScript C.PlutusScriptV3' that always succeeds. Can be used for minting, withdrawal, spending, etc.
-}
alwaysSucceedsScript :: ScriptTarget -> C.PlutusScript C.PlutusScriptV3
alwaysSucceedsScript target =
  C.PlutusScriptSerialised $ serialiseScript $ Scripts.tryCompile target palwaysSucceed

-- All the scripts below are nonced permissioned validators

permissionedMintingScript :: ScriptTarget -> C.Hash C.PaymentKey -> C.PlutusScript C.PlutusScriptV3
permissionedMintingScript target cred =
  let script = Scripts.tryCompile target $ mkPermissionedMinting # pforgetData (pdata (pconstant "permissioned minting" :: ClosedTerm PByteString)) # pdata (pconstant $ transPubKeyHash cred)
  in C.PlutusScriptSerialised $ serialiseScript script

permissionedSpendingScript :: ScriptTarget -> C.Hash C.PaymentKey -> C.PlutusScript C.PlutusScriptV3
permissionedSpendingScript target cred =
  let script = Scripts.tryCompile target $ mkPermissionedTransfer # pforgetData (pdata (pconstant "permissioned spending" :: ClosedTerm PByteString)) # pdata (pconstant $ transPubKeyHash cred)
  in C.PlutusScriptSerialised $ serialiseScript script

blacklistMintingScript :: ScriptTarget -> C.Hash C.PaymentKey -> C.PlutusScript C.PlutusScriptV3
blacklistMintingScript target cred =
  let script = Scripts.tryCompile target $ mkPermissionedMinting # pforgetData (pdata (pconstant "blacklist minting" :: ClosedTerm PByteString)) # pdata (pconstant $ transPubKeyHash cred)
  in C.PlutusScriptSerialised $ serialiseScript script

blacklistSpendingScript :: ScriptTarget -> C.Hash C.PaymentKey -> C.PlutusScript C.PlutusScriptV3
blacklistSpendingScript target cred =
  let script = Scripts.tryCompile target $ mkPermissionedTransfer # pforgetData (pdata (pconstant "blacklist spending" :: ClosedTerm PByteString)) # pdata (pconstant $ transPubKeyHash cred)
  in C.PlutusScriptSerialised $ serialiseScript script

-- Utilities
scriptPolicyIdV3 :: C.PlutusScript C.PlutusScriptV3 -> C.PolicyId
scriptPolicyIdV3 = C.scriptPolicyId . C.PlutusScript C.PlutusScriptV3
