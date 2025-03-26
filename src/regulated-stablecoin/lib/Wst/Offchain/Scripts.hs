{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Wst.Offchain.Scripts (
  -- * Core scripts
  protocolParamsMintingScript,
  protocolParamsSpendingScript,
  directoryNodeMintingScript,
  directoryNodeSpendingScript,
  programmableLogicMintingScript,
  programmableLogicBaseScript,
  programmableLogicGlobalScript,

  -- * Transfer logic
  permissionedMintingScript,
  permissionedSpendingScript,
  freezeTransferScript,
  blacklistMintingScript,
  blacklistSpendingScript,

  -- * Always suceeds
  alwaysSucceedsScript,

  -- Utils
  scriptPolicyIdV3
  )
  where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Convex.PlutusLedger.V1 (transCredential, transPolicyId, transPubKeyHash,
                               transStakeCredential)
import Convex.PlutusLedger.V3 (transTxOutRef)
import Plutarch.Prelude
import Plutarch.Script (serialiseScript)
import PlutusLedgerApi.V3
import SmartTokens.Contracts.AlwaysYields (palwaysSucceed)
import SmartTokens.Contracts.ExampleTransferLogic (mkFreezeAndSeizeTransfer,
                                                   mkPermissionedTransfer)
import SmartTokens.Contracts.Issuance (mkProgrammableLogicMinting)
import SmartTokens.Contracts.ProgrammableLogicBase (mkProgrammableLogicBase,
                                                    mkProgrammableLogicGlobal)
import SmartTokens.Contracts.ProtocolParams (alwaysFailScript,
                                             mkPermissionedMinting,
                                             mkProtocolParametersMinting)
import SmartTokens.Core.Scripts (ScriptTarget (..))
import SmartTokens.Core.Scripts qualified as Scripts
import SmartTokens.LinkedList.MintDirectory (mkDirectoryNodeMP)
import SmartTokens.LinkedList.SpendDirectory (pmkDirectorySpending)

-- Protocol params

-- | The minting script for the protocol parameters NFT, takes initial TxIn for
-- one shot mint
protocolParamsMintingScript :: ScriptTarget -> C.TxIn -> C.PlutusScript C.PlutusScriptV3
protocolParamsMintingScript target txIn =
  let script = Scripts.tryCompile target $ mkProtocolParametersMinting # pdata (pconstant $ transTxOutRef txIn)
  in C.PlutusScriptSerialised $ serialiseScript script

-- | The spending script for the protocol parameters NFT parameterized by ""
-- nonce
protocolParamsSpendingScript :: ScriptTarget -> C.PlutusScript C.PlutusScriptV3
protocolParamsSpendingScript target =
  let script = Scripts.tryCompile target $ alwaysFailScript # pforgetData (pdata (pconstant "" :: ClosedTerm PByteString))
  in C.PlutusScriptSerialised $ serialiseScript script

-- | The minting script for the directory node tokens, takes initial TxIn for
-- symbol uniqueness across instances
directoryNodeMintingScript :: ScriptTarget -> C.TxIn -> C.PlutusScript C.PlutusScriptV3
directoryNodeMintingScript target txIn =
  let script = Scripts.tryCompile target $ mkDirectoryNodeMP # pdata (pconstant $ transTxOutRef txIn)
  in C.PlutusScriptSerialised $ serialiseScript script

-- | The spending script for the directory node tokens, parameterized by the
-- policy id of the protocol parameters NFT.
directoryNodeSpendingScript :: ScriptTarget -> C.PolicyId -> C.PlutusScript C.PlutusScriptV3
directoryNodeSpendingScript target paramsPolId =
  let script = Scripts.tryCompile target $ pmkDirectorySpending # pdata (pconstant $ transPolicyId paramsPolId)
  in C.PlutusScriptSerialised $ serialiseScript script

-- TODO: can we change the signature to just take the param policy id?
programmableLogicMintingScript :: ScriptTarget -> C.PaymentCredential -> C.StakeCredential -> C.PlutusScript C.PlutusScriptV3
programmableLogicMintingScript target progLogicBaseSpndingCred mintingCred =
  let script = Scripts.tryCompile target
               $ mkProgrammableLogicMinting
                  # pdata (pconstant $ transCredential progLogicBaseSpndingCred)
                  # pdata (pconstant $ extractScriptHash $ transStakeCredential mintingCred)
  in C.PlutusScriptSerialised $ serialiseScript script
  where
    extractScriptHash :: Credential -> ScriptHash
    extractScriptHash (ScriptCredential h) = h
    extractScriptHash _ = error "Expected ScriptCredential"

programmableLogicBaseScript :: ScriptTarget -> C.StakeCredential -> C.PlutusScript C.PlutusScriptV3 -- Parameterized by the stake cred of the global script
programmableLogicBaseScript target globalCred =
  let script = Scripts.tryCompile target $ mkProgrammableLogicBase # pdata (pconstant $ transStakeCredential globalCred)
  in C.PlutusScriptSerialised $ serialiseScript script

programmableLogicGlobalScript :: ScriptTarget -> C.PolicyId -> C.PlutusScript C.PlutusScriptV3 -- Parameterized by the CS holding protocol params datum
programmableLogicGlobalScript target paramsPolId =
  let script = Scripts.tryCompile target $ mkProgrammableLogicGlobal # pdata (pconstant $ transPolicyId paramsPolId)
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
