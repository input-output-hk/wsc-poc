{-# LANGUAGE OverloadedStrings #-}
module ProgrammableTokens.OffChain.Scripts(
  -- * Core scripts
  protocolParamsMintingScript,
  protocolParamsSpendingScript,
  directoryNodeMintingScript,
  directoryNodeSpendingScript,

  programmableLogicBaseScript,
  programmableLogicGlobalScript,
  programmableLogicMintingScript,

  -- * Issuance Cbor Hex Script
  issuanceCborHexSpendingScript,
  issuanceCborHexMintingScript,

  -- * Always suceeds
  alwaysSucceedsScript,

  -- * Etc.
  scriptPolicyIdV3
) where

import Cardano.Api.Shelley qualified as C
import Convex.PlutusLedger.V1 (transCredential, transPolicyId,
                               transStakeCredential)
import Convex.PlutusLedger.V3 (transTxOutRef)
import Plutarch.Evaluate (applyArguments)
import Plutarch.Prelude (ClosedTerm, PByteString, pconstant, pdata, pforgetData,
                         (#))
import Plutarch.Script (serialiseScript)
import PlutusLedgerApi.V3 (Credential (..), ScriptHash, toData)
import SmartTokens.Contracts.AlwaysYields (palwaysSucceed)
import SmartTokens.Contracts.Issuance (mkProgrammableLogicMinting)
import SmartTokens.Contracts.IssuanceCborHex (mkIssuanceCborHexMinting)
import SmartTokens.Contracts.ProgrammableLogicBase (mkProgrammableLogicBase,
                                                    mkProgrammableLogicGlobal)
import SmartTokens.Contracts.ProtocolParams (alwaysFailScript,
                                             mkProtocolParametersMinting)
import SmartTokens.Core.Scripts (ScriptTarget (..))
import SmartTokens.Core.Scripts qualified as Scripts
import SmartTokens.LinkedList.MintDirectory (mkDirectoryNodeMP)
import SmartTokens.LinkedList.SpendDirectory (pmkDirectorySpending)

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

-- | The minting script for the directory node tokens, takes initial TxIn for
-- symbol uniqueness across instances
directoryNodeMintingScript :: ScriptTarget -> C.TxIn -> C.TxIn -> C.PlutusScript C.PlutusScriptV3
directoryNodeMintingScript target txIn issuanceInitTxIn =
  let issuanceCS = transPolicyId $ scriptPolicyIdV3 $ issuanceCborHexMintingScript target issuanceInitTxIn
      script = Scripts.tryCompile target $ mkDirectoryNodeMP # pdata (pconstant $ transTxOutRef txIn) # pdata (pconstant issuanceCS)
  in C.PlutusScriptSerialised $ serialiseScript script

-- | The spending script for the directory node tokens, parameterized by the
-- policy id of the protocol parameters NFT.
directoryNodeSpendingScript :: ScriptTarget -> C.PolicyId -> C.PlutusScript C.PlutusScriptV3
directoryNodeSpendingScript target paramsPolId =
  let script = Scripts.tryCompile target $ pmkDirectorySpending # pdata (pconstant $ transPolicyId paramsPolId)
  in C.PlutusScriptSerialised $ serialiseScript script

programmableLogicBaseScript :: ScriptTarget -> C.StakeCredential -> C.PlutusScript C.PlutusScriptV3 -- Parameterized by the stake cred of the global script
programmableLogicBaseScript target globalCred =
  let script = Scripts.tryCompile target $ mkProgrammableLogicBase # pdata (pconstant $ transStakeCredential globalCred)
  in C.PlutusScriptSerialised $ serialiseScript script

programmableLogicGlobalScript :: ScriptTarget -> C.PolicyId -> C.PlutusScript C.PlutusScriptV3 -- Parameterized by the CS holding protocol params datum
programmableLogicGlobalScript target paramsPolId =
  let script = Scripts.tryCompile target $ mkProgrammableLogicGlobal # pdata (pconstant $ transPolicyId paramsPolId)
  in C.PlutusScriptSerialised $ serialiseScript script

{-| 'C.PlutusScript C.PlutusScriptV3' that always succeeds. Can be used for minting, withdrawal, spending, etc.
-}
alwaysSucceedsScript :: ScriptTarget -> C.PlutusScript C.PlutusScriptV3
alwaysSucceedsScript target =
  C.PlutusScriptSerialised $ serialiseScript $ Scripts.tryCompile target palwaysSucceed

-- TODO: can we change the signature to just take the param policy id?
programmableLogicMintingScript :: ScriptTarget -> C.PaymentCredential -> C.StakeCredential -> C.PlutusScript C.PlutusScriptV3
programmableLogicMintingScript _target progLogicBaseSpndingCred mintingCred =
  let unappliedScript = Scripts.tryCompile Production
               $ mkProgrammableLogicMinting
                  # pdata (pconstant $ transCredential progLogicBaseSpndingCred)
      script = applyArguments unappliedScript [toData $ extractScriptHash $ transStakeCredential mintingCred]
  in C.PlutusScriptSerialised $ serialiseScript script
  where
    extractScriptHash :: Credential -> ScriptHash
    extractScriptHash (ScriptCredential h) = h
    extractScriptHash _ = error "Expected ScriptCredential"

-- Utilities
scriptPolicyIdV3 :: C.PlutusScript C.PlutusScriptV3 -> C.PolicyId
scriptPolicyIdV3 = C.scriptPolicyId . C.PlutusScript C.PlutusScriptV3
