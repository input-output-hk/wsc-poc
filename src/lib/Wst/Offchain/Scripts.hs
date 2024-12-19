{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Wst.Offchain.Scripts (
  protocolParamsMintingScript,
  protocolParamsSpendingScript,
  directoryNodeMintingScript,
  directoryNodeSpendingScript,
  programmableLogicMintingScript,
  programmableLogicBaseScript,
  programmableLogicGlobalScript,
  scriptPolicyIdV3
  )
  where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Convex.PlutusLedger.V1 (transCredential, transPolicyId,
                               transStakeCredential)
import Convex.PlutusLedger.V3 (transTxOutRef)
import Plutarch (ClosedTerm, Config (..), LogLevel (..), TracingMode (..), (#))
import Plutarch.Builtin (pdata, pforgetData)
import Plutarch.ByteString (PByteString)
import Plutarch.Lift (pconstant)
import Plutarch.Script (serialiseScript)
import SmartTokens.Contracts.Issuance (mkProgrammableLogicMinting)
import SmartTokens.Contracts.ProgrammableLogicBase (mkProgrammableLogicBase,
                                                    mkProgrammableLogicGlobal)
import SmartTokens.Contracts.ProtocolParams (alwaysFailScript,
                                             mkProtocolParametersMinting)
import SmartTokens.Core.Scripts (tryCompile)
import SmartTokens.LinkedList.MintDirectory (mkDirectoryNodeMP)
import SmartTokens.LinkedList.SpendDirectory (pmkDirectorySpending)

tracingConfig :: Config
tracingConfig = Tracing LogInfo DoTracingAndBinds

prodConfig :: Config
prodConfig = NoTracing

-- Protocol params

-- | The minting script for the protocol parameters NFT, takes initial TxIn for
-- one shot mint
protocolParamsMintingScript :: C.TxIn -> C.PlutusScript C.PlutusScriptV3
protocolParamsMintingScript txIn =
  let script = tryCompile prodConfig $ mkProtocolParametersMinting # pdata (pconstant $ transTxOutRef txIn)
  in C.PlutusScriptSerialised $ serialiseScript script

-- | The spending script for the protocol parameters NFT parameterized by ""
-- nonce
protocolParamsSpendingScript :: C.PlutusScript C.PlutusScriptV3
protocolParamsSpendingScript =
  let script = tryCompile prodConfig $ alwaysFailScript # pforgetData (pdata (pconstant "" :: ClosedTerm PByteString))
  in C.PlutusScriptSerialised $ serialiseScript script

-- | The minting script for the directory node tokens, takes initial TxIn for
-- symbol uniqueness across instances
directoryNodeMintingScript :: C.TxIn -> C.PlutusScript C.PlutusScriptV3
directoryNodeMintingScript txIn =
  let script = tryCompile tracingConfig $ mkDirectoryNodeMP # pdata (pconstant $ transTxOutRef txIn)
  in C.PlutusScriptSerialised $ serialiseScript script

-- | The spending script for the directory node tokens, parameterized by the
-- policy id of the protocol parameters NFT.
directoryNodeSpendingScript :: C.PolicyId -> C.PlutusScript C.PlutusScriptV3
directoryNodeSpendingScript paramsPolId =
  let script = tryCompile prodConfig $ pmkDirectorySpending # pdata (pconstant $ transPolicyId paramsPolId)
  in C.PlutusScriptSerialised $ serialiseScript script

-- TODO: can we change the signature to just take the param policy id?
programmableLogicMintingScript :: C.PaymentCredential -> C.StakeCredential -> C.PolicyId -> C.PlutusScript C.PlutusScriptV3
programmableLogicMintingScript progLogicBaseSpndingCred mintingCred nodePolId =
  let script = tryCompile prodConfig
               $ mkProgrammableLogicMinting
                  # pdata (pconstant $ transCredential progLogicBaseSpndingCred)
                  # pdata (pconstant $ transPolicyId nodePolId)
                  # pdata (pconstant $ transStakeCredential mintingCred)
  in C.PlutusScriptSerialised $ serialiseScript script

programmableLogicBaseScript :: C.StakeCredential -> C.PlutusScript C.PlutusScriptV3 -- Parameterized by the stake cred of the global script
programmableLogicBaseScript globalCred =
  let script = tryCompile prodConfig $ mkProgrammableLogicBase # pdata (pconstant $ transStakeCredential globalCred)
  in C.PlutusScriptSerialised $ serialiseScript script

programmableLogicGlobalScript :: C.PolicyId -> C.PlutusScript C.PlutusScriptV3 -- Parameterized by the CS holding protocol params datum
programmableLogicGlobalScript paramsPolId =
  let script = tryCompile prodConfig $ mkProgrammableLogicGlobal # pdata (pconstant $ transPolicyId paramsPolId)
  in C.PlutusScriptSerialised $ serialiseScript script

-- Utilities
scriptPolicyIdV3 :: C.PlutusScript C.PlutusScriptV3 -> C.PolicyId
scriptPolicyIdV3 = C.scriptPolicyId . C.PlutusScript C.PlutusScriptV3
