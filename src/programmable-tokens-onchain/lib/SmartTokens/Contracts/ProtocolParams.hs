{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QualifiedDo         #-}
module SmartTokens.Contracts.ProtocolParams (
  mkProtocolParametersMinting,
  alwaysFailScript,
  mkPermissionedMinting,
) where

import Plutarch.Core.Context
import Plutarch.Core.List
import Plutarch.Core.Trace (pdebug)
import Plutarch.Core.Utils
import Plutarch.Core.ValidationLogic
import Plutarch.Core.Value
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import SmartTokens.Types.Constants (pprotocolParamsTokenData)

-- | Protocol Parameters minting policy
-- This validator allows minting of a single token with a single token name.
mkProtocolParametersMinting :: Term s (PAsData PTxOutRef :--> PScriptContext :--> PUnit)
mkProtocolParametersMinting = plam $ \oref ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
  PTxInfo {ptxInfo'inputs, ptxInfo'mint} <- pmatch pscriptContext'txInfo
  PMintingScript ownCS <- pmatch pscriptContext'scriptInfo

  mintedValue <- plet $ pfromData ptxInfo'mint
  let ownTkPairs = ptryLookupValue # ownCS # mintedValue

  -- Enforce that only a single token name is minted for this policy
  ownTkPair <- plet (pheadSingleton # ownTkPairs)
  ownTokenName <- plet (pfstBuiltin # ownTkPair)
  ownNumMinted <- plet (psndBuiltin # ownTkPair)
  pvalidateConditions
    [ pdebug "minted tn must match protocolParamsToken" $ ownTokenName #== pprotocolParamsTokenData
    , pdebug "only single pp token must be minted" $ ownNumMinted #== pconstant 1
    , pdebug "must spent ppInitTxOutRef" $ phasUTxO # pfromData oref # pfromData ptxInfo'inputs
    ]

-- | Permissioned Minting Policy
-- This minting policy checks for a given permissioned credential in the signatories of the transaction.
-- It allows minting of any number of tokens with any token name so long as the credential authorizes the transaction.
mkPermissionedMinting :: Term s (PData :--> PAsData PPubKeyHash :--> PScriptContext :--> PUnit)
mkPermissionedMinting = plam $ \_ permissionedCred ctx ->
  pvalidateConditions
    [ ptxSignedByPkh # permissionedCred # (pfromData . ptxInfoSignatories . pscriptContextTxInfo) ctx
    ]

-- | A nonced always fails script
-- The parameter is used to modify the script hash.
-- This is where the protocol parameters UTxO should reside.
alwaysFailScript :: Term s (PData :--> PScriptContext :--> PUnit)
alwaysFailScript = plam $ \_ _ctx -> perror
