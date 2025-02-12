{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QualifiedDo         #-}
module SmartTokens.Contracts.ProtocolParams (
  mkProtocolParametersMinting,
  alwaysFailScript,
  mkPermissionedMinting,
) where

import Plutarch.Core.Utils (phasUTxO, pheadSingleton, pletFieldsMinting,
                            ptryLookupValue, ptxSignedByPkh,
                            pvalidateConditions)
import Plutarch.LedgerApi.V3 (PPubKeyHash, PScriptContext, PTxOutRef)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude (ClosedTerm, PAsData, PData, PEq ((#==)), PUnit,
                         pconstantData, perror, pfield, pfromData, pfstBuiltin,
                         plam, plet, pletFields, psndBuiltin, type (:-->), (#))
import SmartTokens.Types.Constants (pprotocolParamsTokenData)

-- | Protocol Parameters minting policy
-- This validator allows minting of a single token with a single token name.
mkProtocolParametersMinting :: ClosedTerm (PAsData PTxOutRef :--> PScriptContext :--> PUnit)
mkProtocolParametersMinting = plam $ \oref ctx -> P.do
  ctxF <- pletFields @'["txInfo", "scriptInfo"] ctx
  infoF <- pletFields @'["inputs", "mint"] ctxF.txInfo
  scriptInfoF <- pletFieldsMinting ctxF.scriptInfo
  let ownCS = scriptInfoF._0
  mintedValue <- plet $ pfromData infoF.mint
  let ownTkPairs = ptryLookupValue # ownCS # mintedValue
  -- Enforce that only a single token name is minted for this policy
  ownTkPair <- plet (pheadSingleton # ownTkPairs)
  ownTokenName <- plet (pfstBuiltin # ownTkPair)
  ownNumMinted <- plet (psndBuiltin # ownTkPair)
  pvalidateConditions
    [ ownTokenName #== pprotocolParamsTokenData
    , ownNumMinted #== pconstantData 1
    , phasUTxO # oref # infoF.inputs
    ]

-- | Permissioned Minting Policy
-- This minting policy checks for a given permissioned credential in the signatories of the transaction.
-- It allows minting of any number of tokens with any token name so long as the credential authorizes the transaction.
mkPermissionedMinting :: ClosedTerm (PData :--> PAsData PPubKeyHash :--> PScriptContext :--> PUnit)
mkPermissionedMinting = plam $ \_ permissionedCred ctx ->
  pvalidateConditions
    [ ptxSignedByPkh # permissionedCred # (pfield @"signatories" # (pfield @"txInfo" # ctx))
    ]

-- | A nonced always fails script
-- The parameter is used to modify the script hash.
-- This is where the protocol parameters UTxO should reside.
alwaysFailScript :: ClosedTerm (PData :--> PScriptContext :--> PUnit)
alwaysFailScript = plam $ \_ _ctx -> perror
