module SmartTokens.Contracts.ProtocolParams (
  mkProtocolParametersMinting,
  alwaysFailScript,
) where

import Plutarch.LedgerApi.V3 ( PTxOutRef, PScriptContext, PPubKeyHash )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
    ( PAsData,
      PData,
      PEq((#==)),
      type (:-->),
      (#),
      plet,
      perror,
      pconstantData,
      ClosedTerm,
      plam,
      pfromData,
      pfstBuiltin,
      psndBuiltin,
      pletFields,
      PUnit )
import SmartTokens.Core.Utils
    ( pheadSingleton,
      ptryLookupValue,
      phasUTxO,
      pvalidateConditions,
      pletFieldsMinting, ptxSignedByPkh )
import SmartTokens.Types.Constants ( pprotocolParamsTokenData ) 

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
-- This minting policy checks for a given 
mkPermissionedMinting :: ClosedTerm (PAsData PPubKeyHash :--> PScriptContext :--> PUnit)
mkPermissionedMinting = plam $ \oref ctx -> P.do
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
    [ ptxSignedByPkh # permissionedCred # pfield @"txInfo" # ctx
    ]


-- | A nonced always fails script
-- The parameter is used to modify the script hash.
-- This is where the protocol parameters UTxO should reside. 
alwaysFailScript :: ClosedTerm (PData :--> PScriptContext :--> PUnit)
alwaysFailScript = plam $ \_ _ctx -> perror 