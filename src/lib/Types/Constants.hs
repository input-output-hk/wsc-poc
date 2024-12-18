{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Types.Constants where

import Plutarch
import Plutarch.LedgerApi.V1 (PTokenName (..))
import Plutarch.Prelude
import SmartTokens.Core.Utils
import PlutusLedgerApi.V1 (TokenName(..))
import Plutarch.Builtin (PDataNewtype(..))

{-# INLINABLE djedOrderToken #-}
djedOrderToken :: TokenName
djedOrderToken = "DjedOrderTicket"

pdjedOrderToken :: ClosedTerm PTokenName 
pdjedOrderToken = pconstant djedOrderToken

psetNodePrefix :: ClosedTerm PByteString
psetNodePrefix = pconstant "FSN"

pnodeKeyTN :: ClosedTerm (PByteString :--> PTokenName)
pnodeKeyTN = phoistAcyclic $
  plam $
    \nodeKey -> pcon $ PTokenName $ pcon $ PDataNewtype $ pdata nodeKey

poriginNodeTN :: ClosedTerm PTokenName
poriginNodeTN =
  let tn :: TokenName
      tn = ""
   in pconstant tn

-- ptryParseNodeKey :: ClosedTerm (PTokenName :--> PByteString)
-- ptryParseNodeKey = phoistAcyclic $
--   plam $ \(pnonew -> tn) -> P.do
--     let prefixLength = 3
--         tnLength = plengthBS # tn
--         key = psliceBS # prefixLength # (tnLength - prefixLength) # tn
--     passert "incorrect node prefix" $ pisPrefixOf # psetNodePrefix # tn
--     pif (prefixLength #< tnLength) key perror 
-- ptryParseNodeKey :: ClosedTerm (PTokenName :--> PByteString)

ptryParseNodeKey :: ClosedTerm (PTokenName :--> PByteString)
ptryParseNodeKey = phoistAcyclic $
  plam $ \(pnonew -> tn) -> tn