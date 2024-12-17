{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Types.Constants(
  psetNodePrefix,
  pnodeKeyTN,
  poriginNodeTN,
  ptryParseNodeKey
) where

import Plutarch.LedgerApi.V1 (PTokenName (..))
import Plutarch.Prelude
import Plutarch.Core.Utils (pnonew, passert, pisPrefixOf)
import PlutusLedgerApi.V1 (TokenName(..))
import Plutarch.Builtin (PDataNewtype(..))
import qualified Plutarch.Monadic as P

psetNodePrefix :: ClosedTerm PByteString
psetNodePrefix = pconstant "FSN"

pnodeKeyTN :: ClosedTerm (PByteString :--> PTokenName)
pnodeKeyTN = phoistAcyclic $
  plam $
    \nodeKey -> pcon $ PTokenName $ pcon $ PDataNewtype $ pdata $ psetNodePrefix <> nodeKey

poriginNodeTN :: ClosedTerm PTokenName
poriginNodeTN =
  let tn :: TokenName
      tn = "FSN"
   in pconstant tn

ptryParseNodeKey :: ClosedTerm (PTokenName :--> PByteString)
ptryParseNodeKey = phoistAcyclic $
  plam $ \(pnonew -> tn) -> P.do
    let prefixLength = 3
        tnLength = plengthBS # tn
        key = psliceBS # prefixLength # (tnLength - prefixLength) # tn
    passert "incorrect node prefix" $ pisPrefixOf # psetNodePrefix # tn
    pif (prefixLength #< tnLength) key perror 