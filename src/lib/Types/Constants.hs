{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Types.Constants(
  pnodeKeyTN,
  poriginNodeTN,
  ptryParseNodeKey
) where

import Plutarch.LedgerApi.V1 (PTokenName (..))
import Plutarch.Prelude
import Plutarch.Core.Utils (pnonew)
import PlutusLedgerApi.V1 (TokenName(..))
import Plutarch.Builtin (PDataNewtype(..))

pnodeKeyTN :: ClosedTerm (PByteString :--> PTokenName)
pnodeKeyTN = phoistAcyclic $
  plam $
    \nodeKey -> pcon $ PTokenName $ pcon $ PDataNewtype $ pdata nodeKey

poriginNodeTN :: ClosedTerm PTokenName
poriginNodeTN =
  let tn :: TokenName
      tn = ""
   in pconstant tn

ptryParseNodeKey :: ClosedTerm (PTokenName :--> PByteString)
ptryParseNodeKey = phoistAcyclic $
  plam $ \(pnonew -> tn) -> tn