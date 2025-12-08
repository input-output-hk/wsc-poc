{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo       #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Types.Constants(
  pnodeKeyTN,
  poriginNodeTN,
  pparseNodeKey
) where

import Plutarch.LedgerApi.V1 (PTokenName (..))
import Plutarch.Prelude
import PlutusLedgerApi.V1 (TokenName (..))

pnodeKeyTN :: Term s PByteString -> Term s PTokenName
pnodeKeyTN nodeKey = pcon $ PTokenName $ nodeKey

poriginNodeTN :: Term s PTokenName
poriginNodeTN =
  let tn :: TokenName
      tn = TokenName ""
   in pconstant tn

pparseNodeKey :: Term s PTokenName -> Term s PByteString
pparseNodeKey tn = pto tn
