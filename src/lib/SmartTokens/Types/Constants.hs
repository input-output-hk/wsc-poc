{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module SmartTokens.Types.Constants where

import Plutarch.LedgerApi.V1 (PTokenName (..))
import Plutarch.Prelude
    ( PAsData, pconstantData, ClosedTerm, pconstant )
import PlutusLedgerApi.V1 (TokenName(..))

{-# INLINABLE protocolParamsToken #-}
protocolParamsToken :: TokenName
protocolParamsToken = "ProtocolParams"

pprotocolParamsToken :: ClosedTerm PTokenName 
pprotocolParamsToken = pconstant protocolParamsToken

pprotocolParamsTokenData :: ClosedTerm (PAsData PTokenName)
pprotocolParamsTokenData = pconstantData protocolParamsToken


