{-# LANGUAGE OverloadedStrings #-}
module SmartTokens.Types.Constants(
    protocolParamsToken,
    pprotocolParamsToken,
    pprotocolParamsTokenData
) where

import Plutarch.LedgerApi.V1 (PTokenName (..))
import Plutarch.Prelude
    ( PAsData, pconstantData, ClosedTerm, pconstant )
import PlutusLedgerApi.V1 (TokenName(..))

protocolParamsToken :: TokenName
protocolParamsToken = "ProtocolParams"

pprotocolParamsToken :: ClosedTerm PTokenName 
pprotocolParamsToken = pconstant protocolParamsToken

pprotocolParamsTokenData :: ClosedTerm (PAsData PTokenName)
pprotocolParamsTokenData = pconstantData protocolParamsToken


