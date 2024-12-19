{-# LANGUAGE OverloadedStrings #-}
module SmartTokens.Types.Constants(
    protocolParamsToken,
    pprotocolParamsToken,
    pprotocolParamsTokenData,

    -- * Directory node token name
    directoryNodeToken,
    pdirectoryNodeToken,
    pdirectoryNodeTokenData
) where

import Plutarch.LedgerApi.V1 (PTokenName (..))
import Plutarch.Prelude (ClosedTerm, PAsData, pconstant, pconstantData)
import PlutusLedgerApi.V1 (TokenName (..))

protocolParamsToken :: TokenName
protocolParamsToken = "ProtocolParams"

pprotocolParamsToken :: ClosedTerm PTokenName
pprotocolParamsToken = pconstant protocolParamsToken

pprotocolParamsTokenData :: ClosedTerm (PAsData PTokenName)
pprotocolParamsTokenData = pconstantData protocolParamsToken

directoryNodeToken :: TokenName
directoryNodeToken = ""

pdirectoryNodeToken :: ClosedTerm PTokenName
pdirectoryNodeToken = pconstant directoryNodeToken

pdirectoryNodeTokenData :: ClosedTerm (PAsData PTokenName)
pdirectoryNodeTokenData = pconstantData directoryNodeToken
