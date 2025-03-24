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
import Plutarch.Prelude (ClosedTerm, PAsData, pconstant)
import PlutusLedgerApi.V1 (TokenName (..))
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)

protocolParamsToken :: TokenName
protocolParamsToken = TokenName (stringToBuiltinByteString "ProtocolParams")

pprotocolParamsToken :: ClosedTerm PTokenName
pprotocolParamsToken = pconstant protocolParamsToken

pprotocolParamsTokenData :: ClosedTerm (PAsData PTokenName)
pprotocolParamsTokenData = pconstant protocolParamsToken

directoryNodeToken :: TokenName
directoryNodeToken = TokenName ""

pdirectoryNodeToken :: ClosedTerm PTokenName
pdirectoryNodeToken = pconstant directoryNodeToken

pdirectoryNodeTokenData :: ClosedTerm (PAsData PTokenName)
pdirectoryNodeTokenData = pconstant directoryNodeToken
