{-# LANGUAGE OverloadedStrings #-}

module SmartTokens.Types.Constants(
    protocolParamsToken,
    pprotocolParamsToken,
    pprotocolParamsTokenData,

    -- * Directory node token name
    directoryNodeToken,
    pdirectoryNodeToken,
    pdirectoryNodeTokenData,

    -- * Issuance cbor hex token name
    issuanceCborHexToken,
    pissuanceCborHexToken,
    pissuanceCborHexTokenData,
) where

import Plutarch.LedgerApi.V1 (PTokenName (..))
import Plutarch.Prelude (PAsData, Term, pconstant)
import PlutusLedgerApi.V1 (TokenName (..))
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)

protocolParamsToken :: TokenName
protocolParamsToken = TokenName (stringToBuiltinByteString "ProtocolParams")

pprotocolParamsToken :: Term s PTokenName
pprotocolParamsToken = pconstant protocolParamsToken

pprotocolParamsTokenData :: Term s (PAsData PTokenName)
pprotocolParamsTokenData = pconstant protocolParamsToken

directoryNodeToken :: TokenName
directoryNodeToken = TokenName ""

pdirectoryNodeToken :: Term s PTokenName
pdirectoryNodeToken = pconstant directoryNodeToken

pdirectoryNodeTokenData :: Term s (PAsData PTokenName)
pdirectoryNodeTokenData = pconstant directoryNodeToken

issuanceCborHexToken :: TokenName
issuanceCborHexToken = TokenName (stringToBuiltinByteString "IssuanceCborHex")

pissuanceCborHexToken :: Term s PTokenName
pissuanceCborHexToken = pconstant issuanceCborHexToken

pissuanceCborHexTokenData :: Term s (PAsData PTokenName)
pissuanceCborHexTokenData = pconstant issuanceCborHexToken

