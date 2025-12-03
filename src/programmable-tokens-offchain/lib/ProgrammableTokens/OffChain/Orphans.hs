{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Orphan instances
-}
module ProgrammableTokens.OffChain.Orphans() where

import Cardano.Api qualified as C
import Control.Lens ((&), (.~), (?~))
import Data.OpenApi.Internal (NamedSchema (..),
                              OpenApiType (OpenApiInteger, OpenApiObject, OpenApiString),
                              Referenced (Inline), Schema)
import Data.OpenApi.Lens qualified as L
import Data.OpenApi.Schema (ToSchema (..))
import Data.Typeable (Typeable)

instance (Typeable ctx, Typeable era) => ToSchema (C.TxOut ctx era) where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "TxOut")
    $ mempty
      & L.type_ ?~ OpenApiObject
      & L.description ?~ "Global parameters of the programmable token directory"
      & L.properties .~
        [ ( "address"
          , Inline addrSchema
          )
        , ( "datum"
          , Inline $ mempty
              & L.type_ ?~ OpenApiObject
              & L.description ?~ "the datum of the output (if any)"
          )
        , ( "inlineDatum"
          , Inline $ mempty
              & L.type_ ?~ OpenApiObject
              & L.description ?~ "the inline datum of the output (if any)"
          )
        , ( "inlineDatumRaw"
          , Inline $ mempty
              & L.type_ ?~ OpenApiObject
              & L.description ?~ "the inline datum of the output (if any), CBOR serialised and base-16 encoded"
          )
        , ( "inlineDatumhash"
          , Inline $ mempty
              & L.type_ ?~ OpenApiString
              & L.description ?~ "hash of the inline datum of the output (if it exists)"
          )
        , ( "referenceScript"
          , Inline $ mempty
              & L.type_ ?~ OpenApiObject
              & L.description ?~ "reference script (if any), text envelope format"
          )
        , ( "value"
          , Inline valueSchema
          )
        ]

instance ToSchema C.TxIn where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "TxIn")
    $ mempty
        & L.type_ ?~ OpenApiString
        & L.description ?~ "TxIn consisting of (Transaction hash + # + index)"
        & L.example ?~ "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#2"

instance ToSchema (C.Hash C.PaymentKey) where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "Hash PaymentKey")
    $ mempty
        & L.type_ ?~ OpenApiString
        & L.description ?~ "Hash of a payment key"
        & L.example ?~ "f6ac5676b58d8ce280c1f09af4a2e82dd58c1aa2fb075aa005afa1da"

valueSchema :: Schema
valueSchema = mempty
  & L.type_ ?~ OpenApiObject
  & L.description ?~ "Value locked in the output. Always includes a 'lovelace' key, may include other keys if non-Ada assets are present."
  & L.properties .~
    [ ("lovelace", Inline $ mempty & L.type_ ?~ OpenApiInteger)
    ]

instance ToSchema C.Value where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "Value") valueSchema

addrSchema :: Schema
addrSchema = mempty
  & L.type_ ?~ OpenApiString
  & L.description ?~ "bech32-encoded cardano address"
  & L.example ?~ "addr_test1qpju2uhn72ur6j5alln6nz7dqcgcjal7xjaw7lwdjdaex4qhr3xpz63fjwvlpsnu8efnhfdja78d3vkv8ks6ac09g3usemu2yl"

instance ToSchema (C.Address C.ShelleyAddr) where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "Address") addrSchema

instance ToSchema C.AssetName where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "Asset name")
    $ mempty
        & L.type_ ?~ OpenApiString

instance ToSchema C.Quantity where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "Quantity")
    $ mempty
        & L.type_ ?~ OpenApiInteger

instance ToSchema C.TxId where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "TxId")
    $ mempty
        & L.type_ ?~ OpenApiString

instance ToSchema C.PolicyId where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "PolicyId")
    $ mempty
        & L.type_ ?~ OpenApiString
        & L.description ?~ "Policy ID"
        & L.example ?~ "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53"

instance ToSchema C.ScriptHash where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "ScriptHash")
    $ mempty
        & L.type_ ?~ OpenApiString
        & L.description ?~ "Script hash"
        & L.example ?~ "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53"
