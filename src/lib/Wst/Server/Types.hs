{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}


{- | This module contains the relevant types for the server.
 -}
module Wst.Server.Types (
  API,
  APIInEra,
  QueryAPI,
  BuildTxAPI,

  -- * Build tx arguments
  IssueProgrammableTokenArgs(..),
  TransferProgrammableTokenArgs(..),
  AddToBlacklistArgs(..),
  SeizeAssetsArgs(..),

  -- * Newtypes
  TextEnvelopeJSON(..),
  SerialiseAddress(..)
) where

import Cardano.Api (AssetName, Quantity)
import Cardano.Api qualified as C
import Control.Lens ((&), (.~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as JSON
import Data.OpenApi (NamedSchema (..), OpenApiType (OpenApiObject),
                     Referenced (Inline), ToSchema (..))
import Data.OpenApi.Internal (OpenApiType (OpenApiString))
import Data.OpenApi.Lens qualified as L
import Data.OpenApi.ParamSchema (ToParamSchema (..))
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Servant (FromHttpApiData (..), ToHttpApiData (toUrlPiece))
import Servant.API (Capture, Description, Get, JSON, NoContent, Post, ReqBody,
                    type (:>), (:<|>) (..))
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)
import Wst.JSON.Utils qualified as JSON
import Wst.Offchain.Query (UTxODat (..))

type APIInEra = "api" :> "v1" :> API C.ConwayEra

newtype TextEnvelopeJSON a = TextEnvelopeJSON{ unTextEnvelopeJSON :: a }

instance C.HasTextEnvelope a => ToJSON (TextEnvelopeJSON a) where
  toJSON = toJSON . C.serialiseToTextEnvelope Nothing . unTextEnvelopeJSON

instance C.HasTextEnvelope a => FromJSON (TextEnvelopeJSON a) where
  parseJSON val = parseJSON val >>= either (fail . show) (pure . TextEnvelopeJSON) . C.deserialiseFromTextEnvelope (C.proxyToAsType Proxy)

instance C.HasTextEnvelope a => ToSchema (TextEnvelopeJSON a) where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "TextEnvelopeJSON")
    $ mempty
        & L.type_ ?~ OpenApiObject
        & L.description ?~ "Text envelope"
        & L.properties .~
          [ ("cborHex", Inline $ mempty & L.type_ ?~ OpenApiString & L.description ?~ "The CBOR-serialised value, base-16 encoded")
          , ("description", Inline $ mempty & L.type_ ?~ OpenApiString & L.description ?~ "Description of the serialised value")
          , ("type", Inline $ mempty & L.type_ ?~ OpenApiString & L.description ?~ "Type of the serialised value")
          ]

newtype SerialiseAddress a = SerialiseAddress{unSerialiseAddress :: a }

deriving newtype instance ToJSON (SerialiseAddress (C.Address C.ShelleyAddr))
deriving newtype instance FromJSON (SerialiseAddress (C.Address C.ShelleyAddr))
deriving newtype instance ToSchema (SerialiseAddress (C.Address C.ShelleyAddr))

instance ToParamSchema (SerialiseAddress a) where
  toParamSchema _proxy =
    mempty
      & L.type_ ?~ OpenApiString
      & L.description ?~ "bech32-serialised cardano address"
      & L.example ?~ "addr1q9d42egme33z960rr8vlnt69lpmythdpm7ydk2e6k5nj5ghay9rg60vw49kejfah76sqeh4yshlsntgg007y0wgjlfwju6eksr"

instance C.SerialiseAddress a => FromHttpApiData (SerialiseAddress a) where
  parseUrlPiece =
    maybe (Left "Failed to deserialise address") (Right . SerialiseAddress) . C.deserialiseAddress (C.proxyToAsType Proxy)

instance C.SerialiseAddress a => ToHttpApiData (SerialiseAddress a) where
  toUrlPiece = C.serialiseAddress . unSerialiseAddress

type API era =
  "healthcheck" :> Description "Is the server alive?" :> Get '[JSON] NoContent
  :<|> "query" :> QueryAPI era
  :<|> "tx" :> BuildTxAPI era

type QueryAPI era =
  "global-params" :> Description "The UTxO with the global parameters" :> Get '[JSON] (UTxODat era ProgrammableLogicGlobalParams)
  :<|> "blacklist" :> Description "The list of addresses that have been blacklisted" :> Capture "address" (SerialiseAddress (C.Address C.ShelleyAddr)) :>  Get '[JSON] [C.Hash C.PaymentKey]
  :<|> "user-funds" :> Description "Total value locked in programmable token outputs addressed to the user" :> Capture "address" (SerialiseAddress (C.Address C.ShelleyAddr)) :> Get '[JSON] C.Value
  :<|> "all-funds" :> Description "Total value of all programmable tokens" :> Get '[JSON] C.Value
  :<|> "address" :> Description "The user's receiving address for programmable tokens" :> Capture "address" (SerialiseAddress (C.Address C.ShelleyAddr)) :> Get '[JSON] (C.Address C.ShelleyAddr)

{-| Arguments for the programmable-token endpoint. The asset name can be something like "USDW" for the regulated stablecoin.
-}
data IssueProgrammableTokenArgs =
  IssueProgrammableTokenArgs
    { itaIssuer    :: C.Address C.ShelleyAddr
    , itaAssetName :: AssetName
    , itaQuantity  :: Quantity
    }
    deriving stock (Eq, Show, Generic)

jsonOptions3 :: JSON.Options
jsonOptions3 = JSON.customJsonOptions 3

jsonOptions2 :: JSON.Options
jsonOptions2 = JSON.customJsonOptions 2

instance ToJSON IssueProgrammableTokenArgs where
  toJSON = JSON.genericToJSON jsonOptions3
  toEncoding = JSON.genericToEncoding jsonOptions3

instance FromJSON IssueProgrammableTokenArgs where
  parseJSON = JSON.genericParseJSON jsonOptions3

instance ToSchema IssueProgrammableTokenArgs where
  declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions jsonOptions3)

data TransferProgrammableTokenArgs =
  TransferProgrammableTokenArgs
    { ttaSender    :: C.Address C.ShelleyAddr
    , ttaRecipient :: C.Address C.ShelleyAddr
    , ttaIssuer    :: C.Address C.ShelleyAddr
    , ttaAssetName :: AssetName
    , ttaQuantity  :: Quantity
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON TransferProgrammableTokenArgs where
  toJSON = JSON.genericToJSON jsonOptions3
  toEncoding = JSON.genericToEncoding jsonOptions3

instance FromJSON TransferProgrammableTokenArgs where
  parseJSON = JSON.genericParseJSON jsonOptions3

instance ToSchema TransferProgrammableTokenArgs where
  declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions jsonOptions3)

data AddToBlacklistArgs =
  AddToBlacklistArgs
    { atbIssuer           :: C.Address C.ShelleyAddr
    , atbBlacklistAddress :: C.Address C.ShelleyAddr
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON AddToBlacklistArgs where
  toJSON = JSON.genericToJSON jsonOptions3
  toEncoding = JSON.genericToEncoding jsonOptions3

instance FromJSON AddToBlacklistArgs where
  parseJSON = JSON.genericParseJSON jsonOptions3

instance ToSchema AddToBlacklistArgs where
  declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions jsonOptions3)

data SeizeAssetsArgs =
  SeizeAssetsArgs
    { saIssuer  :: C.Address C.ShelleyAddr
    , saTarget  :: C.Address C.ShelleyAddr
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON SeizeAssetsArgs where
  toJSON = JSON.genericToJSON jsonOptions2
  toEncoding = JSON.genericToEncoding jsonOptions2

instance FromJSON SeizeAssetsArgs where
  parseJSON = JSON.genericParseJSON jsonOptions2

instance ToSchema SeizeAssetsArgs where
  declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions jsonOptions2)

type BuildTxAPI era =
  "programmable-token" :>
    ( "issue" :> Description "Create some programmable tokens" :> ReqBody '[JSON] IssueProgrammableTokenArgs :> Post '[JSON] (TextEnvelopeJSON (C.Tx era))
      :<|> "transfer" :> Description "Transfer programmable tokens from one address to another" :> ReqBody '[JSON] TransferProgrammableTokenArgs :> Post '[JSON] (TextEnvelopeJSON (C.Tx era))
      :<|> "blacklist" :> Description "Add a credential to the blacklist" :> ReqBody '[JSON] AddToBlacklistArgs :> Post '[JSON] (TextEnvelopeJSON (C.Tx era))
      :<|> "seize" :> Description "Seize a user's funds" :> ReqBody '[JSON] SeizeAssetsArgs :> Post '[JSON] (TextEnvelopeJSON (C.Tx era))
    )
