{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}


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
  BlacklistNodeArgs(..),
  SeizeAssetsArgs(..),
  MultiSeizeAssetsArgs(..),
  AddVKeyWitnessArgs(..),
  RegisterTransferScriptsArgs(..),
  BlacklistInitArgs(..),

  -- * Response types
  UserBalanceResponse(..),

  -- * Newtypes
  TextEnvelopeJSON(..),
  SerialiseAddress(..),
  PolicyIdHex(..)
) where

import Cardano.Api (AssetName, Quantity)
import Cardano.Api qualified as C
import Control.Lens ((&), (.~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as JSON
import Data.OpenApi (NamedSchema (..),
                     OpenApiType (OpenApiInteger, OpenApiObject),
                     Referenced (Inline), ToSchema (..))
import Data.OpenApi.Internal (OpenApiType (OpenApiString))
import Data.OpenApi.Lens qualified as L
import Data.OpenApi.ParamSchema (ToParamSchema (..))
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import GHC.Generics (Generic)
import ProgrammableTokens.JSON.Utils qualified as JSON
import Servant (FromHttpApiData (..), ToHttpApiData (toUrlPiece))
import Servant.API (Capture, Description, Get, JSON, NoContent, Post, ReqBody,
                    type (:>), (:<|>) (..))
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)
import Wst.Offchain.BuildTx.TransferLogic (BlacklistReason, SeizeReason)
import Wst.Offchain.Query (UTxODat (..))

type APIInEra = "api" :> "v1" :> API C.ConwayEra

newtype TextEnvelopeJSON a = TextEnvelopeJSON{ unTextEnvelopeJSON :: a }
  deriving newtype (Eq, Show)

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
  deriving newtype (Eq, Show)

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

instance ToParamSchema PolicyIdHex where
  toParamSchema _ =
    mempty
      & L.type_ ?~ OpenApiString
      & L.description ?~ "hex-encoded policy identifier"
      & L.example ?~ "4cfd5e2b0c534b4e0cda0f5d84df7e0d3d3c6a74c0e5f3d823a58a38"

newtype PolicyIdHex = PolicyIdHex { unPolicyIdHex :: C.PolicyId }
  deriving stock (Eq, Show)

instance FromHttpApiData PolicyIdHex where
  parseUrlPiece txt =
    case C.deserialiseFromRawBytesHex C.AsPolicyId (TextEncoding.encodeUtf8 txt) of
      Left err -> Left ("Failed to parse policy id: " <> Text.pack (show err))
      Right pid -> Right (PolicyIdHex pid)

instance ToHttpApiData PolicyIdHex where
  toUrlPiece (PolicyIdHex pid) = C.serialiseToRawBytesHexText pid

type API era =
  "healthcheck" :> Description "Is the server alive?" :> Get '[JSON] NoContent
  :<|> "query" :> QueryAPI era
  :<|> "tx" :> BuildTxAPI era

type QueryAPI era =
  "global-params" :> Description "The UTxO with the global parameters" :> Get '[JSON] (UTxODat era ProgrammableLogicGlobalParams)
  :<|> "blacklist" :> Description "The list of addresses that have been blacklisted" :> Capture "address" (SerialiseAddress (C.Address C.ShelleyAddr)) :>  Get '[JSON] [C.Hash C.PaymentKey]
  :<|> "user-funds" :> Description "Total value locked in programmable token outputs addressed to the user" :> Capture "address" (SerialiseAddress (C.Address C.ShelleyAddr)) :> Get '[JSON] UserBalanceResponse
  :<|> "all-funds" :> Description "Total value of all programmable tokens" :> Get '[JSON] C.Value
  :<|> "address" :> Description "The user's receiving address for programmable tokens" :> Capture "address" (SerialiseAddress (C.Address C.ShelleyAddr)) :> Get '[JSON] (C.Address C.ShelleyAddr)
  :<|> "freeze-policy-id" :> Description "The policy ID for the freeze and seize programmable token policy associated with this user" :> Capture "address" (SerialiseAddress (C.Address C.ShelleyAddr)) :> Get '[JSON] C.PolicyId
  :<|> "stake-scripts" :> Description "The stake scripts for the programmable token" :> Capture "address" (SerialiseAddress (C.Address C.ShelleyAddr)) :> Get '[JSON] [C.ScriptHash]
  :<|> "user-total-programmable-value" :> Description "Total value of all programmable tokens addressed to the user" :> Capture "address" (SerialiseAddress (C.Address C.ShelleyAddr)) :> Get '[JSON] C.Value
  :<|> "policy-issuer" :> Description "Issuer address associated with a freeze/seize policy id" :> Capture "policy_id" PolicyIdHex :> Get '[JSON] (SerialiseAddress (C.Address C.ShelleyAddr))

data RegisterTransferScriptsArgs =
  RegisterTransferScriptsArgs
    { rsaIssuer :: C.Address C.ShelleyAddr
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON RegisterTransferScriptsArgs where
  toJSON = JSON.genericToJSON jsonOptions3
  toEncoding = JSON.genericToEncoding jsonOptions3

instance ToSchema RegisterTransferScriptsArgs where
  declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions jsonOptions3)

instance FromJSON RegisterTransferScriptsArgs where
  parseJSON = JSON.genericParseJSON jsonOptions3

{-| Arguments for the programmable-token endpoint. The asset name can be something like "USDW" for the regulated stablecoin.
-}
data IssueProgrammableTokenArgs =
  IssueProgrammableTokenArgs
    { itaIssuer    :: C.Address C.ShelleyAddr
    , itaAssetName :: AssetName
    , itaQuantity  :: Quantity
    , itaRecipient :: C.Address C.ShelleyAddr
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
    , ttaSubmitFailingTx :: Bool
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON TransferProgrammableTokenArgs where
  toJSON = JSON.genericToJSON jsonOptions3
  toEncoding = JSON.genericToEncoding jsonOptions3

instance FromJSON TransferProgrammableTokenArgs where
  parseJSON = JSON.genericParseJSON jsonOptions3

instance ToSchema TransferProgrammableTokenArgs where
  declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions jsonOptions3)

data BlacklistNodeArgs =
  BlacklistNodeArgs
    { bnaIssuer           :: C.Address C.ShelleyAddr
    , bnaBlacklistAddress :: C.Address C.ShelleyAddr
    , bnaReason           :: BlacklistReason
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON BlacklistNodeArgs where
  toJSON = JSON.genericToJSON jsonOptions3
  toEncoding = JSON.genericToEncoding jsonOptions3

instance FromJSON BlacklistNodeArgs where
  parseJSON = JSON.genericParseJSON jsonOptions3

instance ToSchema BlacklistNodeArgs where
  declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions jsonOptions3)

data SeizeAssetsArgs =
  SeizeAssetsArgs
    { saIssuer  :: C.Address C.ShelleyAddr
    , saTarget  :: C.Address C.ShelleyAddr
    , saReason  :: SeizeReason
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON SeizeAssetsArgs where
  toJSON = JSON.genericToJSON jsonOptions2
  toEncoding = JSON.genericToEncoding jsonOptions2

instance FromJSON SeizeAssetsArgs where
  parseJSON = JSON.genericParseJSON jsonOptions2

instance ToSchema SeizeAssetsArgs where
  declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions jsonOptions2)

data MultiSeizeAssetsArgs =
  MultiSeizeAssetsArgs
    { msaIssuer  :: C.Address C.ShelleyAddr
    , msaTarget  :: [C.Address C.ShelleyAddr]
    , msaReason  :: SeizeReason
    , msaNumUTxOsToSeize :: Int
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON MultiSeizeAssetsArgs where
  toJSON = JSON.genericToJSON jsonOptions2
  toEncoding = JSON.genericToEncoding jsonOptions2

instance FromJSON MultiSeizeAssetsArgs where
  parseJSON = JSON.genericParseJSON jsonOptions2

instance ToSchema MultiSeizeAssetsArgs where
  declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions jsonOptions2)

data AddVKeyWitnessArgs era =
  AddVKeyWitnessArgs
    { avwTx :: TextEnvelopeJSON (C.Tx era)
    , avwVKeyWitness :: TextEnvelopeJSON (C.KeyWitness era)
    }
    deriving stock (Generic)

instance C.IsShelleyBasedEra era => ToJSON (AddVKeyWitnessArgs era) where
  toJSON = JSON.genericToJSON jsonOptions3
  toEncoding = JSON.genericToEncoding jsonOptions3

instance C.IsShelleyBasedEra era => FromJSON (AddVKeyWitnessArgs era) where
  parseJSON = JSON.genericParseJSON jsonOptions3

instance C.IsShelleyBasedEra era => ToSchema (AddVKeyWitnessArgs era) where
  declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions jsonOptions2)

type BuildTxAPI era =
  "programmable-token" :>
    ( "issue" :> Description "Create some programmable tokens" :> ReqBody '[JSON] IssueProgrammableTokenArgs :> Post '[JSON] (TextEnvelopeJSON (C.Tx era))
      :<|> "transfer" :> Description "Transfer programmable tokens from one address to another" :> ReqBody '[JSON] TransferProgrammableTokenArgs :> Post '[JSON] (TextEnvelopeJSON (C.Tx era))
      :<|> "blacklist" :> Description "Add a credential to the blacklist" :> ReqBody '[JSON] BlacklistNodeArgs :> Post '[JSON] (TextEnvelopeJSON (C.Tx era))
      :<|> "unblacklist" :> Description "Remove a credential from the blacklist" :> ReqBody '[JSON] BlacklistNodeArgs :> Post '[JSON] (TextEnvelopeJSON (C.Tx era))
      :<|> "seize" :> Description "Seize a user's funds" :> ReqBody '[JSON] SeizeAssetsArgs :> Post '[JSON] (TextEnvelopeJSON (C.Tx era))
      :<|> "seize-multi" :> Description "Seize multiple user's funds" :> ReqBody '[JSON] MultiSeizeAssetsArgs :> Post '[JSON] (TextEnvelopeJSON (C.Tx era))
      :<|> "register-transfer-scripts" :> Description "Register the transfer scripts" :> ReqBody '[JSON] RegisterTransferScriptsArgs :> Post '[JSON] (TextEnvelopeJSON (C.Tx era))
      :<|> "blacklist-init" :> Description "Initialize the blacklist" :> ReqBody '[JSON] BlacklistInitArgs :> Post '[JSON] (TextEnvelopeJSON (C.Tx era))
    )
  :<|>
  "add-vkey-witness" :> Description "Add a VKey witness to a transaction" :> ReqBody '[JSON] (AddVKeyWitnessArgs era) :> Post '[JSON] (TextEnvelopeJSON (C.Tx era))
  :<|>
  "submit" :> Description "Submit a transaction to the blockchain" :> ReqBody '[JSON] (TextEnvelopeJSON (C.Tx era)) :> Post '[JSON] C.TxId

{-| Arguments for the blacklist-init endpoint.
-}
data BlacklistInitArgs =
  BlacklistInitArgs
    { biaIssuer :: C.Address C.ShelleyAddr
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON BlacklistInitArgs where
  toJSON = JSON.genericToJSON jsonOptions3
  toEncoding = JSON.genericToEncoding jsonOptions3

instance FromJSON BlacklistInitArgs where
  parseJSON = JSON.genericParseJSON jsonOptions3

instance ToSchema BlacklistInitArgs where
  declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions jsonOptions3)

{-| Response to the user-balance query
-}
data UserBalanceResponse =
  UserBalanceResponse
    { ubrProgrammableTokens :: C.Value
    , ubrUserLovelace       :: C.Quantity -- ^ Total Ada locked by the user's public key hash (that is, value NOT in programmable token outputs)
    , ubrAdaOnlyOutputs     :: Int -- ^ Count of ada-only outputs locked by user's public key hash
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON UserBalanceResponse where
  toJSON = JSON.genericToJSON jsonOptions3
  toEncoding = JSON.genericToEncoding jsonOptions3

instance FromJSON UserBalanceResponse where
  parseJSON = JSON.genericParseJSON jsonOptions3

instance ToSchema UserBalanceResponse where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "UserBalanceResponse")
    $ mempty
        & L.type_ ?~ OpenApiObject
        & L.description ?~ "Response to user-funds query"
        & L.properties .~
          [ ("programmable_tokens", Inline $ mempty & L.type_ ?~ OpenApiObject & L.description ?~ "Combined value locked in programmable token outputs addressed to the user")
          , ("user_lovelace", Inline $ mempty & L.type_ ?~ OpenApiInteger & L.description ?~ "Total Ada locked by the user's public key hash")
          , ("ada_only_outputs", Inline $ mempty & L.type_ ?~ OpenApiInteger & L.description ?~ "Count of ada-only outputs locked by user's public key hash")
          ]
