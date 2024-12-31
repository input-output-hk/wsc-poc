{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}


{- | This module contains the relevant types for the server.
 -}
module Wst.Server.Types (
  API,
  APIInEra,
  QueryAPI,
  BuildTxAPI,
  IssueProgrammableTokenArgs(..),
  TransferProgrammableTokenArgs(..),
  AddToBlacklistArgs(..),
  SeizeAssetsArgs(..),
  TextEnvelopeJSON(..),
) where

import Cardano.Api (AssetName, Quantity)
import Cardano.Api qualified as C
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Servant.API (Description, Get, JSON, NoContent, Post, ReqBody, type (:>),
                    (:<|>) (..))
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)
import Wst.Offchain.Query (UTxODat (..))

type APIInEra = API C.ConwayEra

newtype TextEnvelopeJSON a = TextEnvelopeJSON{ unTextEnvelopeJSON :: a }

instance C.HasTextEnvelope a => ToJSON (TextEnvelopeJSON a) where
  toJSON = toJSON . C.serialiseToTextEnvelope Nothing . unTextEnvelopeJSON

instance C.HasTextEnvelope a => FromJSON (TextEnvelopeJSON a) where
  parseJSON val = parseJSON val >>= either (fail . show) (pure . TextEnvelopeJSON) . C.deserialiseFromTextEnvelope (C.proxyToAsType Proxy)

type API era =
  "healthcheck" :> Description "Is the server alive?" :> Get '[JSON] NoContent
  :<|> "query" :> QueryAPI era
  :<|> "tx" :> BuildTxAPI era

type QueryAPI era =
  "global-params" :> Description "The UTxO with the global parameters" :> Get '[JSON] (UTxODat era ProgrammableLogicGlobalParams)

{-| Arguments for the programmable-token endpoint. The asset name can be something like "USDW" for the regulated stablecoin.
-}
data IssueProgrammableTokenArgs =
  IssueProgrammableTokenArgs
    { itaIssuer    :: C.Address C.ShelleyAddr
    , itaAssetName :: AssetName
    , itaQuantity  :: Quantity
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data TransferProgrammableTokenArgs =
  TransferProgrammableTokenArgs
    { ttaSender    :: C.Address C.ShelleyAddr
    , ttaRecipient :: C.Address C.ShelleyAddr
    , ttaIssuer    :: C.Address C.ShelleyAddr
    , ttaAssetName :: AssetName
    , ttaQuantity  :: Quantity
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data AddToBlacklistArgs =
  AddToBlacklistArgs
    { atbIssuer           :: C.Address C.ShelleyAddr
    , atbBlacklistAddress :: C.Address C.ShelleyAddr
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data SeizeAssetsArgs =
  SeizeAssetsArgs
    { saIssuer  :: C.Address C.ShelleyAddr
    , saTarget  :: C.Address C.ShelleyAddr
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

type BuildTxAPI era =
  "programmable-token" :>
    ( "issue" :> Description "Create some programmable tokens" :> ReqBody '[JSON] IssueProgrammableTokenArgs :> Post '[JSON] (TextEnvelopeJSON (C.Tx era))
      :<|> "transfer" :> Description "Transfer programmable tokens from one address to another" :> ReqBody '[JSON] TransferProgrammableTokenArgs :> Post '[JSON] (TextEnvelopeJSON (C.Tx era))
      :<|> "blacklist" :> Description "Add a credential to the blacklist" :> ReqBody '[JSON] AddToBlacklistArgs :> Post '[JSON] (TextEnvelopeJSON (C.Tx era))
      :<|> "seize" :> Description "Seize a user's funds" :> ReqBody '[JSON] SeizeAssetsArgs :> Post '[JSON] (TextEnvelopeJSON (C.Tx era))
    )
