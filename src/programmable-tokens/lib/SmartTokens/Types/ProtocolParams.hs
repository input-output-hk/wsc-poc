{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}


{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QualifiedDo           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module SmartTokens.Types.ProtocolParams (
  ProgrammableLogicGlobalParams (..),
  PProgrammableLogicGlobalParams (..),
) where

import Cardano.Api.Shelley qualified as C
import Control.Lens ((&), (.~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (Bifunctor (..))
import Data.OpenApi.Internal (OpenApiType (OpenApiArray, OpenApiObject, OpenApiString),
                              Referenced (Inline))
import Data.OpenApi.Lens qualified as L
import Data.OpenApi.ParamSchema (ToParamSchema (..))
import Data.OpenApi.Schema (ToSchema (..), defaultSchemaOptions,
                            paramSchemaToNamedSchema)
import Generics.SOP qualified as SOP
import GHC.Generics (Generic)
import Plutarch.Internal.Lift
import Plutarch.LedgerApi.V3 (PCredential, PCurrencySymbol)
import Plutarch.Prelude
import Plutarch.Repr.Data
import PlutusLedgerApi.V3 (Credential, CurrencySymbol)
import PlutusTx qualified
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Prelude qualified as PlutusTxPrelude

-- TODO:
-- Figure out why deriving PlutusType breaks when I uncomment this
-- and disable no-deferred-type-errors
data ProgrammableLogicGlobalParams = ProgrammableLogicGlobalParams
  { directoryNodeCS :: CurrencySymbol
  , progLogicCred :: Credential
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

instance PlutusTx.FromData ProgrammableLogicGlobalParams where
  fromBuiltinData dat = do
    xs <- BI.chooseData dat Nothing Nothing (Just $ BI.unsafeDataAsList dat) Nothing Nothing
    directoryNodeCurrSymb <- PlutusTx.fromBuiltinData $ BI.head xs
    progLogicCred <- PlutusTx.fromBuiltinData $ BI.head $ BI.tail xs
    PlutusTxPrelude.pure PlutusTxPrelude.$ ProgrammableLogicGlobalParams directoryNodeCurrSymb progLogicCred

instance PlutusTx.ToData ProgrammableLogicGlobalParams where
  toBuiltinData ProgrammableLogicGlobalParams{directoryNodeCS, progLogicCred} =
    let directoryNodeCS' = PlutusTx.toBuiltinData directoryNodeCS
        progLogicCred' = PlutusTx.toBuiltinData progLogicCred
     in BI.mkList $ BI.mkCons directoryNodeCS' (BI.mkCons progLogicCred' $ BI.mkNilData BI.unitval)

data PProgrammableLogicGlobalParams (s :: S)
  = PProgrammableLogicGlobalParams
      { pdirectoryNodeCS :: Term s (PAsData PCurrencySymbol)
      , pprogLogicCred :: Term s (PAsData PCredential)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataRec PProgrammableLogicGlobalParams)

deriving via DeriveDataPLiftable (PAsData PProgrammableLogicGlobalParams) ProgrammableLogicGlobalParams
  instance PLiftable PProgrammableLogicGlobalParams

-- We're using the Data representation of the PlutusLedgerApi types here
-- Because it is somewhat human-readable (more so than the hex representation)

plutusDataToJSON :: forall a. (PlutusTx.ToData a) => a -> Aeson.Value
plutusDataToJSON = C.scriptDataToJson C.ScriptDataJsonNoSchema . C.unsafeHashableScriptData . C.fromPlutusData . PlutusTx.toData

plutusDataFromJSON :: forall a. (PlutusTx.FromData a) => Aeson.Value -> Either String a
plutusDataFromJSON val = do
  k <- bimap show C.getScriptData $ C.scriptDataFromJson C.ScriptDataJsonNoSchema val
  maybe (Left "fromData failed") Right (PlutusTx.fromData $ C.toPlutusData k)

instance ToJSON ProgrammableLogicGlobalParams where
  toJSON ProgrammableLogicGlobalParams{directoryNodeCS, progLogicCred} =
    object
      [ "directory_node_currency_symbol" .= plutusDataToJSON directoryNodeCS
      , "programmable_logic_credential"  .= plutusDataToJSON progLogicCred
      ]

instance FromJSON ProgrammableLogicGlobalParams where
  parseJSON = withObject "ProgrammableLogicGlobalParams" $ \obj ->
    ProgrammableLogicGlobalParams
      <$> (obj .: "directory_node_currency_symbol" >>= either fail pure . plutusDataFromJSON)
      <*> (obj .: "programmable_logic_credential" >>= either fail pure . plutusDataFromJSON)

instance ToParamSchema ProgrammableLogicGlobalParams where
  toParamSchema _proxy =
    mempty
      & L.type_ ?~ OpenApiObject
      & L.description ?~ "Global parameters of the programmable token directory"
      & L.properties .~
        [ ( "directory_node_currency_symbol"
          , Inline $ mempty
              & L.type_ ?~ OpenApiString
              & L.description ?~ "base16-encoded script payment credential of the programmable logic script"
              & L.example ?~ "0xc0000000000000000000000000000000000000000000000000000000"
          )
        , ( "programmable_logic_credential"
        , Inline $ mempty
              & L.type_ ?~ OpenApiArray
              & L.description ?~ "plutus-data-encoded payment credential of the programmable logic"
              & L.example ?~ toJSON @[Aeson.Value] [toJSON @Int 0, toJSON @[String] ["0x0a0eb28fbaec9e61d20e9fe4c6ac5e5ee4520bb274b1e3292721d26f"]]
          )
        ]

instance ToSchema ProgrammableLogicGlobalParams where
  declareNamedSchema = pure . paramSchemaToNamedSchema defaultSchemaOptions
