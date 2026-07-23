{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QualifiedDo           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module SmartTokens.Types.ProtocolParams (
  ProgrammableLogicGlobalParams (..),
  PProgrammableLogicGlobalParams (..),
) where

import Cardano.Api.Plutus qualified as C
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
import PlutusLedgerApi.V3 (Credential, CurrencySymbol)
import PlutusTx qualified
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Prelude qualified as PlutusTxPrelude

-- | Protocol-wide parameters stored in the NFT-authenticated protocol-params
-- datum (spec §4, doc/design-issuance-dual-arm-custody.md).
--
-- Field order is NORMATIVE and consensus-critical (the issuance policy
-- raw-accesses these positions without shape validation, §7):
--
--   0. 'directoryNodeCS'  — directory-node NFT currency symbol (unchanged)
--   1. 'progLogicCred'    — programmable-logic base spending credential (unchanged)
--   2. 'globalLogicCred'  — global transfer rewarding credential (NEW)
--   3. 'seizeLogicCred'   — standalone seize rewarding credential (NEW)
--
-- Fields 0/1 are byte-compatible with the previous 2-field datum, so every
-- existing consumer that reads only those positions is unaffected. Fields 2/3
-- are appended and MUST NOT be forced by hot paths that do not need them
-- (see the raw-field readers in "SmartTokens.Contracts.ProgrammableLogicBase").
-- Credentials are stored Data-encoded (not @ScriptHash@) so consumers compare
-- them with a direct @equalsData@ (601 mem) rather than a byte-decomposed
-- compare (2129 mem); the "key credential in a script slot" misconfiguration is
-- made unrepresentable at mint time by the hardened anchor policy (§4.1), not by
-- the type.
data ProgrammableLogicGlobalParams = ProgrammableLogicGlobalParams
  { directoryNodeCS :: CurrencySymbol
  , progLogicCred :: Credential
  , globalLogicCred :: Credential
  , seizeLogicCred :: Credential
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

instance PlutusTx.FromData ProgrammableLogicGlobalParams where
  fromBuiltinData dat = do
    xs <- BI.chooseData dat Nothing Nothing (Just $ BI.unsafeDataAsList dat) Nothing Nothing
    directoryNodeCurrSymb <- PlutusTx.fromBuiltinData $ BI.head xs
    let xs1 = BI.tail xs
    progLogicCred <- PlutusTx.fromBuiltinData $ BI.head xs1
    let xs2 = BI.tail xs1
    globalLogicCred <- PlutusTx.fromBuiltinData $ BI.head xs2
    seizeLogicCred <- PlutusTx.fromBuiltinData $ BI.head $ BI.tail xs2
    PlutusTxPrelude.pure PlutusTxPrelude.$
      ProgrammableLogicGlobalParams directoryNodeCurrSymb progLogicCred globalLogicCred seizeLogicCred

instance PlutusTx.ToData ProgrammableLogicGlobalParams where
  toBuiltinData ProgrammableLogicGlobalParams{directoryNodeCS, progLogicCred, globalLogicCred, seizeLogicCred} =
    let directoryNodeCS' = PlutusTx.toBuiltinData directoryNodeCS
        progLogicCred' = PlutusTx.toBuiltinData progLogicCred
        globalLogicCred' = PlutusTx.toBuiltinData globalLogicCred
        seizeLogicCred' = PlutusTx.toBuiltinData seizeLogicCred
     in BI.mkList $
          BI.mkCons directoryNodeCS' $
            BI.mkCons progLogicCred' $
              BI.mkCons globalLogicCred' $
                BI.mkCons seizeLogicCred' $
                  BI.mkNilData BI.unitval

data PProgrammableLogicGlobalParams (s :: S)
  = PProgrammableLogicGlobalParams
      { pdirectoryNodeCS :: Term s (PAsData PCurrencySymbol)
      , pprogLogicCred :: Term s (PAsData PCredential)
      , pglobalLogicCred :: Term s (PAsData PCredential)
      , pseizeLogicCred :: Term s (PAsData PCredential)
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
  toJSON ProgrammableLogicGlobalParams{directoryNodeCS, progLogicCred, globalLogicCred, seizeLogicCred} =
    object
      [ "directory_node_currency_symbol" .= plutusDataToJSON directoryNodeCS
      , "programmable_logic_credential"  .= plutusDataToJSON progLogicCred
      , "global_logic_credential"        .= plutusDataToJSON globalLogicCred
      , "seize_logic_credential"         .= plutusDataToJSON seizeLogicCred
      ]

instance FromJSON ProgrammableLogicGlobalParams where
  parseJSON = withObject "ProgrammableLogicGlobalParams" $ \obj ->
    ProgrammableLogicGlobalParams
      <$> (obj .: "directory_node_currency_symbol" >>= either fail pure . plutusDataFromJSON)
      <*> (obj .: "programmable_logic_credential" >>= either fail pure . plutusDataFromJSON)
      <*> (obj .: "global_logic_credential" >>= either fail pure . plutusDataFromJSON)
      <*> (obj .: "seize_logic_credential" >>= either fail pure . plutusDataFromJSON)

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
        , ( "global_logic_credential"
        , Inline $ mempty
              & L.type_ ?~ OpenApiArray
              & L.description ?~ "plutus-data-encoded rewarding credential of the global transfer validator"
              & L.example ?~ toJSON @[Aeson.Value] [toJSON @Int 0, toJSON @[String] ["0x0a0eb28fbaec9e61d20e9fe4c6ac5e5ee4520bb274b1e3292721d26f"]]
          )
        , ( "seize_logic_credential"
        , Inline $ mempty
              & L.type_ ?~ OpenApiArray
              & L.description ?~ "plutus-data-encoded rewarding credential of the standalone seize validator"
              & L.example ?~ toJSON @[Aeson.Value] [toJSON @Int 0, toJSON @[String] ["0x0a0eb28fbaec9e61d20e9fe4c6ac5e5ee4520bb274b1e3292721d26f"]]
          )
        ]

instance ToSchema ProgrammableLogicGlobalParams where
  declareNamedSchema = pure . paramSchemaToNamedSchema defaultSchemaOptions
