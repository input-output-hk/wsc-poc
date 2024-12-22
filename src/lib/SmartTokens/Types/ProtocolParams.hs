{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# LANGUAGE InstanceSigs         #-}

module SmartTokens.Types.ProtocolParams (
  ProgrammableLogicGlobalParams (..),
  PProgrammableLogicGlobalParams (..),
) where

import Cardano.Api.Shelley qualified as C
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (Bifunctor (..))
import Generics.SOP qualified as SOP
import Plutarch.Core.PlutusDataList (DerivePConstantViaDataList (..),
                                     PlutusTypeDataList, ProductIsData (..))
import Plutarch.DataRepr (PDataFields)
import Plutarch.LedgerApi.V3 (PCredential, PCurrencySymbol)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude (DerivePlutusType (..), Generic, PDataRecord, PEq,
                         PIsData, PLabeledType ((:=)), PShow, PlutusType, S,
                         Term)
import PlutusLedgerApi.V3 (Credential, CurrencySymbol)
import PlutusTx qualified

-- TODO:
-- Figure out why deriving PlutusType breaks when I uncomment this
-- and disable no-deferred-type-errors
data ProgrammableLogicGlobalParams = ProgrammableLogicGlobalParams
  { directoryNodeCS :: CurrencySymbol
  , progLogicCred :: Credential
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)
  deriving
    (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData) via (ProductIsData ProgrammableLogicGlobalParams)
  deriving (PConstantDecl) via (DerivePConstantViaDataList ProgrammableLogicGlobalParams PProgrammableLogicGlobalParams)

newtype PProgrammableLogicGlobalParams (s :: S)
  = PProgrammableLogicGlobalParams
      ( Term
          s
          ( PDataRecord
              '[ "directoryNodeCS" ':= PCurrencySymbol
               , "progLogicCred" ':= PCredential
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow, PDataFields)

instance DerivePlutusType PProgrammableLogicGlobalParams where
  type DPTStrat _ = PlutusTypeDataList

instance PUnsafeLiftDecl PProgrammableLogicGlobalParams where
  type PLifted PProgrammableLogicGlobalParams = ProgrammableLogicGlobalParams

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
