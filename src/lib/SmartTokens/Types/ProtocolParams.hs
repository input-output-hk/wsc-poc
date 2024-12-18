{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module SmartTokens.Types.ProtocolParams (
  ProgrammableLogicGlobalParams (..),
  PProgrammableLogicGlobalParams (..),
) where

import Plutarch.Core.PlutusDataList
    ( DerivePConstantViaDataList(..),
      PlutusTypeDataList,
      ProductIsData(..) )
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PCredential)
import Plutarch.Prelude
import Plutarch.DataRepr (PDataFields)
import PlutusTx qualified
import PlutusLedgerApi.V3 (Credential, CurrencySymbol)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))

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