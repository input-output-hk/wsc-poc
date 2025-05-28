{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo           #-}
{-# LANGUAGE UndecidableInstances  #-}

module SmartTokens.Contracts.IssuanceCborHex (
  IssuanceCborHex (..),
  PIssuanceCborHex (..),
  mkIssuanceCborHexMinting,
) where

import Generics.SOP qualified as SOP
import GHC.Generics (Generic)
import Plutarch.Core.List
import Plutarch.Core.Utils
import Plutarch.Core.ValidationLogic
import Plutarch.Core.Value
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import PlutusLedgerApi.V3 (BuiltinByteString)
import PlutusTx qualified
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Prelude qualified as PlutusTx
import SmartTokens.Types.Constants (pissuanceCborHexTokenData)

data IssuanceCborHex =
  IssuanceCborHex {
    prefixCborHex :: BuiltinByteString,
    postfixCborHex :: BuiltinByteString
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

instance PlutusTx.ToData IssuanceCborHex where
  toBuiltinData IssuanceCborHex{prefixCborHex, postfixCborHex} = PlutusTx.toBuiltinData [prefixCborHex, postfixCborHex]

instance PlutusTx.FromData IssuanceCborHex where
  fromBuiltinData builtinData = do
    xs <- BI.chooseData builtinData Nothing Nothing (Just $ BI.unsafeDataAsList builtinData) Nothing Nothing
    prefixCborHex_ <- PlutusTx.fromBuiltinData $ BI.head xs
    let tail_ = BI.tail xs
    postfixCborHex_ <- PlutusTx.fromBuiltinData $ BI.head tail_
    PlutusTx.pure PlutusTx.$ IssuanceCborHex prefixCborHex_ postfixCborHex_


data PIssuanceCborHex (s :: S)
  = PIssuanceCborHex
      { pprefixCborHex :: Term s (PAsData PByteString)
      , ppostfixCborHex :: Term s (PAsData PByteString)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PShow, PEq)
  deriving (PlutusType) via (DeriveAsDataRec PIssuanceCborHex)

deriving via DeriveDataPLiftable (PAsData PIssuanceCborHex) IssuanceCborHex
  instance PLiftable PIssuanceCborHex

mkIssuanceCborHexMinting :: ClosedTerm (PAsData PTxOutRef :--> PScriptContext :--> PUnit)
mkIssuanceCborHexMinting = plam $ \oref ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
  PTxInfo {ptxInfo'inputs, ptxInfo'mint} <- pmatch pscriptContext'txInfo
  PMintingScript ownCS <- pmatch pscriptContext'scriptInfo

  mintedValue <- plet $ pfromData ptxInfo'mint
  let ownTkPairs = ptryLookupValue # ownCS # mintedValue

  -- Enforce that only a single token name is minted for this policy
  ownTkPair <- plet (pheadSingleton # ownTkPairs)
  ownTokenName <- plet (pfstBuiltin # ownTkPair)
  ownNumMinted <- plet (psndBuiltin # ownTkPair)
  pvalidateConditions
    [ ownTokenName #== pissuanceCborHexTokenData
    , ownNumMinted #== pconstant 1
    , phasUTxO # pfromData oref # pfromData ptxInfo'inputs
    ]
