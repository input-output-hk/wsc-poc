{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QualifiedDo           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module SmartTokens.Types.PTokenDirectory (
  DirectorySetNode (..),
  PDirectorySetNode (..),
  isHeadNode,
  isTailNode,
  pemptyBSData,
  pemptyCSData,
  pmkDirectorySetNode,
  pisInsertedOnNode,
  pisInsertedNode,
) where

import SmartTokens.Core.PlutusDataList
    ( DerivePConstantViaDataList(..),
      PlutusTypeDataList,
      ProductIsData(..) )
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 ( PCredential, PCurrencySymbol )
import Plutarch.Prelude
    ( Generic,
      (#),
      phoistAcyclic,
      type (:-->),
      ClosedTerm,
      S,
      Term,
      plam,
      DerivePlutusType(..),
      PlutusType,
      pfield,
      pconstant,
      PBool,
      PEq(..),
      PAsData,
      PIsData,
      PByteString,
      PDataRecord,
      PLabeledType((:=)),
      PShow, PInteger )
import Plutarch.Builtin ( pforgetData, plistData, PBuiltinList (PCons), PData )
import Plutarch.Internal.PlutusType (PInner, PlutusType, pcon, pcon', pmatch, pmatch')
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.DataRepr ( PDataFields )
import PlutusTx qualified
import PlutusLedgerApi.V3 (Credential, CurrencySymbol)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Evaluate (unsafeEvalTerm)
import Plutarch (Config(NoTracing))
import SmartTokens.Core.Utils (pmkBuiltinList)
import Plutarch.List
import Plutarch.Prelude

data BlackListNode = 
  BlackListNode {
    key :: Credential,
    next :: Credential
  } 

newtype PBlacklistNode (s :: S)
  = PBlacklistNode
      ( Term
          s
          ( PDataRecord
              '[ "key" ':= PByteString
               , "next" ':= PByteString
               ]
          )
      )
  deriving stock (Generic)

-- TODO: 
-- The reason we have to manually implement this is because the PlutusTypeDataList DerivePlutusType strategy
-- breaks when we use PByteString fields probably due to the fact that the PLifted/PConstant instances use ByteString 
-- instead of BuiltinByteString. We should fix the PlutusTypeDataList strategy to work with PByteString fields.
instance PlutusType PBlacklistNode where
  type PInner PBlacklistNode = PDataRecord '[ "key" ':= PByteString, "next" ':= PByteString ]
  pcon' (PBlacklistNode t1) = t1
  pmatch' xs f =
    plet (pto xs) $ \innerFieldList ->
      let key = phead # innerFieldList
      in plet (ptail # innerFieldList) $ \remaining ->
          let next = phead # remaining
          in pif (pnull # (ptail # remaining)) (f (PBlacklistNode (pdcons # punsafeCoerce key #$ pdcons # punsafeCoerce next # pdnil))) perror


-- instance DerivePlutusType PBlacklistNode where
--   type DPTStrat _ = PlutusTypeDataList

data DirectorySetNode = DirectorySetNode
  { key :: CurrencySymbol
  , next :: CurrencySymbol
  , transferLogicScript  :: Credential
  , issuerLogicScript :: Credential
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)
  deriving
    (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData) via (ProductIsData DirectorySetNode)

deriving via
  ( DerivePConstantViaDataList
      DirectorySetNode
      PDirectorySetNode
  )
  instance
    (PConstantDecl DirectorySetNode)

-- Optimization:
-- Use the following manual instances instead of the deriving via above if so that we can define key and next fields of type ByteString
-- We should discuss whether we want to prefer newtypes or primitive types for datum / redeemer fields going forward.
-- import Data.ByteString
-- import PlutusTx.Builtins qualified as Builtins
-- import PlutusTx.Builtins.Internal qualified as BI
-- import PlutusTx.Prelude (BuiltinByteString, fromBuiltin, toBuiltin)
-- instance PlutusTx.ToData DirectorySetNode where
--   toBuiltinData DirectorySetNode{key, next, transferLogicScript, issuerLogicScript} = 
--     BI.mkList $ BI.mkCons (BI.mkB $ toBuiltin key) $ BI.mkCons (BI.mkB $ toBuiltin next) $ BI.mkCons (PlutusTx.toBuiltinData transferLogicScript) $ BI.mkCons (PlutusTx.toBuiltinData issuerLogicScript) $ BI.mkNilData BI.unitval

-- instance PlutusTx.FromData DirectorySetNode where
--   fromBuiltinData builtinData = 
--     let fields = BI.snd $ BI.unsafeDataAsConstr builtinData
--         key = BI.head fields 
--         fields1 = BI.tail fields
--         next = BI.head fields1
--         fields2 = BI.tail fields1
--         transferLogicScript = PlutusTx.unsafeFromBuiltinData $ BI.head fields2
--         fields3 = BI.tail fields2
--         issuerLogicScript = PlutusTx.unsafeFromBuiltinData $ BI.head fields3
--      in Just $ DirectorySetNode (fromBuiltin $ BI.unsafeDataAsB key) (fromBuiltin $ BI.unsafeDataAsB next) transferLogicScript issuerLogicScript


newtype PDirectorySetNode (s :: S)
  = PDirectorySetNode
      ( Term
          s
          ( PDataRecord
              '[ "key" ':= PCurrencySymbol
               , "next" ':= PCurrencySymbol
               , "transferLogicScript" ':= PCredential
               , "issuerLogicScript" ':= PCredential
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow, PDataFields)

instance DerivePlutusType PDirectorySetNode where
  type DPTStrat _ = PlutusTypeDataList

instance PUnsafeLiftDecl PDirectorySetNode where
  type PLifted PDirectorySetNode = DirectorySetNode

isHeadNode :: ClosedTerm (PAsData PDirectorySetNode :--> PBool)
isHeadNode = plam $ \node ->
  pfield @"key" # node #== pemptyCSData

isTailNode :: ClosedTerm (PAsData PDirectorySetNode :--> PBool)
isTailNode = plam $ \node ->
  pfield @"next" # node #== pemptyCSData

pemptyBSData :: ClosedTerm (PAsData PByteString)
pemptyBSData = unsafeEvalTerm NoTracing (punsafeCoerce (pconstant $ PlutusTx.B ""))

pemptyCSData :: ClosedTerm (PAsData PCurrencySymbol)
pemptyCSData = unsafeEvalTerm NoTracing (punsafeCoerce (pconstant $ PlutusTx.B ""))


pmkDirectorySetNode :: ClosedTerm (PAsData PByteString :--> PAsData PByteString :--> PAsData PCredential :--> PAsData PCredential :--> PAsData PDirectorySetNode)
pmkDirectorySetNode = phoistAcyclic $
  plam $ \key next transferLogicCred issuerLogicCred ->
    punsafeCoerce $ plistData # pmkBuiltinList [pforgetData key, pforgetData next, pforgetData transferLogicCred, pforgetData issuerLogicCred]

pisInsertedOnNode :: ClosedTerm (PAsData PByteString :--> PAsData PByteString :--> PAsData PCredential :--> PAsData PCredential :--> PAsData PDirectorySetNode :--> PBool)
pisInsertedOnNode = phoistAcyclic $
  plam $ \insertedKey coveringKey transferLogicCred issuerLogicCred outputNode ->
    let expectedDirectoryNode = pmkDirectorySetNode # coveringKey # insertedKey # transferLogicCred # issuerLogicCred
     in outputNode #== expectedDirectoryNode

pisInsertedNode :: ClosedTerm (PAsData PByteString :--> PAsData PByteString :--> PAsData PCredential :--> PAsData PCredential :--> PAsData PDirectorySetNode :--> PBool)
pisInsertedNode = phoistAcyclic $
  plam $ \insertedKey coveringNext transferLogicCred issuerLogicCred outputNode ->
    let expectedDirectoryNode = pmkDirectorySetNode # insertedKey # coveringNext # transferLogicCred # issuerLogicCred
     in outputNode #== expectedDirectoryNode
