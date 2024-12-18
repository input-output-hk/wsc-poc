{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QualifiedDo #-}
module SmartTokens.Types.PTokenDirectory (
  DirectorySetNode (..),
  PDirectorySetNode (..),
  PBlacklistNode (..),
  isHeadNode,
  isTailNode,
  pemptyBSData,
  pemptyCSData,
  pmkDirectorySetNode,
  pisInsertedOnNode,
  pisInsertedNode,
  pletFieldsBlacklistNode,
  pisEmptyNode,
) where

import Plutarch.Core.PlutusDataList
    ( DerivePConstantViaDataList(..),
      PlutusTypeDataList,
      ProductIsData(..) )
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 ( PCredential, PCurrencySymbol )
import Plutarch.DataRepr.Internal.Field (HRec (..), Labeled (Labeled))
import Plutarch.Builtin ( pforgetData, plistData, pasList )
import Plutarch.Internal.PlutusType (pcon', pmatch')
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.DataRepr ( PDataFields )
import PlutusTx qualified
import PlutusLedgerApi.V3 (Credential, CurrencySymbol)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Evaluate (unsafeEvalTerm)
import Plutarch (Config(NoTracing))
import Plutarch.Core.Utils (pmkBuiltinList, pheadSingleton, pdeserializeCredential)
import Plutarch.List
import Plutarch.Prelude
import PlutusTx (
  Data (B, Constr),
 )
-- data BlackListNode =
--   BlackListNode {
--     key :: BuiltinByteString,
--     next :: BuiltinByteString
--   }

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
      let key_ = phead # innerFieldList
      in plet (ptail # innerFieldList) $ \remaining ->
          let next_ = phead # remaining
          in pif (pnull # (ptail # remaining)) (f (PBlacklistNode (pdcons # punsafeCoerce key_ #$ pdcons # punsafeCoerce next_ # pdnil))) perror

type PBlacklistNodeHRec (s :: S) =
  HRec
    '[ '("key", Term s (PAsData PByteString))
     , '("next", Term s (PAsData PByteString))
     ]

pletFieldsBlacklistNode :: forall {s :: S} {r :: PType}. Term s (PAsData PBlacklistNode) -> (PBlacklistNodeHRec s -> Term s r) -> Term s r
pletFieldsBlacklistNode term = runTermCont $ do
  fields <- tcont $ plet $ pasList # (pforgetData term)
  let key = punsafeCoerce @_ @_ @(PAsData PByteString) $ phead # fields
      next = punsafeCoerce @_ @_ @(PAsData PByteString) $ pheadSingleton # (ptail # fields)
  tcont $ \f -> f $ HCons (Labeled @"key" key) (HCons (Labeled @"next" next) HNil)

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

pisEmptyNode :: ClosedTerm (PAsData PDirectorySetNode :--> PBool)
pisEmptyNode = plam $ \node ->
  let nullTransferLogicCred = pconstant (Constr 0 [PlutusTx.B ""])
      nullIssuerLogicCred = pconstant (Constr 0 [PlutusTx.B ""])
      expectedEmptyNode = punsafeCoerce $ plistData # pmkBuiltinList [pforgetData pemptyBSData, pforgetData pemptyBSData, nullTransferLogicCred, nullIssuerLogicCred]
  in node #== expectedEmptyNode

pemptyBSData :: ClosedTerm (PAsData PByteString)
pemptyBSData = unsafeEvalTerm NoTracing (punsafeCoerce (pconstant $ PlutusTx.B ""))

pemptyCSData :: ClosedTerm (PAsData PCurrencySymbol)
pemptyCSData = unsafeEvalTerm NoTracing (punsafeCoerce (pconstant $ PlutusTx.B ""))

pmkDirectorySetNode :: ClosedTerm (PAsData PByteString :--> PAsData PByteString :--> PAsData PCredential :--> PAsData PCredential :--> PAsData PDirectorySetNode)
pmkDirectorySetNode = phoistAcyclic $
  plam $ \key_ next_ transferLogicCred issuerLogicCred ->
    punsafeCoerce $ plistData # pmkBuiltinList [pforgetData key_, pforgetData next_, pforgetData transferLogicCred, pforgetData issuerLogicCred]

pisInsertedOnNode :: ClosedTerm (PAsData PByteString :--> PAsData PByteString :--> PAsData PCredential :--> PAsData PCredential :--> PAsData PDirectorySetNode :--> PBool)
pisInsertedOnNode = phoistAcyclic $
  plam $ \insertedKey coveringKey transferLogicCred issuerLogicCred outputNode ->
    let expectedDirectoryNode = pmkDirectorySetNode # coveringKey # insertedKey # transferLogicCred # issuerLogicCred
     in outputNode #== expectedDirectoryNode

-- pisInsertedNode :: ClosedTerm (PAsData PByteString :--> PAsData PByteString :--> PAsData PCredential :--> PAsData PCredential :--> PAsData PDirectorySetNode :--> PBool)
-- pisInsertedNode = phoistAcyclic $
--   plam $ \insertedKey coveringNext transferLogicCred issuerLogicCred outputNode ->
--     let expectedDirectoryNode = pmkDirectorySetNode # insertedKey # coveringNext # transferLogicCred # issuerLogicCred
--      in outputNode #== expectedDirectoryNode

pisInsertedNode :: ClosedTerm (PAsData PByteString :--> PAsData PByteString :--> PAsData PDirectorySetNode :--> PBool)
pisInsertedNode = phoistAcyclic $ 
  plam $ \insertedKey coveringNext outputNode ->
    pletFields @'["transferLogicScript", "issuerLogicScript"] outputNode $ \outputNodeDatumF ->
      let transferLogicCred = outputNodeDatumF.transferLogicScript
          issuerLogicCred = outputNodeDatumF.issuerLogicScript
          expectedDirectoryNode = 
            pmkDirectorySetNode # insertedKey # coveringNext # pdeserializeCredential transferLogicCred # pdeserializeCredential issuerLogicCred
      in outputNode #== expectedDirectoryNode