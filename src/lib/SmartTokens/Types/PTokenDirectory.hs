{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo           #-}
{-# LANGUAGE UndecidableInstances  #-}

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
  BlacklistNode(..),
) where

import Generics.SOP qualified as SOP
import Plutarch ( Config(NoTracing), Config(NoTracing) )
import Plutarch.Builtin
    ( pasByteStr,
      pasConstr,
      pasList,
      pforgetData,
      plistData,
      pforgetData,
      plistData )
import Plutarch.Core.PlutusDataList (DerivePConstantViaDataList (..),
                                     PlutusTypeDataList, ProductIsData (..))
import Plutarch.Core.Utils (pcond, pheadSingleton, pmkBuiltinList)
import Plutarch.DataRepr (PDataFields)
import Plutarch.DataRepr.Internal.Field (HRec (..), Labeled (Labeled))
import Plutarch.Evaluate (unsafeEvalTerm)
import Plutarch.LedgerApi.V3 (PCredential, PCurrencySymbol)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.List
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V3
    ( Credential, CurrencySymbol, BuiltinByteString )
import PlutusTx (Data (B, Constr))
import PlutusTx qualified
import Plutarch.DataRepr.Internal
import GHC.Stack (HasCallStack)
import Plutarch.Internal.Other (printScript)
import qualified Data.Text as T
import qualified Plutarch.Internal as PI



data BlacklistNode =
  BlacklistNode {
    blnKey :: BuiltinByteString,
    blnNext :: BuiltinByteString
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)
  deriving
    (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData) via (ProductIsData BlacklistNode)

deriving via (DerivePConstantViaData BlacklistNode PBlacklistNode)
  instance (PConstantDecl BlacklistNode)

newtype PBlacklistNode (s :: S)
  = PBlacklistNode
      ( Term
          s
          ( PDataRecord
              '[ "blnKey" ':= PByteString
               , "blnNext" ':= PByteString
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields, PIsData)

instance DerivePlutusType PBlacklistNode where
  type DPTStrat PBlacklistNode = PlutusTypeData

instance PUnsafeLiftDecl PBlacklistNode where
  type PLifted PBlacklistNode = BlacklistNode



-- _printTerm (communicated by Philip) just print some term as string. The term we want to print is
-- @
-- _term :: forall {s :: S}. Term s PBlacklistNode
-- _term = unsafeEvalTerm NoTracing (pconstant $ BlackListNode { key = "a", next = "b" })
-- @
-- Below, we inline the term and have it in a code lens. You can even run the code lens via Haskell
-- language server. The lens will then replace the string starting with "program ..." with exactly
-- the same string.
--
-- >>>  _printTerm NoTracing $ unsafeEvalTerm NoTracing (pconstant $ BlacklistNode { blnKey = "a hi", blnNext = "a" })
-- "program 1.0.0 (List [B #61206869, B #61])"
_printTerm :: HasCallStack => Config -> ClosedTerm a -> String
_printTerm config term = printScript $ either (error . T.unpack) id $ PI.compile config term



type PBlacklistNodeHRec (s :: S) =
  HRec
    '[ '("key", Term s (PAsData PByteString))
     , '("next", Term s (PAsData PByteString))
     ]

pletFieldsBlacklistNode :: forall {s :: S} {r :: PType}. Term s (PAsData PBlacklistNode) -> (PBlacklistNodeHRec s -> Term s r) -> Term s r
pletFieldsBlacklistNode term = runTermCont $ do
  fields <- tcont $ plet $ pasList # (pforgetData term)
  let key_ = punsafeCoerce @_ @_ @(PAsData PByteString) $ phead # fields
      next_ = punsafeCoerce @_ @_ @(PAsData PByteString) $ pheadSingleton # (ptail # fields)
  tcont $ \f -> f $ HCons (Labeled @"key" key_) (HCons (Labeled @"next" next_) HNil)

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
      let transferLogicCred_ = outputNodeDatumF.transferLogicScript
          issuerLogicCred_ = outputNodeDatumF.issuerLogicScript
          expectedDirectoryNode =
            pmkDirectorySetNode # insertedKey # coveringNext # pdeserializeCredential transferLogicCred_ # pdeserializeCredential issuerLogicCred_
      in outputNode #== expectedDirectoryNode

pdeserializeCredential :: Term s (PAsData PCredential) -> Term s (PAsData PCredential)
pdeserializeCredential term =
  plet (pasConstr # pforgetData term) $ \constrPair ->
    plet (pfstBuiltin # constrPair) $ \constrIdx ->
      pif (plengthBS # (pasByteStr # (pheadSingleton # (psndBuiltin # constrPair))) #== 28)
          (
            pcond
              [ ( constrIdx #== 0 , term)
              , ( constrIdx #== 1 , term)
              ]
              perror
          )
          perror
