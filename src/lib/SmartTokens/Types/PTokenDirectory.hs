{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
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
  pisEmptyNode,
  BlacklistNode(..),
) where

import Generics.SOP qualified as SOP
import GHC.Generics (Generic)
import Plutarch.Core.List
import Plutarch.Evaluate (unsafeEvalTerm)
import Plutarch.Internal.Lift
import Plutarch.Internal.Term (Config (NoTracing))
import Plutarch.LedgerApi.V3 (PCredential, PCurrencySymbol)
import Plutarch.Prelude
import Plutarch.Repr.Data
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V3 (BuiltinByteString, Credential, CurrencySymbol)
import PlutusTx (Data (B, Constr))
import PlutusTx qualified
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Prelude qualified as PlutusTx

-- $setup
--
-- >>> import GHC.Stack (HasCallStack)
-- >>> import Plutarch.Internal.Other (printScript)
-- >>> import qualified Data.Text as T
-- >>> import qualified Plutarch.Internal as PI
-- >>> let printTerm :: HasCallStack => Config -> ClosedTerm a -> String ; printTerm config term = printScript $ either (error . T.unpack) id $ PI.compile config term
--
-- Setup for doctest / HLS. Everything in this block is available for each test. Function
-- definitions are a bit annoying but we probably should introduce printTerm somewhere anyway and
-- then just import.
--
-- Note: Dont put comments over the setup line!
--

data BlacklistNode =
  BlacklistNode {
    blnKey :: BuiltinByteString,
    blnNext :: BuiltinByteString
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)
PlutusTx.makeIsDataIndexed ''BlacklistNode [('BlacklistNode, 0)]

-- instance PlutusTx.ToData BlacklistNode where
--   toBuiltinData BlacklistNode{blnKey, blnNext} =
--     let blnKeyBstr = head $ snd $ BI.unsafeDataAsConstr (toBuiltinData blnKey)
--         blnNextBstr = head $ snd $ BI.unsafeDataAsConstr (toBuiltinData blnNext)
--      in BI.mkList [blnKeyBstr,  blnNextBstr]
--
-- instance PlutusTx.FromData BlacklistNode where
--   fromBuiltinData builtinData =
--     let fields = BI.unsafeDataAsList builtinData
--         key = head fields
--         fields1 = tail fields
--         next = head fields1
--      in Just $ undefined -- Don't know how to determine whether credential is pub key or script

{-
>>> _printTerm $ unsafeEvalTerm NoTracing (mkRecordConstr PBlacklistNode (#blnKey .= pconstant "ffffffffffffffffffffffffffffffffffffffffffffffffffffffff" .& #blnNext .=  pconstant ""))
No instance for `IsString (PAsDataLifted PByteString)'
  arising from the literal `"ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'
In the first argument of `pconstant', namely
  `"ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'
In the second argument of `(.=)', namely
  `pconstant
     "ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'
In the first argument of `(.&)', namely
  `#blnKey
     .=
       pconstant
         "ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'
-}
data PBlacklistNode (s :: S)
  = PBlacklistNode
      { pblnKey :: Term s (PAsData PByteString)
      , pblnNext :: Term s (PAsData PByteString)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PShow, PEq)
  deriving (PlutusType) via (DeriveAsDataStruct PBlacklistNode)

deriving via DeriveDataPLiftable PBlacklistNode BlacklistNode
  instance PLiftable PBlacklistNode

-- _printTerm (communicated by Philip) just print some term as string. The term we want to print is
-- @
-- _term :: forall {s :: S}. Term s PBlacklistNode
-- _term = unsafeEvalTerm NoTracing (pconstant $ BlackListNode { key = "a", next = "b" })
-- @
-- Below, we inline the term and have it in a code lens. You can even run the code lens via Haskell
-- language server. The lens will then replace the string starting with "program ..." with exactly
-- the same string.
--
-- >>> printTerm NoTracing (pconstant $ BlacklistNode { blnKey = "a hi", blnNext = "a" })
-- "program 1.0.0 (List [B #61206869, B #61])"

data DirectorySetNode = DirectorySetNode
  { key :: CurrencySymbol
  , next :: CurrencySymbol
  , transferLogicScript  :: Credential
  , issuerLogicScript :: Credential
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

instance PlutusTx.FromData DirectorySetNode where
  fromBuiltinData dat = do
    xs <- BI.chooseData dat Nothing Nothing (Just $ BI.unsafeDataAsList dat) Nothing Nothing
    directoryNodeCurrSymb <- PlutusTx.fromBuiltinData $ BI.head xs
    let tailCurrSymb = BI.tail xs
    nextNodeCurrSymb <- PlutusTx.fromBuiltinData $ BI.head tailCurrSymb
    let tailCurrSymb1 = BI.tail tailCurrSymb
    transferLogicCred <- PlutusTx.fromBuiltinData $ BI.head tailCurrSymb1
    issuerLogicCred <- PlutusTx.fromBuiltinData $ BI.head $ BI.tail tailCurrSymb1
    PlutusTx.pure PlutusTx.$ DirectorySetNode directoryNodeCurrSymb nextNodeCurrSymb transferLogicCred issuerLogicCred

instance PlutusTx.ToData DirectorySetNode where
  toBuiltinData DirectorySetNode{..} =
    let directoryNodeCS' = PlutusTx.toBuiltinData key
        nextNodeCS' = PlutusTx.toBuiltinData next
        transferLogicCred' = PlutusTx.toBuiltinData transferLogicScript
        issuerLogicCred' = PlutusTx.toBuiltinData issuerLogicScript
     in BI.mkList $ BI.mkCons directoryNodeCS' (BI.mkCons nextNodeCS' (BI.mkCons transferLogicCred' (BI.mkCons issuerLogicCred' $ BI.mkNilData BI.unitval)))

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

data PDirectorySetNode (s :: S)
  = PDirectorySetNode
      { pkey :: Term s (PAsData PCurrencySymbol)
      , pnext :: Term s (PAsData PCurrencySymbol)
      , ptransferLogicScript :: Term s (PAsData PCredential)
      , pissuerLogicScript :: Term s (PAsData PCredential)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataRec PDirectorySetNode)

deriving via DeriveDataPLiftable (PAsData PDirectorySetNode) DirectorySetNode
  instance PLiftable PDirectorySetNode

isHeadNode :: Term s (PAsData PDirectorySetNode) -> Term s PBool
isHeadNode node =
  pmatch (pfromData node) $ \node' ->
    pkey node' #== pemptyCSData

isTailNode :: Term s (PAsData PDirectorySetNode) -> Term s PBool
isTailNode node =
  pmatch (pfromData node) $ \node' ->
    pnext node' #== ptailNextData

{-|

>>> _printTerm $ unsafeEvalTerm NoTracing emptyNode
"program\n  1.0.0\n  (List\n     [ B #\n     , B #ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff\n     , Constr 0 [B #]\n     , Constr 0 [B #] ])"
-}
emptyNode :: ClosedTerm (PAsData PDirectorySetNode)
emptyNode =
  let nullTransferLogicCred = pconstant (Constr 0 [PlutusTx.B ""])
      nullIssuerLogicCred = pconstant (Constr 0 [PlutusTx.B ""])
  in punsafeCoerce $ plistData # pmkBuiltinList [pforgetData pemptyBSData, pforgetData ptailNextData, nullTransferLogicCred, nullIssuerLogicCred]

pisEmptyNode :: Term s (PAsData PDirectorySetNode) -> Term s PBool
pisEmptyNode node =
  node #== emptyNode

pemptyBSData :: ClosedTerm (PAsData PByteString)
pemptyBSData = unsafeEvalTerm NoTracing (punsafeCoerce (pconstant @PData $ PlutusTx.B ""))

pemptyCSData :: ClosedTerm (PAsData PCurrencySymbol)
pemptyCSData = unsafeEvalTerm NoTracing (punsafeCoerce (pconstant @PData $ PlutusTx.B ""))

ptailNextData  :: ClosedTerm (PAsData PCurrencySymbol)
ptailNextData = unsafeEvalTerm NoTracing (punsafeCoerce $ pdata (phexByteStr "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))

pmkDirectorySetNode :: ClosedTerm (PAsData PByteString :--> PAsData PByteString :--> PAsData PCredential :--> PAsData PCredential :--> PAsData PDirectorySetNode)
pmkDirectorySetNode = phoistAcyclic $
  plam $ \key_ next_ transferLogicCred issuerLogicCred ->
    punsafeCoerce $ plistData # pmkBuiltinList [pforgetData key_, pforgetData next_, pforgetData transferLogicCred, pforgetData issuerLogicCred]

pisInsertedOnNode :: ClosedTerm (PAsData PByteString :--> PAsData PByteString :--> PAsData PCredential :--> PAsData PCredential :--> PAsData PDirectorySetNode :--> PBool)
pisInsertedOnNode = phoistAcyclic $
  plam $ \insertedKey coveringKey transferLogicCred issuerLogicCred outputNode ->
    let expectedDirectoryNode = pmkDirectorySetNode # coveringKey # insertedKey # transferLogicCred # issuerLogicCred
     in outputNode #== expectedDirectoryNode

pisInsertedNode :: ClosedTerm (PAsData PByteString :--> PAsData PByteString :--> PAsData PDirectorySetNode :--> PBool)
pisInsertedNode = phoistAcyclic $
  plam $ \insertedKey coveringNext outputNode ->
    pmatch (pfromData outputNode) $ \(PDirectorySetNode {ptransferLogicScript, pissuerLogicScript}) ->
      let transferLogicCred_ = ptraceInfoShowId ptransferLogicScript
          issuerLogicCred_ = ptraceInfoShowId pissuerLogicScript
          expectedDirectoryNode =
            pmkDirectorySetNode # insertedKey # coveringNext # pdeserializeDirectoryCredential transferLogicCred_ # pdeserializeDirectoryCredential issuerLogicCred_
      in outputNode #== expectedDirectoryNode

pdeserializeDirectoryCredential :: Term s (PAsData PCredential) -> Term s (PAsData PCredential)
pdeserializeDirectoryCredential term =
  plet (pasConstr # pforgetData term) $ \constrPair ->
    plet (pfstBuiltin # constrPair) $ \constrIdx ->
      pif (plengthBS # (pasByteStr # (pheadSingleton # (psndBuiltin # constrPair))) #<= 28)
          (
            pcond
              [ ( constrIdx #== 0 , term)
              , ( constrIdx #== 1 , term)
              ]
              (ptraceInfoError "Invalid credential")
          )
          (ptraceInfoError $ pconstant "Invalid credential len" <> pshow (plengthBS # (pasByteStr # (pheadSingleton # (psndBuiltin # constrPair)))))
