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
  BlacklistNode (..),
  isHeadNode,
  isTailNode,
  pemptyBSData,
  pemptyCSData,
  pmkDirectorySetNode,
  pisInsertedOnNode,
  pisInsertedNode,
  pisEmptyNode,
  pmkBlacklistNode,
  pisInsertedOnBlacklistNode,
  pisInsertedBlacklistNode,
  pisEmptyBlacklistNode,
  pisBlacklistTailNode,
  pemptyBlacklistNode,
) where

import Generics.SOP qualified as SOP
import GHC.Generics (Generic)
import Plutarch.Core.List
import Plutarch.Evaluate (unsafeEvalTerm)
import Plutarch.Internal.Lift
import Plutarch.Internal.Term (Config (NoTracing))
import Plutarch.LedgerApi.V3 (PCredential, PCurrencySymbol)
import Plutarch.Prelude
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

instance PlutusTx.ToData BlacklistNode where
  toBuiltinData BlacklistNode{blnKey, blnNext} = PlutusTx.toBuiltinData [blnKey, blnNext]

instance PlutusTx.FromData BlacklistNode where
  fromBuiltinData builtinData = do
    xs <- BI.chooseData builtinData Nothing Nothing (Just $ BI.unsafeDataAsList builtinData) Nothing Nothing
    blnKey_ <- PlutusTx.fromBuiltinData $ BI.head xs
    let tail_ = BI.tail xs
    blnNext_ <- PlutusTx.fromBuiltinData $ BI.head tail_
    PlutusTx.pure PlutusTx.$ BlacklistNode blnKey_ blnNext_

{-
>>> _printTerm $ unsafeEvalTerm NoTracing (pconstant (PBlacklistNode {blnKey = pconstant @(PAsData PByteString) "ffffffffffffffffffffffffffffffffffffffffffffffffffffffff", blnNext =  pconstant (PAsData PByteString) ""}))
--
-}
data PBlacklistNode (s :: S)
  = PBlacklistNode
      { pblnKey :: Term s (PAsData PByteString)
      , pblnNext :: Term s (PAsData PByteString)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PShow, PEq)
  deriving (PlutusType) via (DeriveAsDataRec PBlacklistNode)

deriving via DeriveDataPLiftable (PAsData PBlacklistNode) BlacklistNode
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

pmkBlacklistNode :: ClosedTerm (PAsData PByteString :--> PAsData PByteString :--> PAsData PBlacklistNode)
pmkBlacklistNode = phoistAcyclic $
  plam $ \key_ next_ ->
    punsafeCoerce $ plistData # pmkBuiltinList [pforgetData key_, pforgetData next_]

pisInsertedOnBlacklistNode :: ClosedTerm (PAsData PByteString :--> PAsData PByteString :--> PAsData PBlacklistNode :--> PBool)
pisInsertedOnBlacklistNode = phoistAcyclic $
  plam $ \insertedKey coveringKey outputNode ->
    let expectedDirectoryNode = pmkBlacklistNode # coveringKey # insertedKey
     in outputNode #== expectedDirectoryNode

pisInsertedBlacklistNode :: ClosedTerm (PAsData PByteString :--> PAsData PByteString :--> PAsData PBlacklistNode :--> PBool)
pisInsertedBlacklistNode = phoistAcyclic $
  plam $ \insertedKey coveringNext outputNode ->
    let expectedDirectoryNode = pmkBlacklistNode # insertedKey # coveringNext
    in outputNode #== expectedDirectoryNode

pemptyBlacklistNode :: ClosedTerm (PAsData PBlacklistNode)
pemptyBlacklistNode = punsafeCoerce $ plistData # pmkBuiltinList [pforgetData pemptyBSData, pforgetData ptailBlackListNext]

ptailBlackListNext  :: ClosedTerm (PAsData PByteString)
ptailBlackListNext = unsafeEvalTerm NoTracing (punsafeCoerce $ pdata (phexByteStr "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))

pisBlacklistTailNode :: Term s (PAsData PBlacklistNode) -> Term s PBool
pisBlacklistTailNode node =
  pmatch (pfromData node) $ \(PBlacklistNode {pblnNext}) ->
    pblnNext #== ptailBlackListNext

pisEmptyBlacklistNode :: Term s (PAsData PBlacklistNode) -> Term s PBool
pisEmptyBlacklistNode node =
  node #== pemptyBlacklistNode

data DirectorySetNode = DirectorySetNode
  { key :: CurrencySymbol
  , next :: CurrencySymbol
  , transferLogicScript  :: Credential
  , issuerLogicScript :: Credential
  , globalStateCS :: CurrencySymbol
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
    let tailTransferLogic = BI.tail tailCurrSymb1
    issuerLogicCred <- PlutusTx.fromBuiltinData $ BI.head $ tailTransferLogic
    globalStateCS' <- PlutusTx.fromBuiltinData $ BI.head $ BI.tail $ tailTransferLogic
    PlutusTx.pure PlutusTx.$ DirectorySetNode directoryNodeCurrSymb nextNodeCurrSymb transferLogicCred issuerLogicCred globalStateCS'

instance PlutusTx.ToData DirectorySetNode where
  toBuiltinData DirectorySetNode{..} =
    let directoryNodeCS' = PlutusTx.toBuiltinData key
        nextNodeCS' = PlutusTx.toBuiltinData next
        transferLogicCred' = PlutusTx.toBuiltinData transferLogicScript
        issuerLogicCred' = PlutusTx.toBuiltinData issuerLogicScript
        globalStateCS' = PlutusTx.toBuiltinData globalStateCS
     in BI.mkList (BI.mkCons directoryNodeCS' (BI.mkCons nextNodeCS' (BI.mkCons transferLogicCred' (BI.mkCons issuerLogicCred' (BI.mkCons globalStateCS' $ BI.mkNilData BI.unitval)))))

data PDirectorySetNode (s :: S)
  = PDirectorySetNode
      { pkey :: Term s (PAsData PCurrencySymbol)
      , pnext :: Term s (PAsData PCurrencySymbol)
      , ptransferLogicScript :: Term s (PAsData PCredential)
      , pissuerLogicScript :: Term s (PAsData PCredential)
      , pglobalStateCS :: Term s (PAsData PCurrencySymbol)
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
  in punsafeCoerce $ plistData # pmkBuiltinList [pforgetData pemptyBSData, pforgetData ptailNextData, nullTransferLogicCred, nullIssuerLogicCred, pforgetData pemptyBSData]

pisEmptyNode :: Term s (PAsData PDirectorySetNode) -> Term s PBool
pisEmptyNode node =
  node #== emptyNode

pemptyBSData :: ClosedTerm (PAsData PByteString)
pemptyBSData = unsafeEvalTerm NoTracing (punsafeCoerce (pconstant @PData $ PlutusTx.B ""))

pemptyCSData :: ClosedTerm (PAsData PCurrencySymbol)
pemptyCSData = unsafeEvalTerm NoTracing (punsafeCoerce (pconstant @PData $ PlutusTx.B ""))

ptailNextData  :: ClosedTerm (PAsData PCurrencySymbol)
ptailNextData = unsafeEvalTerm NoTracing (punsafeCoerce $ pdata (phexByteStr "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))

pmkDirectorySetNode :: ClosedTerm (PAsData PByteString :--> PAsData PByteString :--> PAsData PCredential :--> PAsData PCredential :--> PAsData PCurrencySymbol :--> PAsData PDirectorySetNode)
pmkDirectorySetNode = phoistAcyclic $
  plam $ \key_ next_ transferLogicCred issuerLogicCred globalStateCS_ ->
    punsafeCoerce $ plistData # pmkBuiltinList [pforgetData key_, pforgetData next_, pforgetData transferLogicCred, pforgetData issuerLogicCred, pforgetData globalStateCS_]

pisInsertedOnNode :: ClosedTerm (PAsData PByteString :--> PAsData PByteString :--> PAsData PCredential :--> PAsData PCredential :--> PAsData PCurrencySymbol :--> PAsData PDirectorySetNode :--> PBool)
pisInsertedOnNode = phoistAcyclic $
  plam $ \insertedKey coveringKey transferLogicCred issuerLogicCred globalCS outputNode ->
    let expectedDirectoryNode = pmkDirectorySetNode # coveringKey # insertedKey # transferLogicCred # issuerLogicCred # globalCS
     in outputNode #== expectedDirectoryNode

pisInsertedNode :: ClosedTerm (PAsData PByteString :--> PAsData PByteString :--> PAsData PDirectorySetNode :--> PBool)
pisInsertedNode = phoistAcyclic $
  plam $ \insertedKey coveringNext outputNode ->
    pmatch (pfromData outputNode) $ \(PDirectorySetNode {ptransferLogicScript, pissuerLogicScript, pglobalStateCS}) ->
      let transferLogicCred_ = ptraceInfoShowId ptransferLogicScript
          issuerLogicCred_ = ptraceInfoShowId pissuerLogicScript
          expectedDirectoryNode =
            pmkDirectorySetNode # insertedKey # coveringNext # pdeserializeDirectoryCredential transferLogicCred_ # pdeserializeDirectoryCredential issuerLogicCred_ # pdeserializeCurrencySymbol pglobalStateCS
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

pdeserializeCurrencySymbol :: Term s (PAsData PCurrencySymbol) -> Term s (PAsData PCurrencySymbol)
pdeserializeCurrencySymbol term =
  plet (pasByteStr # pforgetData term) $ \bstr ->
    pif (plengthBS # bstr #<= 28)
        term
        (ptraceInfoError $ pconstant "Invalid CurrencySymbol len" <> pshow (plengthBS # bstr))
