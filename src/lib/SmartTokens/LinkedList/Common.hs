{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QualifiedDo           #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module SmartTokens.LinkedList.Common (
  PDirectoryCommon (..),
  makeCommon,
  pInit,
  pInsert,
  nodeInputUtxoDatumUnsafePair,
  parseNodeOutputUtxoPair,
) where

import GHC.Generics (Generic)
import Plutarch.Core.Context
import Plutarch.Core.List
import Plutarch.Core.Utils
import Plutarch.Core.Value
import Plutarch.Internal.Term
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3
import Plutarch.LedgerApi.Value (pnormalize)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import SmartTokens.Types.PTokenDirectory (PDirectorySetNode (..), pisEmptyNode,
                                          pisInsertedNode, pisInsertedOnNode)
import Types.Constants (pnodeKeyTN, poriginNodeTN, pparseNodeKey)

ppaysToAddress :: Term s (PAddress :--> PAsData PTxOut :--> PBool)
ppaysToAddress = phoistAcyclic $ plam $ \adr txOut -> adr #== ptxOutAddress (pfromData txOut)

{- | Ensures that the minted amount of the FinSet CS is exactly the specified
     tokenName and amount
-}
correctNodeTokenMinted ::
  ClosedTerm
    ( PCurrencySymbol
        :--> PTokenName
        :--> PInteger
        :--> PValue 'Sorted 'NonZero
        :--> PBool
    )
correctNodeTokenMinted = phoistAcyclic $
  plam $ \nodeCS tokenName amount mint -> P.do
    PJust nodeMint <- pmatch $ AssocMap.plookup # nodeCS # pto mint
    let tokenMap = AssocMap.psingleton # tokenName # amount
    tokenMap #== nodeMint

-- Potentially use this in the future if we plan to manage additional
-- value in the directory nodes.
nodeInputUtxoDatumUnsafePair ::
  ClosedTerm
    ( PAsData PTxOut
        :--> PPair (PValue 'Sorted 'Positive) (PAsData PDirectorySetNode)
    )
nodeInputUtxoDatumUnsafePair = phoistAcyclic $ plam $ \out ->
  pmatch (pfromData out) $ \(PTxOut {ptxOut'value, ptxOut'datum}) ->
    pmatch ptxOut'datum $ \case
      POutputDatum d ->
        plet (punsafeCoerce (pto d)) $ \nodeDat ->
          pcon (PPair (pfromData ptxOut'value) nodeDat)
      _ -> ptraceInfoError "Expected output datum"

nodeInputUtxoDatumUnsafe
  :: ClosedTerm (PAsData PTxOut :--> PAsData PDirectorySetNode)
nodeInputUtxoDatumUnsafe = phoistAcyclic $ plam $ \txOut ->
  punsafeCoerce (ptxOutInlineDatumRaw $ pfromData txOut)

parseNodeOutputUtxo ::
  ClosedTerm
    ( PAsData PCurrencySymbol
        :--> PAsData PTxOut
        :--> PAsData PDirectorySetNode
    )
parseNodeOutputUtxo = phoistAcyclic $
  plam $ \nodeCS out -> P.do
    PTxOut {ptxOut'value, ptxOut'datum} <- pmatch $ pfromData out
    value <- plet $ pfromData ptxOut'value
    csPair <- plet $ ptrySingleTokenCS # nodeCS # value
    let nodeTokenName = pfstBuiltin # csPair
        amount = pfromData $ psndBuiltin # csPair
    POutputDatum od <- pmatch ptxOut'datum
    datum <- plet $ punsafeCoerce od
    PDirectorySetNode {pkey, pnext} <- pmatch (pfromData datum)

    nodeKeyData <- plet (pforgetData pkey)
    let nodeKey = pasByteStr # nodeKeyData
        nodeNext = pasByteStr # pforgetData pnext

    -- Prevents TokenDust attack
    passert "Too many assets" $ pcountOfUniqueTokens # value #== 2
    passert "Incorrect number of node tokens" $ amount #== 1
    passert "Node is not ordered" $ nodeKey #< nodeNext
    passert "Incorrect token name" $ nodeKeyData #== pforgetData nodeTokenName
    datum

-- Potentially use this in the future if we plan to manage additional
-- value in the directory nodes.
parseNodeOutputUtxoPair ::
  ClosedTerm
    ( PAsData PCurrencySymbol
        :--> PTxOut
        :--> PPair (PValue 'Sorted 'Positive) (PAsData PDirectorySetNode)
    )
parseNodeOutputUtxoPair = phoistAcyclic $
  plam $ \nodeCS out -> P.do
    PTxOut {ptxOut'value, ptxOut'datum} <- pmatch out
    value <- plet $ pfromData ptxOut'value
    csPair <- plet $ ptrySingleTokenCS # nodeCS # value
    let tn = pfromData $ pfstBuiltin # csPair
        amount = pfromData $ psndBuiltin # csPair
    POutputDatum od <- pmatch ptxOut'datum
    datum <- plet $ punsafeCoerce od
    let nodeKey = pparseNodeKey tn
        datumKey = punsafeCoerce $ pmatch (pfromData datum) $ \(PDirectorySetNode {pkey}) -> pfromData pkey

    -- Prevents TokenDust attack
    passert "All FSN tokens from node policy" $
      pheadSingleton # (pfindCurrencySymbolsByTokenPrefix # value # pconstant "FSN") #== nodeCS
    passert "Too many assets" $ pcountOfUniqueTokens # value #== 2
    passert "Incorrect number of nodeTokens" $ amount #== 1
    passert "Incorrect token name" $ nodeKey #== datumKey
    pcon (PPair value datum)

makeCommon ::
  forall {r :: PType} {s :: S}.
  Term s PScriptContext ->
  TermCont @r
    s
    ( PDirectoryCommon s )
makeCommon ctx' = do
  ------------------------------
  -- Preparing info needed for validation:
  PScriptContext {pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatchC ctx'
  PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'mint} <- pmatchC pscriptContext'txInfo

  ownCS <- tcont . plet $ P.do
    PMintingScript mintRecord <- pmatch pscriptContext'scriptInfo
    mintRecord

  mint <- tcont . plet $ pnormalize #$ pfromData ptxInfo'mint
  hasNodeTk <- tcont . plet $ phasDataCS # ownCS
  txInputs <- tcont . plet $ pfromData ptxInfo'inputs
  let txOutputs = pfromData ptxInfo'outputs
  fromNodeValidator <- tcont . plet $ pmapFilter @PBuiltinList # plam (\txo -> hasNodeTk # pfromData (ptxOutValue txo)) # plam (ptxInInfoResolved . pfromData) # txInputs
  toNodeValidator <- tcont . plet $ pfilter @PBuiltinList # plam (\txo -> hasNodeTk # pfromData (ptxOutValue $ pfromData txo)) # txOutputs
  ------------------------------

  let atNodeValidator =
        pelimList
          ( \firstNodeInput _ ->
            let isSameAddress = (ppaysToAddress # ptxOutAddress firstNodeInput)
             in pall # isSameAddress # toNodeValidator
          )
          (pconstant True)
          fromNodeValidator

  pguardC "all same origin" atNodeValidator

  nodeInputs <- tcont . plet $ pmap # nodeInputUtxoDatumUnsafe # punsafeCoerce fromNodeValidator

  nodeOutputs <-
    tcont . plet $
      pmap
        # (parseNodeOutputUtxo # ownCS)
        # toNodeValidator

  let common =
        MkCommon
          { ownCS = pfromData ownCS
          , mint
          , nodeInputs
          , nodeOutputs
          }
  pure common


-- | Initialize the linked list
--   Validations:
--     - No node inputs should be spent
--     - There should be only a single node token minted (the origin node token)
--     - There should be exactly one node output, the key of which should be empty and the next key should be empty
pInit :: forall (s :: S). PDirectoryCommon s -> Term s PUnit
pInit common = P.do
  -- Input Checks
  passert "Init must not spend Nodes" $ pnull # common.nodeInputs

  -- Output Checks:
  let nodeOutput = pheadSingleton # common.nodeOutputs
  passert "Init output one node and empty" $ pisEmptyNode nodeOutput

  -- Mint checks:
  passert "Incorrect mint for Init" $
    correctNodeTokenMinted # common.ownCS # poriginNodeTN # 1 # common.mint

  pconstant ()

-- | Insert a new node into the linked list
--   Validations:
--     - There should be only one spent node (tx inputs contains only one node UTxO)
--     - The spent node indeed covers the key we want to insert
--     - The key of the spent node is lexographically less than pkToInsert and
--       the next key of the spent node is lexographically greater than pkToInsert.
--     - The node outputs should contain the inserted node
--     - There should be only a single node token minted (token name of which should match the key we are inserting)
pInsert ::
  forall (s :: S).
  PDirectoryCommon s ->
  Term s (PAsData PByteString :--> PUnit)
pInsert common = plam $ \pkToInsert -> P.do
  keyToInsert <- plet $ pfromData pkToInsert

  passert "Key to insert must be valid Currency Symbol" $ ptraceInfoIfFalse (pshow keyToInsert) $ plengthBS # keyToInsert #== 28

  -- Input Checks:
  -- There is only one spent node (tx inputs contains only one node UTxO)
  let coveringDatum = pheadSingleton # common.nodeInputs

  -- Output Checks:
  PDirectorySetNode {pkey, pnext, ptransferLogicScript, pissuerLogicScript} <- pmatch $ pfromData coveringDatum
  coveringDatumKey <- plet $ pasByteStr # pforgetData pkey
  coveringDatumNext <- plet $ pasByteStr # pforgetData pnext

  -- The key of the spent node is lexographically less than pkToInsert and
  -- the next key of the spent node is lexographically greater than pkToInsert.
  -- Thus the coveringNode is the node upon which we are inserting.
  passert "Spent node should cover inserting key" $
    pand' # (coveringDatumKey #< keyToInsert) # (keyToInsert #< coveringDatumNext)

  let isInsertedOnNode = pisInsertedOnNode # pdata keyToInsert # pdata coveringDatumKey # ptransferLogicScript # pissuerLogicScript
      isInsertedNode = pisInsertedNode # pdata keyToInsert # pdata coveringDatumNext

  passert "Incorrect node outputs for Insert" $
    pany
      # isInsertedOnNode
      # common.nodeOutputs
      #&& pany # isInsertedNode # common.nodeOutputs

  -- Mint checks:
  passert "Incorrect mint for Insert" $
    correctNodeTokenMinted # common.ownCS # pnodeKeyTN keyToInsert # 1 # common.mint

  pconstant ()

-- Common information shared between all redeemers.
data PDirectoryCommon (s :: S) = MkCommon
  { ownCS :: Term s PCurrencySymbol
  -- ^ state token (own) CS
  , mint :: Term s (PValue 'Sorted 'NonZero)
  -- ^ value minted in current Tx
  , nodeInputs :: Term s (PBuiltinList (PAsData PDirectorySetNode))
  -- ^ node inputs in the tx
  , nodeOutputs :: Term s (PBuiltinList (PAsData PDirectorySetNode))
  -- ^ node outputs in the tx
  }
  deriving stock (Generic)
