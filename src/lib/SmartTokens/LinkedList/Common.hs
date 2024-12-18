module SmartTokens.LinkedList.Common (
  PDirectoryCommon (..),
  makeCommon,
  pInit,
  pInsert,
  nodeInputUtxoDatumUnsafePair,
  parseNodeOutputUtxoPair,
) where

import Plutarch.LedgerApi.Value (pnormalize)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.Monadic qualified as P
import Plutarch.Bool (pand')
import Plutarch.Prelude
    ( Generic,
      (#),
      (#$),
      phoistAcyclic,
      plet,
      pto,
      pcon,
      pmatch,
      tcont,
      type (:-->),
      ClosedTerm,
      PType,
      S,
      Term,
      plam,
      TermCont,
      PByteString,
      pconstant,
      PEq((#==)),
      PBool,
      PPartialOrd((#<)),
      PInteger,
      (#&&),
      pdata,
      pfromData,
      pfield,
      pletFields,
      pall,
      pany,
      pfilter,
      pmap,
      pguardC,
      PAsData,
      PBuiltinList,
      PListLike(pnull, phead),
      PMaybe(PJust),
      PPair(..),
      PUnit )
import Plutarch.Unsafe (punsafeCoerce)
import SmartTokens.Core.Utils (
  passert,
  paysToAddress,
  pcountOfUniqueTokens,
  pfindCurrencySymbolsByTokenPrefix,
  pheadSingleton,
  phasDataCS,
  psingletonOfCS,
  ptryFromInlineDatum,
  pmapFilter,
 )
import Types.Constants (pnodeKeyTN, poriginNodeTN, ptryParseNodeKey)
import SmartTokens.Types.PTokenDirectory
    ( PDirectorySetNode,
      pisInsertedOnNode,
      pisInsertedNode,
      pisEmptyNode )
import Plutarch.LedgerApi.V3
    ( KeyGuarantees(Sorted),
      AmountGuarantees(NonZero, Positive),
      PCurrencySymbol,
      PTokenName,
      PValue,
      POutputDatum(POutputDatum),
      PTxOut,
      PScriptContext,
      PScriptInfo(PMintingScript) )
import Plutarch.Builtin (pforgetData, pasByteStr)

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
nodeInputUtxoDatumUnsafePair = phoistAcyclic $
  plam $ \out -> pletFields @'["value", "datum"] out $ \outF ->
    plet (punsafeCoerce $ ptryFromInlineDatum # outF.datum) $ \nodeDat ->
      pcon (PPair (pfromData outF.value) nodeDat)

nodeInputUtxoDatumUnsafe
  :: ClosedTerm (PAsData PTxOut :--> PAsData PDirectorySetNode)
nodeInputUtxoDatumUnsafe = phoistAcyclic $ plam $ \txOut ->
  punsafeCoerce $ ptryFromInlineDatum # (pfield @"datum" # txOut)

parseNodeOutputUtxo ::
  ClosedTerm
    ( PAsData PCurrencySymbol
        :--> PAsData PTxOut
        :--> PAsData PDirectorySetNode
    )
parseNodeOutputUtxo = phoistAcyclic $
  plam $ \nodeCS out -> P.do
    txOut <- pletFields @'["address", "value", "datum"] out
    value <- plet $ pfromData txOut.value
    PPair nodeTokenName amount <- pmatch $ psingletonOfCS # nodeCS # value
    POutputDatum od <- pmatch $ pfromData txOut.datum
    datum <- plet $ punsafeCoerce (pfield @"outputDatum" # od)
    datumF <- pletFields @'["key", "next", "transferLogicScript", "issuerLogicScript"] datum

    nodeKeyData <- plet (pforgetData datumF.key)
    let nodeKey = pasByteStr # nodeKeyData
        nodeNext = pasByteStr # pforgetData datumF.next

    -- The following are checked by `pisInsertedNode` 
    -- passert "transferLogicScript deserialization" $ pdeserializesToCredential # datumF.transferLogicScript
    -- passert "issuerLogicScript deserialization" $ pdeserializesToCredential # datumF.issuerLogicScript

    -- Prevents TokenDust attack
    passert "Too many assets" $ pcountOfUniqueTokens # value #== 2
    passert "Incorrect number of node tokens" $ amount #== 1
    passert "Node is not ordered" $ nodeKey #< nodeNext
    passert "Incorrect token name" $ nodeKeyData #== pforgetData (pdata nodeTokenName)
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
    txOut <- pletFields @'["address", "value", "datum"] out
    value <- plet $ pfromData txOut.value
    PPair tn amount <- pmatch $ psingletonOfCS # nodeCS # value
    POutputDatum od <- pmatch $ pfromData txOut.datum
    datum <- plet $ punsafeCoerce (pfield @"outputDatum" # od)
    let nodeKey = ptryParseNodeKey # tn
        datumKey = punsafeCoerce $ pfield @"key" # datum

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
  ctx <- tcont $ pletFields @'["txInfo", "scriptInfo"] ctx'
  info <-
    tcont $
      pletFields
        @'["inputs", "outputs", "mint", "referenceInputs", "signatories", "validRange"]
        ctx.txInfo

  ownCS <- tcont . plet $ P.do
    PMintingScript mintRecord <- pmatch ctx.scriptInfo
    pfield @"_0" # mintRecord

  mint <- tcont . plet $ pnormalize #$ pfromData info.mint
  -- asOuts <- tcont . plet $ pmap # plam (pfield @"resolved" #)
  -- refInsAsOuts <- tcont . plet $ asOuts # pfromData info.referenceInputs
  hasNodeTk <- tcont . plet $ phasDataCS # ownCS
  -- insAsOuts <- tcont . plet $ pmap # plam (pfield @"resolved" #) # info.inputs
  -- onlyAtNodeVal <- tcont . plet $ pfilter @PBuiltinList # plam (\txo -> (hasNodeTk # (pfield @"value" # txo)))
  txInputs <- tcont . plet $ pfromData info.inputs
  let txOutputs = punsafeCoerce @_ @_ @(PBuiltinList (PAsData PTxOut)) (pfromData info.outputs)
  fromNodeValidator <- tcont . plet $ pmapFilter @PBuiltinList # plam (\txo -> hasNodeTk # (pfield @"value" # txo)) # plam (pfield @"resolved" #) # txInputs
  toNodeValidator <- tcont . plet $ pfilter @PBuiltinList # plam (\txo -> hasNodeTk # (pfield @"value" # txo)) # txOutputs
  ------------------------------

  let firstNodeInput :: Term _ (PAsData PTxOut) = phead @PBuiltinList # fromNodeValidator
  let atNodeValidator =
        let isSameAddress = (paysToAddress # (pfield @"address" # firstNodeInput))
         in pall # isSameAddress # toNodeValidator

  pguardC "all same origin" atNodeValidator

  nodeInputs <- tcont . plet $ pmap # nodeInputUtxoDatumUnsafe # fromNodeValidator

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
  passert "Init output one node and empty" $ pisEmptyNode # nodeOutput

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

  -- Input Checks:
  -- There is only one spent node (tx inputs contains only one node UTxO)
  let coveringDatum = pheadSingleton # common.nodeInputs

  -- Output Checks:
  coveringDatumF <- pletFields @'["key", "next", "transferLogicScript", "issuerLogicScript"] coveringDatum
  coveringDatumKey <- plet $ pasByteStr # pforgetData coveringDatumF.key
  coveringDatumNext <- plet $ pasByteStr # pforgetData coveringDatumF.next

  -- The key of the spent node is lexographically less than pkToInsert and 
  -- the next key of the spent node is lexographically greater than pkToInsert.
  -- Thus the coveringNode is the node upon which we are inserting. 
  passert "Spent node should cover inserting key" $
    pand' # (coveringDatumKey #< keyToInsert) # (keyToInsert #< coveringDatumNext)

  let isInsertedOnNode = pisInsertedOnNode # pdata keyToInsert # pdata coveringDatumKey # coveringDatumF.transferLogicScript # coveringDatumF.issuerLogicScript
      isInsertedNode = pisInsertedNode # pdata keyToInsert # pdata coveringDatumNext

  passert "Incorrect node outputs for Insert" $
    pany
      # isInsertedOnNode
      # common.nodeOutputs
      #&& pany # isInsertedNode # common.nodeOutputs

  -- Mint checks:
  passert "Incorrect mint for Insert" $
    correctNodeTokenMinted # common.ownCS # (pnodeKeyTN # keyToInsert) # 1 # common.mint

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
