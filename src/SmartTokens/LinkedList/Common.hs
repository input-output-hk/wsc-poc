module AirdropEvent.Mint.Common (
  PDirectoryCommon (..),
  makeCommon,
  pInit,
  pDeinit,
  pRemove,
  pInsert,
) where

import Plutarch.LedgerApi.Value (plovelaceValueOf, pnormalize, pvalueOf)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
-- import Plutarch.LedgerApi.Interval (pafter, pbefore)
-- import Plutarch.TermCont (pguardC)
import Plutarch.List (pconvertLists)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import SmartTokens.Core.Utils (
  pand'List,
  passert,
  paysToAddress,
  pcountOfUniqueTokens,
  pfindWithRest,
  pfindCurrencySymbolsByTokenPrefix,
  pheadSingleton,
  phasDataCS,
  psingletonOfCS,
  (#>=),
  pfromPDatum,
  ptryFromInlineDatum,
  pmapFilter,
 )
import Types.Constants (pnodeKeyTN, poriginNodeTN, ptryParseNodeKey)
import SmartTokens.Types.PTokenDirectory
import Plutarch.LedgerApi.V3
import Plutarch.Builtin (pforgetData)

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

-- nodeInputUtxoDatum ::
--   ClosedTerm
--     ( PAsData PCurrencySymbol
--         :--> PTxOut
--         :--> PMaybe (PAsData PDirectorySetNode)
--     )
-- nodeInputUtxoDatum = phoistAcyclic $
--   plam $ \nodeCS out -> P.do
--     txOut <- pletFields @'["datum", "value"] out
--     let value = pfromData txOut.value
--     pcheck (phasDataCS # nodeCS # value) $
--       punsafeCoerce $
--         ptryFromInlineDatum # txOut.datum

nodeInputUtxoDatumUnsafe ::
  ClosedTerm
    ( PTxOut
        :--> PPair (PValue 'Sorted 'Positive) (PAsData PDirectorySetNode)
    )
nodeInputUtxoDatumUnsafe = phoistAcyclic $
  plam $ \out -> pletFields @'["value", "datum"] out $ \outF ->
    plet (punsafeCoerce $ ptryFromInlineDatum # outF.datum) $ \nodeDat ->
      pcon (PPair (pfromData outF.value) nodeDat)

parseNodeOutputUtxo ::
  ClosedTerm
    ( PAsData PCurrencySymbol
        :--> PTxOut
        :--> PPair (PValue 'Sorted 'Positive) (PAsData PDirectorySetNode)
    )
parseNodeOutputUtxo = phoistAcyclic $
  plam $ \nodeCS out -> P.do
    txOut <- pletFields @'["address", "value", "datum"] out
    value <- plet $ pfromData txOut.value
    PPair tn amount <- pmatch $ psingletonOfCS # nodeCS # value
    POutputDatum od <- pmatch $ pfromData txOut.datum
    datum <- plet $ pfromPDatum #$ pfield @"outputDatum" # od
    let nodeKey = ptryParseNodeKey # tn
        datumKey = pmatch (pfield @"key" # datum)

    -- Prevents TokenDust attack
    passert "All FSN tokens from node policy" $
      pheadSingleton # (pfindCurrencySymbolsByTokenPrefix # value # pconstant "FSN") #== nodeCS
    passert "Too many assets" $ pcountOfUniqueTokens # value #== 2
    passert "Incorrect number of nodeTokens" $ amount #== 1
    passert "node is not ordered" $ validNode # datum
    passert "Incorrect token name" $ nodeKey #== datumKey
    pcon (PPair value datum)

makeCommon ::
  forall {r :: PType} {s :: S}.
  Term s PScriptContext ->
  TermCont @r
    s
    ( PDirectoryCommon s
    , Term s (PBuiltinList PTxInInfo)
    , Term s (PBuiltinList (PAsData PPubKeyHash))
    , Term s (PInterval PPosixTime)
    )
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
  txInputs <- tcont . plet $ punsafeCoerce @_ @_ @(PBuiltinList PTxInInfo) info.inputs
  let txOutputs = punsafeCoerce @_ @_ @(PBuiltinList PTxOut) info.outputs
  fromNodeValidator <- tcont . plet $ pmapFilter @PBuiltinList # plam (\txo -> hasNodeTk # (pfield @"value" # txo)) # plam (pfield @"resolved" #) # txInputs
  toNodeValidator <- tcont . plet $ pfilter @PBuiltinList # plam (\txo -> hasNodeTk # (pfield @"value" # txo)) # txOutputs
  ------------------------------

  let atNodeValidator =
        pelimList
          ( \x xs -> plet (paysToAddress # (pfield @"address" # x)) $ \isSameAddr ->
              pand'List
                [ pall # isSameAddr # xs
                , pall # isSameAddr # toNodeValidator
                ]
          )
          (pconstant True)
          fromNodeValidator

  pguardC "all same origin" atNodeValidator

  nodeInputs <- tcont . plet $ pmap # nodeInputUtxoDatumUnsafe #$ pconvertLists # fromNodeValidator

  nodeOutputs <-
    tcont . plet $
      pmap
        # (parseNodeOutputUtxo # ownCS)
        #$ pconvertLists
        # toNodeValidator

  let common =
        MkCommon
          { ownCS = pfromData ownCS
          , mint
          , nodeInputs
          , nodeOutputs
          }
  vrange <- tcont . plet $ pfromData info.validRange
  pure
    ( common
    , txInputs
    , info.signatories
    , vrange
    )

pInit :: forall (s :: S). PDirectoryCommon s -> Term s PUnit
pInit common = P.do
  -- Input Checks
  passert "Init must not spend Nodes" $ pnull # common.nodeInputs
  -- Output Checks:
  PPair _ otherNodes <-
    pmatch $
      pfindWithRest # plam (\nodePair -> pmatch nodePair (\(PPair _ nodeDat) -> isEmptySet # nodeDat)) # common.nodeOutputs

  passert "Init output exactly one Node" $
    pnull # otherNodes
  -- Mint checks:
  passert "Incorrect mint for Init" $
    correctNodeTokenMinted # common.ownCS # poriginNodeTN # 1 # common.mint

  pconstant ()

pInsert ::
  forall (s :: S).
  PDirectoryCommon s ->
  Term s (PAsData PByteString :--> PAsData PCredential :--> PAsData PCredential :--> PUnit)
pInsert common = plam $ \pkToInsert expectedTransferScript expectedIssuerScript -> P.do
  keyToInsert <- plet $ pfromData pkToInsert

  -- Input Checks:
  -- There is only one spent node (tx inputs contains only one node UTxO)
  -- The spent node indeed covers the key we want to insert
  PPair coveringValue coveringDatum <- pmatch $ pheadSingleton # common.nodeInputs
  passert "Spent node should cover inserting key" $ coversLiquidityKey # coveringDatum # keyToInsert

  -- Output Checks:
  coveringDatumF <- pletFields @'["key", "next", "transferLogicScript", "issuerLogicScript"] coveringDatum

  nodeKeyToInsert <- plet $ pcon $ PKey $ pdcons @"_0" # pdata keyToInsert #$ pdnil
  isInsertedOnNode <- plet $ pisInsertedOnNode # nodeKeyToInsert # coveringDatumF.key # coveringDatumF.transferLogicScript # coveringDatumF.issuerLogicScript
  isInsertedNode <- plet $ pisInsertedNode # nodeKeyToInsert # coveringDatumF.next # expectedTransferScript # expectedIssuerScript

  passert "Incorrect node outputs for Insert" $
    pany
      # plam (\nodePair -> pmatch nodePair (\(PPair val dat) -> val #== coveringValue #&& isInsertedOnNode # dat))
      # common.nodeOutputs
      #&& pany # plam (\nodePair -> pmatch nodePair (\(PPair _ dat) -> isInsertedNode # dat)) # common.nodeOutputs

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
  , nodeInputs :: Term s (PList (PPair (PValue 'Sorted 'Positive) (PAsData PDirectorySetNode)))
  -- ^ current Tx outputs to AuctionValidator
  , nodeOutputs :: Term s (PList (PPair (PValue 'Sorted 'Positive) (PAsData PDirectorySetNode)))
  -- ^ current Tx inputs
  }
  deriving stock (Generic)
