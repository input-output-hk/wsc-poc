{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module SmartTokens.LinkedList.MintDirectory (
  mkDirectoryNodeMP,
  mkDirectoryNodeMPW,
) where

import Plutarch.LedgerApi.V3
import Plutarch.LedgerApi.Interval (pafter, pbefore)

--  pRemoveAndDeinit,
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)
import SmartTokens.LinkedList.Common (
  makeCommon,
  pDeinit,
  pInit,
  pInsert,
  pRemove,
 )

import Plutarch.Prelude
import Plutarch.Core.Utils (pand'List, passert, pcond, pisFinite, phasUTxO, pintToByteString)
import SmartTokens.Types.PTokenDirectory 
import Types.Constants (claimRoot, airdropOperator)
import Plutarch.Core.Crypto (pcardanoPubKeyToPubKeyHash, pethereumPubKeyToPubKeyHash, pcompressPublicKey)
import Plutarch.Builtin (pserialiseData, pforgetData, PDataNewtype(..))
import MerkleTree.MerklePatriciaForestry (phas)
import Plutarch.Crypto (pverifyEcdsaSecp256k1Signature, pblake2b_256)

--------------------------------
-- FinSet Node Minting Policy:
--------------------------------

data PDirectoryNodeAction (s :: S)
  = PLInit (Term s (PDataRecord '[]))
  | PLInsert (Term s (PDataRecord '["keyToInsert" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PDirectoryNodeAction where type DPTStrat _ = PlutusTypeData

mkDirectoryNodeMP ::
  ClosedTerm
    ( PAsData PTxOutRef
        :--> PDirectoryNodeAction
        :--> PScriptContext
        :--> PUnit
    )
mkDirectoryNodeMP = plam $ \initUTxO redm ctx -> P.do

  (common, inputs, sigs, vrange) <-
    runTermCont $
      makeCommon ctx

  pmatch redm $ \case
    PLInit _ -> P.do
      passert "Init must consume TxOutRef" $
        phasUTxO # initUTxO # inputs
      pInit common
    PLInsert action -> P.do
      act <- pletFields @'["keyToInsert"] action
      pkToInsert <- plet act.keyToInsert
      let mintsProgrammableToken = pconstant False 
          insertChecks =
            pand'List
                [ mintsProgrammableToken
                ]
      pif insertChecks (pInsert common # pkToInsert) perror

mkDirectoryNodeMPW ::
  ClosedTerm
    ( PAirdropConfig
        :--> PScriptContext :--> PUnit 
    )
mkDirectoryNodeMPW = phoistAcyclic $ plam $ \claimConfig ctx ->
  let red = punsafeCoerce @_ @_ @PDirectoryNodeAction (pto (pfield @"redeemer" # ctx))
   in mkDirectoryNodeMP # claimConfig # red # ctx
