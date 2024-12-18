{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module SmartTokens.LinkedList.MintDirectory (
  mkDirectoryNodeMP,
) where

import Plutarch.LedgerApi.V3 ( PScriptContext, PTxOutRef )
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)
import SmartTokens.LinkedList.Common (
  makeCommon,
  pInit,
  pInsert,
 )

import Plutarch.Prelude
    ( Generic,
      (#),
      perror,
      plet,
      pto,
      pmatch,
      type (:-->),
      ClosedTerm,
      S,
      Term,
      plam,
      DerivePlutusType(..),
      PlutusType,
      TermCont(runTermCont),
      PByteString,
      pconstant,
      PEq,
      pif,
      pfromData,
      pfield,
      pletFields,
      PAsData,
      PIsData,
      PDataRecord,
      PLabeledType((:=)),
      PlutusTypeData,
      PUnit )
import SmartTokens.Core.Utils (pand'List, passert, phasUTxO)

--------------------------------
-- FinSet Node Minting Policy:
--------------------------------

data PDirectoryNodeAction (s :: S)
  = PInit (Term s (PDataRecord '[]))
  | PInsert (Term s (PDataRecord '["keyToInsert" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PDirectoryNodeAction where type DPTStrat _ = PlutusTypeData

mkDirectoryNodeMP ::
  ClosedTerm
    ( PAsData PTxOutRef
      :--> PScriptContext
      :--> PUnit
    )
mkDirectoryNodeMP = plam $ \initUTxO ctx -> P.do
  let red = punsafeCoerce @_ @_ @PDirectoryNodeAction (pto (pfield @"redeemer" # ctx))
 
  common <- runTermCont $ makeCommon ctx

  pmatch red $ \case
    PInit _ -> P.do
      ctxF <- pletFields @'["txInfo"] ctx
      infoF <- pletFields @'["inputs"] ctxF.txInfo
      passert "Init must consume TxOutRef" $
        phasUTxO # initUTxO # pfromData infoF.inputs
      pInit common
    PInsert action -> P.do
      act <- pletFields @'["keyToInsert"] action
      pkToInsert <- plet act.keyToInsert
      let mintsProgrammableToken = pconstant False 
          insertChecks =
            pand'List
                [ mintsProgrammableToken
                ]
      pif insertChecks (pInsert common # pkToInsert) perror
