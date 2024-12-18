{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QualifiedDo         #-}

module SmartTokens.LinkedList.MintDirectory (
  mkDirectoryNodeMP,
) where

import Plutarch.LedgerApi.V3 (PScriptContext, PTxOutRef)
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)
import SmartTokens.LinkedList.Common (makeCommon, pInit, pInsert)

import Plutarch.Core.Utils (pand'List, passert, phasUTxO)
import Plutarch.Prelude (ClosedTerm, DerivePlutusType (..), Generic, PAsData,
                         PByteString, PDataRecord, PEq, PIsData,
                         PLabeledType ((:=)), PUnit, PlutusType, PlutusTypeData,
                         S, Term, TermCont (runTermCont), pconstant, perror,
                         pfield, pfromData, pif, plam, plet, pletFields, pmatch,
                         pto, type (:-->), (#))

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
