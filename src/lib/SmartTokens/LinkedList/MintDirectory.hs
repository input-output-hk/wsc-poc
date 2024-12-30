{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QualifiedDo           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module SmartTokens.LinkedList.MintDirectory (
  mkDirectoryNodeMP,
  DirectoryNodeAction (..)
) where

import Generics.SOP qualified as SOP
import Plutarch.Core.Utils (pand'List, passert, phasUTxO)
import Plutarch.DataRepr (DerivePConstantViaData (..))
import Plutarch.LedgerApi.V3 (PScriptContext, PTxOutRef)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude (ClosedTerm, DerivePlutusType (..), Generic, PAsData,
                         PByteString, PDataRecord, PEq, PIsData,
                         PLabeledType ((:=)), PUnit, PlutusType, PlutusTypeData,
                         S, Term, TermCont (runTermCont), pconstant, perror,
                         pfield, pfromData, pif, plam, plet, pletFields, pmatch,
                         pto, type (:-->), (#))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V3 (CurrencySymbol)
import PlutusTx qualified
import SmartTokens.LinkedList.Common (makeCommon, pInit, pInsert)

--------------------------------
-- FinSet Node Minting Policy:
--------------------------------
data DirectoryNodeAction
  = InitDirectory
  | InsertDirectoryNode CurrencySymbol
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

PlutusTx.makeIsDataIndexed ''DirectoryNodeAction
  [('InitDirectory, 0), ('InsertDirectoryNode, 1)]

deriving via
  (DerivePConstantViaData DirectoryNodeAction PDirectoryNodeAction)
  instance
    (PConstantDecl DirectoryNodeAction)

data PDirectoryNodeAction (s :: S)
  = PInit (Term s (PDataRecord '[]))
  | PInsert (Term s (PDataRecord '["keyToInsert" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance PUnsafeLiftDecl PDirectoryNodeAction where
  type PLifted PDirectoryNodeAction = DirectoryNodeAction

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
      let mintsProgrammableToken = pconstant True
          insertChecks =
            pand'List
                [ mintsProgrammableToken
                ]
      pif insertChecks (pInsert common # pkToInsert) perror
