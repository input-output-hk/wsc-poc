{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
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
import GHC.Generics (Generic)
import Plutarch.Core.Utils (pand'List, passert, phasUTxO)
import Plutarch.Internal.Lift
import Plutarch.LedgerApi.V3 (PScriptContext (..), PTxInfo (..), PTxOutRef)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Repr.Data
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

data PDirectoryNodeAction (s :: S)
  = PInit
  | PInsert { pkeyToInsert :: Term s (PAsData PByteString) }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq)
  deriving (PlutusType) via DeriveAsDataStruct PDirectoryNodeAction

deriving via DeriveDataPLiftable PDirectoryNodeAction DirectoryNodeAction
  instance PLiftable PDirectoryNodeAction

mkDirectoryNodeMP ::
  ClosedTerm
    ( PAsData PTxOutRef
      :--> PScriptContext
      :--> PUnit
    )
mkDirectoryNodeMP = plam $ \initUTxO ctx -> P.do
  PScriptContext {pscriptContext'redeemer} <- pmatch ctx
  let red = punsafeCoerce @PDirectoryNodeAction (pto pscriptContext'redeemer)

  common <- runTermCont $ makeCommon ctx

  pmatch red $ \case
    PInit -> P.do
      PScriptContext {pscriptContext'txInfo} <- pmatch ctx
      PTxInfo {ptxInfo'inputs} <- pmatch pscriptContext'txInfo
      passert "Init must consume TxOutRef" $
        phasUTxO # pfromData initUTxO # pfromData ptxInfo'inputs
      pInit common
    PInsert action -> P.do
      pkToInsert <- plet action
      let mintsProgrammableToken = pconstant True
          insertChecks =
            pand'List
                [ mintsProgrammableToken
                ]
      pif insertChecks (pInsert common # pkToInsert) perror
