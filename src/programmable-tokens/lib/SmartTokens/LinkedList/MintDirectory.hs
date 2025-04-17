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
import Plutarch.Core.Utils (passert, phasUTxO)
import Plutarch.Internal.Lift
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext (..),
                              PTxInfo (..), PTxOutRef)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Repr.Data
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V3 (CurrencySymbol, ScriptHash)
import PlutusTx qualified
import SmartTokens.LinkedList.Common (makeCommon, pInit, pInsert)

--------------------------------
-- FinSet Node Minting Policy:
--------------------------------
data DirectoryNodeAction
  = InitDirectory
  | InsertDirectoryNode CurrencySymbol ScriptHash
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

PlutusTx.makeIsDataIndexed ''DirectoryNodeAction
  [('InitDirectory, 0), ('InsertDirectoryNode, 1)]

data PDirectoryNodeAction (s :: S)
  = PInit
  | PInsert { pkeyToInsert :: Term s (PAsData PByteString), phashedParam :: Term s (PAsData PByteString) }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq)
  deriving (PlutusType) via DeriveAsDataStruct PDirectoryNodeAction

deriving via DeriveDataPLiftable PDirectoryNodeAction DirectoryNodeAction
  instance PLiftable PDirectoryNodeAction

mkDirectoryNodeMP ::
  ClosedTerm
    ( PAsData PTxOutRef
      :--> PAsData PCurrencySymbol
      :--> PScriptContext
      :--> PUnit
    )
mkDirectoryNodeMP = plam $ \initUTxO issuanceCborHexCS ctx -> P.do
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
    PInsert action hashedParam -> P.do
      pkToInsert <- plet action
      pInsert issuanceCborHexCS common # pfromData pkToInsert # pfromData hashedParam
