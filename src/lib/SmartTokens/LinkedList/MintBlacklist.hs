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

module SmartTokens.LinkedList.MintBlacklist (
  mkBlacklistNodeMP,
  BlacklistNodeAction (..),
  PBlacklistNodeAction (..),
) where

import Generics.SOP qualified as SOP
import GHC.Generics (Generic)
import Plutarch.Core.Context
import Plutarch.Core.Utils (pand'List, passert, phasUTxO)
import Plutarch.Internal.Lift
import Plutarch.LedgerApi.V3 (PPubKeyHash, PScriptContext (..), PTxInfo (..),
                              PTxOutRef)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Repr.Data
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V3 (PubKeyHash)
import PlutusTx qualified
import SmartTokens.LinkedList.BlacklistCommon (makeCommon, pInit, pInsert,
                                               pRemove)
--------------------------------
-- Blacklist Node Minting Policy:
--------------------------------
data BlacklistNodeAction
  = InitBlacklist
  | InsertBlacklistNode PubKeyHash
  | RemoveBlacklistNode PubKeyHash
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

PlutusTx.makeIsDataIndexed ''BlacklistNodeAction
  [('InitBlacklist, 0), ('InsertBlacklistNode, 1), ('RemoveBlacklistNode, 2)]

data PBlacklistNodeAction (s :: S)
  = PInitBlacklist
  | PInsertBlacklistNode { pkeyToInsert :: Term s (PAsData PByteString) }
  | PRemoveBlacklistNode { pkeyToRemove :: Term s (PAsData PByteString) }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq)
  deriving (PlutusType) via DeriveAsDataStruct PBlacklistNodeAction

deriving via DeriveDataPLiftable PBlacklistNodeAction BlacklistNodeAction
  instance PLiftable PBlacklistNodeAction

mkBlacklistNodeMP ::
  ClosedTerm
    ( PAsData PTxOutRef
      :--> PAsData PPubKeyHash
      :--> PScriptContext
      :--> PUnit
    )
mkBlacklistNodeMP = plam $ \initUTxO managerPkh ctx -> P.do
  PScriptContext {pscriptContext'redeemer} <- pmatch ctx
  let red = punsafeCoerce @PBlacklistNodeAction (pto pscriptContext'redeemer)

  common <- runTermCont $ makeCommon ctx

  pmatch red $ \case
    PInitBlacklist -> P.do
      PScriptContext {pscriptContext'txInfo} <- pmatch ctx
      PTxInfo {ptxInfo'inputs} <- pmatch pscriptContext'txInfo
      passert "Init must consume TxOutRef" $
        phasUTxO # pfromData initUTxO # pfromData ptxInfo'inputs
      pInit common
    PInsertBlacklistNode action -> P.do
      let signatories = ptxInfoSignatories $ pscriptContextTxInfo ctx
      pkToInsert <- plet action
      let insertChecks =
            pand'List
                [ pelem # managerPkh # pfromData signatories
                ]
      pif insertChecks (pInsert common # pkToInsert) perror
    PRemoveBlacklistNode action -> P.do
      let signatories = ptxInfoSignatories $ pscriptContextTxInfo ctx
      pkToRemove <- plet action
      let removeChecks =
            pand'List
                [ pelem # managerPkh # pfromData signatories
                ]
      pif removeChecks (pRemove common # pkToRemove) perror
