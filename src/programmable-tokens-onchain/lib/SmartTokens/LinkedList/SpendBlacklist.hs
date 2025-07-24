{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QualifiedDo          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module SmartTokens.LinkedList.SpendBlacklist (pmkBlacklistSpending) where

import Plutarch.Core.ValidationLogic (pvalidateConditions)
import Plutarch.Core.Value (phasDataCS)
import Plutarch.LedgerApi.V3 (PCurrencySymbol,
                              PScriptContext (PScriptContext, pscriptContext'scriptInfo, pscriptContext'txInfo),
                              PScriptInfo, PTxInfo (PTxInfo, ptxInfo'mint))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude (ClosedTerm, PAsData, PBool, PEq ((#==)), PUnit, Term,
                         pasConstr, pdata, pforgetData, pfromData, pfstBuiltin,
                         plam, pmatch, ptraceInfoIfFalse, type (:-->), (#))

pisSpendingPurpose :: Term s (PAsData PScriptInfo) -> Term s PBool
pisSpendingPurpose term = (pfstBuiltin # (pasConstr # pforgetData term)) #== 1

pmkBlacklistSpending :: ClosedTerm (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
pmkBlacklistSpending = plam $ \blacklistMP ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
  PTxInfo {ptxInfo'mint} <- pmatch pscriptContext'txInfo

  pvalidateConditions
    [ ptraceInfoIfFalse "Must mint blacklist cs" $ phasDataCS # blacklistMP # pfromData ptxInfo'mint
    , ptraceInfoIfFalse "Expects spending purpose" $ pisSpendingPurpose (pdata pscriptContext'scriptInfo)
    ]
