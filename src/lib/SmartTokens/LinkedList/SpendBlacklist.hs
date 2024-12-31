{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QualifiedDo          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module SmartTokens.LinkedList.SpendBlacklist (pmkBlacklistSpending) where

import Plutarch.Builtin (pasConstr, pforgetData)
import Plutarch.Core.Utils (phasDataCS, pvalidateConditions)
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptInfo)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

pisSpendingPurpose :: Term s (PAsData PScriptInfo) -> Term s PBool
pisSpendingPurpose term = (pfstBuiltin # (pasConstr # pforgetData term)) #== 1

pmkBlacklistSpending :: ClosedTerm (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
pmkBlacklistSpending = plam $ \blacklistMP ctx -> P.do
  ctxF <- pletFields @'["txInfo", "scriptInfo"] ctx
  infoF <- pletFields @'["mint"] ctxF.txInfo
  pvalidateConditions
    [ ptraceInfoIfFalse "Must mint blacklist cs" $ phasDataCS # blacklistMP # pfromData infoF.mint
    , ptraceInfoIfFalse "Expects spending purpose" $ pisSpendingPurpose ctxF.scriptInfo
    ]
