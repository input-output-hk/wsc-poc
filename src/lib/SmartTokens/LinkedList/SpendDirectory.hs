{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QualifiedDo          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module SmartTokens.LinkedList.SpendDirectory (pmkDirectorySpending, pmkDirectorySpendingYielding, pmkDirectoryGlobalLogic) where

import Plutarch.Core.Utils (phasDataCS, pmustFind, pvalidateConditions)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3 (PCredential, PCurrencySymbol,
                              POutputDatum (POutputDatum), PScriptContext)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude (ClosedTerm, PAsData, PBuiltinList,
                         PMaybe (PJust, PNothing), PUnit, pconstant, perror,
                         pfield, pfromData, plam, pletFields, pmatch, pto,
                         type (:-->), (#$), (#))
import Plutarch.Trace
import Plutarch.Unsafe (punsafeCoerce)
import SmartTokens.Types.ProtocolParams (PProgrammableLogicGlobalParams)

pmkDirectoryGlobalLogic :: ClosedTerm (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
pmkDirectoryGlobalLogic = plam $ \protocolParamsCS ctx -> P.do
  ctxF <- pletFields @'["txInfo", "scriptInfo"] ctx
  infoF <- pletFields @'["referenceInputs", "mint"] ctxF.txInfo
  let paramUTxO =
        pfield @"resolved" #$
          pmustFind @PBuiltinList
            # plam (\txIn ->
                    let resolvedIn = pfield @"resolved" # txIn
                    in phasDataCS # protocolParamsCS # (pfield @"value" # resolvedIn)
                  )
            # infoF.referenceInputs
  POutputDatum ((pfield @"outputDatum" #) -> paramDat') <- pmatch $ pfield @"datum" # paramUTxO
  protocolParamsF <- pletFields @'["directoryNodeCS"] (pfromData $ punsafeCoerce @_ @_ @(PAsData PProgrammableLogicGlobalParams) (pto paramDat'))
  pvalidateConditions [phasDataCS # protocolParamsF.directoryNodeCS # pfromData infoF.mint]

pmkDirectorySpendingYielding ::
  ClosedTerm (PAsData PCredential :--> PScriptContext :--> PUnit)
pmkDirectorySpendingYielding = plam $ \globalCred ctx -> P.do
  ctxF <- pletFields @'["txInfo"] ctx
  let stakeCerts = pfield @"wdrl" # ctxF.txInfo
      stakeScript = pfromData globalCred
  pmatch (AssocMap.plookup # stakeScript # stakeCerts) $ \case
    PJust _ -> (pconstant ())
    PNothing -> perror

pmkDirectorySpending ::
  ClosedTerm (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
pmkDirectorySpending = plam $ \protocolParamsCS ctx -> P.do
  ctxF <- pletFields @'["txInfo", "scriptInfo"] ctx
  infoF <- pletFields @'["referenceInputs", "mint"] ctxF.txInfo
  let paramUTxO =
        pfield @"resolved" #$
          pmustFind @PBuiltinList
            # plam (\txIn ->
                    let resolvedIn = pfield @"resolved" # txIn
                    in phasDataCS # protocolParamsCS # pfromData (pfield @"value" # resolvedIn)
                  )
            # pfromData infoF.referenceInputs
  POutputDatum ((pfield @"outputDatum" #) -> paramDat') <- pmatch $ pfield @"datum" # paramUTxO
  protocolParamsF <- pletFields @'["directoryNodeCS"] (pfromData $ punsafeCoerce @_ @_ @(PAsData PProgrammableLogicGlobalParams) (pto paramDat'))
  pvalidateConditions [ptraceInfoIfFalse "cannot find directory CS" $ phasDataCS # protocolParamsF.directoryNodeCS # pfromData infoF.mint]
