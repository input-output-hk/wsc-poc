{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SmartTokens.LinkedList.SpendDirectory (pmkDirectorySpending, pmkDirectorySpendingYielding, pmkDirectoryGlobalLogic) where 

import Plutarch.LedgerApi.V3
    ( PCredential,
      PCurrencySymbol,
      POutputDatum(POutputDatum),
      PScriptContext ) 
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
    ( (#),
      (#$),
      perror,
      pto,
      pmatch,
      type (:-->),
      ClosedTerm,
      plam,
      pconstant,
      pfromData,
      pfield,
      pletFields,
      PAsData,
      PBuiltinList,
      PMaybe(PNothing, PJust),
      PUnit )
import Plutarch.Unsafe (punsafeCoerce)
import SmartTokens.Core.Utils (pvalidateConditions, phasDataCS, pmustFind)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
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
                    in phasDataCS # protocolParamsCS # (pfield @"value" # resolvedIn)
                  )
            # infoF.referenceInputs 
  POutputDatum ((pfield @"outputDatum" #) -> paramDat') <- pmatch $ pfield @"datum" # paramUTxO
  protocolParamsF <- pletFields @'["directoryNodeCS"] (pfromData $ punsafeCoerce @_ @_ @(PAsData PProgrammableLogicGlobalParams) (pto paramDat'))
  pvalidateConditions [phasDataCS # protocolParamsF.directoryNodeCS # pfromData infoF.mint]