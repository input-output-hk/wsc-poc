{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QualifiedDo          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module SmartTokens.LinkedList.SpendDirectory (pmkDirectorySpending, pmkDirectorySpendingYielding, pmkDirectoryGlobalLogic) where

import Plutarch.Core.Context
import Plutarch.Core.List
import Plutarch.Core.ValidationLogic (pvalidateConditions)
import Plutarch.Core.Value (phasDataCS)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import SmartTokens.Types.ProtocolParams

pmkDirectoryGlobalLogic :: ClosedTerm (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
pmkDirectoryGlobalLogic = plam $ \protocolParamsCS ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
  PTxInfo {ptxInfo'referenceInputs, ptxInfo'mint} <- pmatch pscriptContext'txInfo
  pmatch pscriptContext'scriptInfo $ \case
    PRewardingScript _ -> P.do
      let paramUTxO =
            ptxInInfoResolved $ pfromData $
              pmustFind @PBuiltinList
                # plam (\txIn ->
                        let resolvedIn = ptxInInfoResolved $ pfromData txIn
                        in phasDataCS # protocolParamsCS # pfromData (ptxOutValue resolvedIn)
                      )
                # pfromData ptxInfo'referenceInputs
      (POutputDatum paramDat') <- pmatch $ ptxOutDatum paramUTxO
      PProgrammableLogicGlobalParams {pdirectoryNodeCS} <- pmatch (pfromData $ punsafeCoerce @(PAsData PProgrammableLogicGlobalParams) (pto paramDat'))
      pvalidateConditions [phasDataCS # pdirectoryNodeCS # pfromData ptxInfo'mint]
    _ -> perror

pmkDirectorySpendingYielding :: ClosedTerm (PAsData PCredential :--> PScriptContext :--> PUnit)
pmkDirectorySpendingYielding = plam $ \globalCred ctx -> P.do
  PScriptContext {pscriptContext'txInfo} <- pmatch ctx
  PTxInfo {ptxInfo'wdrl} <- pmatch pscriptContext'txInfo
  let stakeCerts = pfromData ptxInfo'wdrl
      stakeScript = pfromData globalCred
  pmatch (AssocMap.plookup # stakeScript # stakeCerts) $ \case
    PJust _ -> (pconstant ())
    PNothing -> perror

pmkDirectorySpending :: ClosedTerm (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
pmkDirectorySpending = plam $ \protocolParamsCS ctx -> P.do
  PScriptContext {pscriptContext'txInfo} <- pmatch ctx
  PTxInfo {ptxInfo'referenceInputs, ptxInfo'mint} <- pmatch pscriptContext'txInfo
  let paramUTxO =
        ptxInInfoResolved $ pfromData $
          pmustFind @PBuiltinList
            # plam (\txIn ->
                    let resolvedIn = ptxInInfoResolved $ pfromData txIn
                    in phasDataCS # protocolParamsCS # pfromData (ptxOutValue resolvedIn)
                  )
            # pfromData ptxInfo'referenceInputs

  POutputDatum paramDat' <- pmatch $ ptxOutDatum paramUTxO
  PProgrammableLogicGlobalParams {pdirectoryNodeCS} <- pmatch (pfromData $ punsafeCoerce @(PAsData PProgrammableLogicGlobalParams) (pto paramDat'))
  pvalidateConditions [phasDataCS # pdirectoryNodeCS # pfromData ptxInfo'mint]
