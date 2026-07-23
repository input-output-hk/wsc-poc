{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module SmartTokens.LinkedList.SpendDirectory (pmkDirectorySpending, pmkDirectorySpendingYielding, pmkDirectoryGlobalLogic) where

import Plutarch.Core.Context
import Plutarch.Core.ValidationLogic (pvalidateConditions)
import Plutarch.Core.Value (phasDataCS)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import SmartTokens.Types.PTokenDirectory (PDirectorySetNode (PDirectorySetNode, pkey))
import SmartTokens.Types.ProtocolParams

pfindReferenceInputByCS ::
    Term s (PAsData PCurrencySymbol) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PAsData PTxInInfo)
pfindReferenceInputByCS currencySymbol referenceInputs =
    ( pfix #$ plam $ \self remainingRefInputs ->
        let txIn = phead # remainingRefInputs
            resolvedIn = ptxInInfoResolved $ pfromData txIn
         in pif
                (phasDataCS # currencySymbol # pfromData (ptxOutValue resolvedIn))
                txIn
                (self # (ptail # remainingRefInputs))
    )
        # referenceInputs

pmkDirectoryGlobalLogic :: Term s (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
pmkDirectoryGlobalLogic = plam $ \protocolParamsCS ctx -> P.do
    PScriptContext{pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
    PTxInfo{ptxInfo'referenceInputs, ptxInfo'mint} <- pmatch pscriptContext'txInfo
    pmatch pscriptContext'scriptInfo $ \case
        PRewardingScript _ -> P.do
            let paramUTxO =
                    ptxInInfoResolved $
                        pfromData $
                            pfindReferenceInputByCS protocolParamsCS (pfromData ptxInfo'referenceInputs)
            (POutputDatum paramDat') <- pmatch $ ptxOutDatum paramUTxO
            PProgrammableLogicGlobalParams{pdirectoryNodeCS} <- pmatch (pfromData $ punsafeCoerce @(PAsData PProgrammableLogicGlobalParams) (pto paramDat'))
            pvalidateConditions [phasDataCS # pdirectoryNodeCS # pfromData ptxInfo'mint]
        _ -> perror

pmkDirectorySpendingYielding :: Term s (PAsData PCredential :--> PScriptContext :--> PUnit)
pmkDirectorySpendingYielding = plam $ \globalCred ctx -> P.do
    PScriptContext{pscriptContext'txInfo} <- pmatch ctx
    PTxInfo{ptxInfo'wdrl} <- pmatch pscriptContext'txInfo
    let stakeCerts = pfromData ptxInfo'wdrl
        stakeScript = pfromData globalCred
    pmatch (AssocMap.plookup # stakeScript # stakeCerts) $ \case
        PJust _ -> (pconstant ())
        PNothing -> perror

pmkDirectorySpending :: Term s (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
pmkDirectorySpending = plam $ \protocolParamsCS ctx -> P.do
    PScriptContext{pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
    PTxInfo{ptxInfo'referenceInputs, ptxInfo'mint} <- pmatch pscriptContext'txInfo
    mint <- plet $ pfromData ptxInfo'mint
    let paramUTxO =
            ptxInInfoResolved $
                pfromData $
                    pfindReferenceInputByCS protocolParamsCS (pfromData ptxInfo'referenceInputs)

    POutputDatum paramDat' <- pmatch $ ptxOutDatum paramUTxO
    PProgrammableLogicGlobalParams{pdirectoryNodeCS} <- pmatch (pfromData $ punsafeCoerce @(PAsData PProgrammableLogicGlobalParams) (pto paramDat'))

    -- R-01 (Aiken): while a registry node is being spent, the transaction must not
    -- mint or burn that node's own programmable-token policy (its `key`). Decode the
    -- spent node's datum from the spending script info to obtain the key.
    PSpendingScript _ownRef spentDatumM <- pmatch pscriptContext'scriptInfo
    spentNodeKey <- plet $
        pmatch spentDatumM $ \case
            PDJust spentDatumData ->
                pmatch (pfromData $ punsafeCoerce @(PAsData PDirectorySetNode) (pto (pfromData spentDatumData))) $ \case
                    PDirectorySetNode{pkey} -> pkey
            PDNothing -> ptraceInfoError "registry spend: spent node has no datum"

    pvalidateConditions
        [ phasDataCS # pdirectoryNodeCS # mint
        , ptraceInfoIfFalse "registry spend: must not mint/burn the spent node's own key" $
            pnot # (phasDataCS # spentNodeKey # mint)
        ]
