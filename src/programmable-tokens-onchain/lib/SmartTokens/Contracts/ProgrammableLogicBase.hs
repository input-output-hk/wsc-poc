{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module SmartTokens.Contracts.ProgrammableLogicBase (
    TokenProof (..),
    ProgrammableLogicGlobalRedeemer (..),
    absoluteToRelativeInputIdxs,
    mkSeizeActRedeemerFromAbsoluteInputIdxs,
    mkSeizeActRedeemerFromRelativeInputIdxs,
    mkProgrammableLogicBase,
    mkProgrammableLogicGlobal,
    pisScriptInvokedEntries,
    pvalueFromCred,
    pvalueToCred,
    poutputsContainExpectedValueAtCred,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Integer (pconstantInteger)
import Plutarch.Core.Context (
    paddressCredential,
    pscriptContextTxInfo,
    ptxInInfoResolved,
    ptxOutDatum,
    ptxOutValue,
 )
import Plutarch.Core.Integrity (pisRewardingScript)
import Plutarch.Core.Internal.Builtins (pmapData, ppairDataBuiltinRaw)
import Plutarch.Core.List
import Plutarch.Core.Utils
import Plutarch.Core.ValidationLogic hiding (pemptyLedgerValue, pvalueFromCred, pvalueToCred)
import Plutarch.Core.Value
import Plutarch.Internal.Lift
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1.Value (Value)
import PlutusTx qualified
import SmartTokens.Types.PTokenDirectory (PDirectorySetNode (..))
import SmartTokens.Types.ProtocolParams (PProgrammableLogicGlobalParams (..))

pjustData :: Term s (PMaybeData a) -> Term s a
pjustData term =
    punsafeCoerce $ phead # (psndBuiltin # (pasConstr # pforgetData (pdata term)))

-- TODO: Replace current corresponding input / output comparison (which compares address, reference script and datum) for multi-seize
-- with constructing the expected output from the input with this function and comparing it to the actual output.
-- Further optimize this with the optimization in the "Everything is possible" UPLC fest presentation.
-- pconstructExpectedOutputWithOutputDatum :: Term s PAddress -> Term s (PAsData (PValue 'Sorted 'Positive)) -> Term s POutputDatum -> Term s (PAsData PTxOut)
-- pconstructExpectedOutputWithOutputDatum address value datum =
--   pdata $ pcon $
--     PTxOut
--      { ptxOut'address = address
--      , ptxOut'value = value
--      , ptxOut'datum = datum
--      , ptxOut'referenceScript = pconstant Nothing
--      }

-- TODO:
-- The current implementation of the contracts in this module are not designed to be maximally efficient.
-- In the future, this should be optimized to use the redeemer indexing design pattern to not just index the directory nodes in the reference inputs,
-- but also to index the programmable inputs and outputs.
data TokenProof
    = TokenExists Integer
    | TokenDoesNotExist Integer
    deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
    ''TokenProof
    [('TokenExists, 0), ('TokenDoesNotExist, 1)]

data PTokenProof (s :: S)
    = PTokenExists {pnodeIdx :: Term s (PAsData PInteger)}
    | PTokenDoesNotExist {pnodeIdx :: Term s (PAsData PInteger)}
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PTokenProof)

deriving via
    DeriveDataPLiftable PTokenProof TokenProof
    instance
        PLiftable PTokenProof

emptyValue :: Value
emptyValue = mempty

pemptyLedgerValue :: Term s (PValue 'Sorted 'Positive)
pemptyLedgerValue = punsafeCoerce $ pconstant @(PValue 'Unsorted 'NoGuarantees) emptyValue

{- | Strip Ada from a ledger value
Importantly this function assumes that the Value is provided by the ledger
(i.e. via the ScriptContext), so Ada is the first entry.
-}
pstripAdaH ::
    forall (v :: AmountGuarantees) (s :: S).
    Term s (PValue 'Sorted v) -> Term s (PValue 'Sorted v)
pstripAdaH value =
    let nonAdaValueMapInner = ptail # pto (pto value)
     in pcon (PValue $ pcon $ PMap nonAdaValueMapInner)

ptokenPairsUnionFast ::
    Term
        s
        ( PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
            :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
            :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
        )
ptokenPairsUnionFast = phoistAcyclic $
    pfix #$ plam $ \self tokensA tokensB ->
        pelimList
            ( \tokenPairA tokensARest ->
                pelimList
                    ( \tokenPairB tokensBRest ->
                        let tokenNameA = pfstBuiltin # tokenPairA
                            tokenNameB = pfstBuiltin # tokenPairB
                            tokenNameABytes = pasByteStr # pforgetData tokenNameA
                            tokenNameBBytes = pasByteStr # pforgetData tokenNameB
                         in pif
                                (tokenNameABytes #== tokenNameBBytes)
                                ( let quantityA = pfromData (psndBuiltin # tokenPairA)
                                      quantityB = pfromData (psndBuiltin # tokenPairB)
                                   in pcons
                                        # (ppairDataBuiltin # tokenNameA # pdata (quantityA + quantityB))
                                        # (self # tokensARest # tokensBRest)
                                )
                                ( pif
                                    (tokenNameABytes #< tokenNameBBytes)
                                    (pcons # tokenPairA # (self # tokensARest # tokensB))
                                    (pcons # tokenPairB # (self # tokensA # tokensBRest))
                                )
                    )
                    tokensA
                    tokensB
            )
            tokensB
            tokensA

pcurrencyPairsUnionFast ::
    Term
        s
        ( PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))
            :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))
            :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))
        )
pcurrencyPairsUnionFast = phoistAcyclic $
    pfix #$ plam $ \self csPairsA csPairsB ->
        pelimList
            ( \csPairA csPairsARest ->
                pelimList
                    ( \csPairB csPairsBRest ->
                        let currencySymbolA = pfstBuiltin # csPairA
                            currencySymbolB = pfstBuiltin # csPairB
                            currencySymbolABytes = pasByteStr # pforgetData currencySymbolA
                            currencySymbolBBytes = pasByteStr # pforgetData currencySymbolB
                         in pif
                                (currencySymbolABytes #== currencySymbolBBytes)
                                ( let tokenPairsA = pto (pfromData (psndBuiltin # csPairA))
                                      tokenPairsB = pto (pfromData (psndBuiltin # csPairB))
                                      mergedTokenPairs = ptokenPairsUnionFast # tokenPairsA # tokenPairsB
                                      mergedPair =
                                        punsafeCoerce $
                                            ppairDataBuiltinRaw
                                                # pforgetData currencySymbolA
                                                # (pmapData # punsafeCoerce mergedTokenPairs)
                                   in pcons
                                        # mergedPair
                                        # (self # csPairsARest # csPairsBRest)
                                )
                                ( pif
                                    (currencySymbolABytes #< currencySymbolBBytes)
                                    (pcons # csPairA # (self # csPairsARest # csPairsB))
                                    (pcons # csPairB # (self # csPairsA # csPairsBRest))
                                )
                    )
                    csPairsA
                    csPairsB
            )
            csPairsB
            csPairsA

pvalueUnionFast :: Term s (PValue 'Sorted 'Positive :--> PValue 'Sorted 'Positive :--> PValue 'Sorted 'Positive)
pvalueUnionFast = phoistAcyclic $ plam $ \valueA valueB ->
    pcon $
        PValue $
            pcon $
                PMap $
                    pcurrencyPairsUnionFast
                        # pto (pto valueA)
                        # pto (pto valueB)

pisScriptInvokedEntries :: Term s (PAsData PCredential :--> PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace)) :--> PBool)
pisScriptInvokedEntries = phoistAcyclic $ plam $ \scriptCredData withdrawalEntries ->
    let go = pfix #$ plam $ \self entries ->
            let entry = phead # entries
             in (pfstBuiltin # entry)
                    #== scriptCredData
                    #|| plet
                        (ptail # entries)
                        ( \entries' ->
                            let entryA = phead # entries'
                             in (pfstBuiltin # entryA) #== scriptCredData #|| self # entries'
                        )
     in go # withdrawalEntries

pvalueFromCred ::
    Term s PCredential ->
    Term s (PBuiltinList (PAsData PPubKeyHash)) ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace))) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PValue 'Sorted 'Positive)
pvalueFromCred cred sigs withdrawalEntries inputs =
    let credData = pforgetData (pdata cred)
     in ( pfix #$ plam $ \self acc ->
            pelimList
                ( \txIn xs ->
                    plet (pdata (ptxInInfoResolved $ pfromData txIn)) $ \resolvedOutData ->
                        plet (psndBuiltin # (pasConstr # pforgetData resolvedOutData)) $ \resolvedOutFields ->
                            let resolvedOutAddressData = phead # resolvedOutFields
                                resolvedOutFieldsRest = ptail # resolvedOutFields
                                resolvedOutValue = punsafeCoerce @(PAsData (PValue 'Sorted 'Positive)) (phead # resolvedOutFieldsRest)
                                paymentCredData = phead # (psndBuiltin # (pasConstr # resolvedOutAddressData))
                                stakingCredMaybe = punsafeCoerce @(PMaybeData PStakingCredential) (phead # (ptail # (psndBuiltin # (pasConstr # resolvedOutAddressData))))
                             in self
                                    # pif
                                        (paymentCredData #== credData)
                                        ( pmatch (pjustData stakingCredMaybe) $ \case
                                            PStakingHash ownerCred ->
                                                pmatch ownerCred $ \case
                                                    PPubKeyCredential pkh ->
                                                        pif
                                                            (ptxSignedByPkh # pkh # sigs)
                                                            (pvalueUnionFast # acc # pstripAdaH (pfromData resolvedOutValue))
                                                            (ptraceInfoError "Missing required pk witness")
                                                    PScriptCredential scriptHash_ ->
                                                        let scriptCredData = pdata $ pcon (PScriptCredential scriptHash_)
                                                         in pif
                                                                (pisScriptInvokedEntries # scriptCredData # withdrawalEntries)
                                                                (pvalueUnionFast # acc # pstripAdaH (pfromData resolvedOutValue))
                                                                (ptraceInfoError "Missing required script witness")
                                            _ -> perror
                                        )
                                        acc
                                    # xs
                )
                acc
        )
            # pemptyLedgerValue
            # inputs

pvalueToCred ::
    Term s PCredential ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s (PValue 'Sorted 'Positive)
pvalueToCred cred inputs =
    let credData = pforgetData (pdata cred)
     in ( pfix #$ plam $ \self acc ->
            pelimList
                ( \txOut xs ->
                    plet (psndBuiltin # (pasConstr # pforgetData txOut)) $ \txOutFields ->
                        let txOutAddress = phead # txOutFields
                            txOutFieldsRest = ptail # txOutFields
                            txOutValue = punsafeCoerce @(PAsData (PValue 'Sorted 'Positive)) (phead # txOutFieldsRest)
                            paymentCredData = phead # (psndBuiltin # (pasConstr # txOutAddress))
                         in self
                                # pif (paymentCredData #== credData) (pvalueUnionFast # acc # pstripAdaH (pfromData txOutValue)) acc
                                # xs
                )
                acc
        )
            # pemptyLedgerValue
            # inputs

{- | Check that outputs whose payment credential equals `progLogicCred` contain at
least the caller-supplied `expectedValue`.

This is a lower-bound containment check, not an equality check:

- Extra assets at mini-ledger outputs are allowed.
- Stake credentials, datums and reference scripts are ignored.
- Outputs are grouped by payment credential only, because mini-ledger ownership is
  carried by the staking credential while the payment credential is the shared
  programmable-logic-base script.

The caller is responsible for supplying the correct `expectedValue`. In the
`TransferAct` path this is the validated programmable input value, adjusted by the
validated programmable mint/burn value. This helper does /not/ query the registry
and does /not/ detect pre-existing programmable tokens already outside the
mini-ledger; it only proves that the specific value passed in still remains at
`progLogicCred` outputs.

Implementation note: if `expectedValue` contains exactly one non-Ada asset, the
function takes a single-asset fast path that scans outputs and accumulates only that
asset quantity. Otherwise it aggregates the full non-Ada value at `progLogicCred`
outputs and checks each expected asset against that aggregate.
-}
poutputsContainExpectedValueAtCred ::
    Term s PCredential ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s (PValue 'Sorted 'Positive) ->
    Term s PBool
poutputsContainExpectedValueAtCred progLogicCred txOutputs expectedValue =
    let
        passetQtyInValue = phoistAcyclic $ plam $ \value cs tn ->
            let tokenQtyInTokenPairs = pfix #$ plam $ \self remainingTokenPairs ->
                    pelimList
                        ( \tokenPair tokenPairsRest ->
                            let tokenName = pfromData (pfstBuiltin # tokenPair)
                                tokenQty = pfromData (psndBuiltin # tokenPair)
                             in pif
                                    (tokenName #== tn)
                                    tokenQty
                                    ( pif
                                        (tn #< tokenName)
                                        0
                                        (self # tokenPairsRest)
                                    )
                        )
                        0
                        remainingTokenPairs
                tokenQtyInCurrencyPairs = pfix #$ plam $ \self remainingCurrencyPairs ->
                    pelimList
                        ( \currencyPair currencyPairsRest ->
                            let currencySymbol = pfromData (pfstBuiltin # currencyPair)
                                tokenPairs = pto (pfromData (psndBuiltin # currencyPair))
                             in pif
                                    (currencySymbol #== cs)
                                    (tokenQtyInTokenPairs # tokenPairs)
                                    ( pif
                                        (cs #< currencySymbol)
                                        0
                                        (self # currencyPairsRest)
                                    )
                        )
                        0
                        remainingCurrencyPairs
             in tokenQtyInCurrencyPairs # pto (pto value)
        hasAtLeastAssetInProgOutputs = pfix #$ plam $ \self requiredQty currentQty cs tn remainingOutputs ->
            pif
                (currentQty #>= requiredQty)
                (pconstant True)
                ( pelimList
                    ( \txOut outputsRest ->
                        pmatch (pfromData txOut) $ \(PTxOut{ptxOut'address, ptxOut'value}) ->
                            pif
                                (paddressCredential ptxOut'address #== progLogicCred)
                                (self # requiredQty # (currentQty + (passetQtyInValue # (pfromData ptxOut'value) # cs # tn)) # cs # tn # outputsRest)
                                (self # requiredQty # currentQty # cs # tn # outputsRest)
                    )
                    (currentQty #>= requiredQty)
                    remainingOutputs
                )
        checkExpectedTokenPairsAgainstActualValue = pfix #$ plam $ \self actualValue expectedCurrencySymbol remainingExpectedTokenPairs ->
            pelimList
                ( \expectedTokenPair expectedTokenPairsRest ->
                    let expectedTokenName = pfromData (pfstBuiltin # expectedTokenPair)
                        expectedTokenQty = pfromData (psndBuiltin # expectedTokenPair)
                     in (passetQtyInValue # actualValue # expectedCurrencySymbol # expectedTokenName #>= expectedTokenQty)
                            #&& self
                            # actualValue
                            # expectedCurrencySymbol
                            # expectedTokenPairsRest
                )
                (pconstant True)
                remainingExpectedTokenPairs
        checkExpectedCurrencyPairsAgainstActualValue = pfix #$ plam $ \self actualValue remainingExpectedCurrencyPairs ->
            pelimList
                ( \expectedCurrencyPair expectedCurrencyPairsRest ->
                    let expectedCurrencySymbol = pfromData (pfstBuiltin # expectedCurrencyPair)
                        expectedTokenPairs = pto (pfromData (psndBuiltin # expectedCurrencyPair))
                     in checkExpectedTokenPairsAgainstActualValue
                            # actualValue
                            # expectedCurrencySymbol
                            # expectedTokenPairs
                            #&& self
                            # actualValue
                            # expectedCurrencyPairsRest
                )
                (pconstant True)
                remainingExpectedCurrencyPairs
        expectedCsPairs = pto (pto expectedValue)
        actualValueAtCred = pvalueToCred progLogicCred txOutputs
     in
        pelimList
            ( \firstExpectedCsPair expectedCsPairsRest ->
                let firstExpectedTokenPairs = pto (pfromData (psndBuiltin # firstExpectedCsPair))
                 in pelimList
                        ( \expectedTokenPair firstExpectedTokenPairsRest ->
                            pif
                                (pnull # expectedCsPairsRest #&& pnull # firstExpectedTokenPairsRest)
                                ( let expectedCurrencySymbol = pfromData (pfstBuiltin # firstExpectedCsPair)
                                      expectedTokenName = pfromData (pfstBuiltin # expectedTokenPair)
                                      expectedRequiredQty = pfromData (psndBuiltin # expectedTokenPair)
                                   in hasAtLeastAssetInProgOutputs
                                        # expectedRequiredQty
                                        # 0
                                        # expectedCurrencySymbol
                                        # expectedTokenName
                                        # txOutputs
                                )
                                (checkExpectedCurrencyPairsAgainstActualValue # actualValueAtCred # expectedCsPairs)
                        )
                        (checkExpectedCurrencyPairsAgainstActualValue # actualValueAtCred # expectedCsPairs)
                        firstExpectedTokenPairs
            )
            (pconstant True)
            expectedCsPairs

{- | Programmable logic base
This validator forwards its validation logic to the programmable logic stake script
using the withdraw-zero design pattern.
-}
mkProgrammableLogicBase :: Term s (PAsData PCredential :--> PScriptContext :--> PUnit)
mkProgrammableLogicBase = plam $ \stakeCred ctx ->
    pmatch (pscriptContextTxInfo ctx) $ \txInfo ->
        let wdrls :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace)))
            wdrls = pto $ pfromData $ ptxInfo'wdrl txInfo
         in plet wdrls $ \withdrawals ->
                let firstWithdrawal :: Term _ (PAsData PCredential)
                    firstWithdrawal = pfstBuiltin # (phead @PBuiltinList # withdrawals)
                    hasCred =
                        (firstWithdrawal #== stakeCred)
                            #|| let go = pfix #$ plam $ \self withdrawals' ->
                                        let withdrawal = phead # withdrawals'
                                         in (pfstBuiltin # withdrawal)
                                                #== stakeCred
                                                #|| plet
                                                    (ptail # withdrawals')
                                                    ( \withdrawals'' ->
                                                        let withdrawalA = phead # withdrawals''
                                                         in (pfstBuiltin # withdrawalA) #== stakeCred #|| self # (ptail # withdrawals'')
                                                    )
                                 in go # (ptail # withdrawals)
                 in pvalidateConditions [ptraceInfoIfFalse "programmable global not invoked" hasCred]

phasCSH :: Term s PCurrencySymbol -> Term s (PAsData (PValue 'Sorted 'Positive)) -> Term s PBool
phasCSH directoryNodeCS value =
    let value' = pto (pto (pfromData value))
     in pfromData (pfstBuiltin # (phead # (ptail # value'))) #== directoryNodeCS

pfindReferenceInputByCS ::
    Term s PCurrencySymbol ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s PProgrammableLogicGlobalParams
pfindReferenceInputByCS currencySymbol referenceInputs =
    let extractParams resolvedOut =
            pmatch (ptxOutDatum resolvedOut) $ \case
                POutputDatum paramDat' ->
                    pfromData $ punsafeCoerce @(PAsData PProgrammableLogicGlobalParams) (pto paramDat')
                _ -> ptraceInfoError "protocol params datum missing"
        firstRefInput = phead # referenceInputs
        go = pfix #$ plam $ \self remainingRefInputs ->
            let txIn = phead # remainingRefInputs
             in plet (ptxInInfoResolved $ pfromData txIn) $ \resolvedOut ->
                    pif
                        (phasCSH currencySymbol (ptxOutValue resolvedOut))
                        (extractParams resolvedOut)
                        (self # (ptail # remainingRefInputs))
     in plet (ptxInInfoResolved $ pfromData firstRefInput) $ \firstResolvedOut ->
            pif
                (phasCSH currencySymbol (ptxOutValue firstResolvedOut))
                (extractParams firstResolvedOut)
                (go # (ptail # referenceInputs))

{- | Traverse the currency symbols of the combined Ada-stripped value of all programmable base inputs.
For each currency symbol, we check a proof that either:
1. The currency symbol is in the directory (and thus is a programmable token)
     - given that it is a programmable token, we check that associated transfer logic script is executed in the transaction
       and add the value entry to the result.
2. The currency symbol is not in the directory.
Return a Value containing only programmable tokens from `totalValue` by filtering out the non-programmable token entries.
-}
pcheckTransferLogicAndGetProgrammableValue ::
    Term s PCurrencySymbol ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PTokenProof)) ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace))) ->
    Term s (PAsData PCredential) ->
    Term s (PValue 'Sorted 'Positive) ->
    Term s (PValue 'Sorted 'Positive)
pcheckTransferLogicAndGetProgrammableValue directoryNodeCS refInputs proofList withdrawalEntries initialCachedTransferScript totalValue =
    plet (pelemAtFast @PBuiltinList # refInputs) $ \patRefUTxOIdx ->
        let mapInnerList :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger))))
            mapInnerList = pto (pto totalValue)
            -- Cache transfer-script invocation checks across adjacent TokenExists proofs.
            go = pfix #$ plam $ \self proofs inputInnerValue actualProgrammableTokenValue cachedTransferScript ->
                pelimList
                    ( \csPair csPairs ->
                        pmatch (pfromData $ phead # proofs) $ \case
                            PTokenExists nodeIdx -> P.do
                                PTxOut{ptxOut'value = directoryNodeUTxOFValue, ptxOut'datum = directoryNodeUTxOFDatum} <- pmatch $ ptxInInfoResolved (pfromData $ patRefUTxOIdx # pfromData nodeIdx)
                                POutputDatum paramDat' <- pmatch directoryNodeUTxOFDatum
                                PDirectorySetNode{pkey = directoryNodeDatumFkey, ptransferLogicScript = directoryNodeDatumFTransferLogicScript} <- pmatch (pfromData $ punsafeCoerce @(PAsData PDirectorySetNode) (pto paramDat'))
                                -- validate that the directory entry for the currency symbol is referenced by the proof
                                -- and that the associated transfer logic script is executed in the transaction
                                let checks =
                                        pand'List
                                            [ ptraceInfoIfFalse "Missing required transfer script" $
                                                (directoryNodeDatumFTransferLogicScript #== cachedTransferScript)
                                                    #|| (pisScriptInvokedEntries # directoryNodeDatumFTransferLogicScript # withdrawalEntries)
                                            , ptraceInfoIfFalse "directory proof mismatch" (directoryNodeDatumFkey #== (pfstBuiltin # csPair))
                                            , ptraceInfoIfFalse "invalid dir node" (phasCSH directoryNodeCS directoryNodeUTxOFValue)
                                            ]
                                pif
                                    checks
                                    ( self
                                        # (ptail # proofs)
                                        # csPairs
                                        # (pcons # csPair # actualProgrammableTokenValue)
                                        # directoryNodeDatumFTransferLogicScript
                                    )
                                    perror
                            PTokenDoesNotExist notExistNodeIdx -> P.do
                                PTxOut{ptxOut'value = prevNodeUTxOValue, ptxOut'datum = prevNodeUTxODatum} <- pmatch $ ptxInInfoResolved (pfromData $ patRefUTxOIdx # pfromData notExistNodeIdx)
                                POutputDatum prevNodeDat' <- pmatch prevNodeUTxODatum
                                PDirectorySetNode{pkey = nodeDatumKey, pnext = nodeDatumNext} <- pmatch (pfromData $ punsafeCoerce @(PAsData PDirectorySetNode) (pto prevNodeDat'))
                                currCS <- plet $ pasByteStr # pforgetData (pfstBuiltin # csPair)
                                nodeKey <- plet $ pasByteStr # pforgetData nodeDatumKey
                                let nodeNext = pasByteStr # pforgetData nodeDatumNext
                                    checks =
                                        pand'List
                                            [ -- the currency symbol is not in the directory
                                              ptraceInfoIfFalse "dir neg-proof node must cover" $ nodeKey #< currCS
                                            , ptraceInfoIfFalse "dir neg-proof node must cover" $ currCS #< nodeNext
                                            , -- both directory entries are legitimate, this is proven by the
                                              -- presence of the directory node currency symbol.
                                              ptraceInfoIfFalse "invalid dir node n" $ phasCSH directoryNodeCS prevNodeUTxOValue
                                            ]
                                pif
                                    checks
                                    ( self
                                        # (ptail # proofs)
                                        # csPairs
                                        # actualProgrammableTokenValue
                                        # cachedTransferScript
                                    )
                                    perror
                    )
                    (pcon $ PValue $ pcon $ PMap actualProgrammableTokenValue)
                    inputInnerValue
         in go
                # proofList
                # mapInnerList
                # pto (pto pemptyLedgerValue)
                # initialCachedTransferScript

{- | Traverse the minted value and validate one directory proof per minted currency symbol.
Returns a signed value containing only programmable currency symbols from the tx mint field.
Proof list must be exactly aligned with the minted currency-symbol list (lockstep).
-}
pcheckMintLogicAndGetProgrammableValue ::
    Term s PCurrencySymbol ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PTokenProof)) ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace))) ->
    Term s (PValue 'Sorted 'NoGuarantees) ->
    Term s (PValue 'Sorted 'NoGuarantees)
pcheckMintLogicAndGetProgrammableValue directoryNodeCS refInputs proofList withdrawalEntries totalMintValue =
    plet (pelemAtFast @PBuiltinList # refInputs) $ \patRefUTxOIdx ->
        let mintedEntries :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger))))
            mintedEntries = pto (pto totalMintValue)
            go = pfix #$ plam $ \self proofs remainingMintEntries programmableMintValue ->
                pelimList
                    ( \mintCsPair mintCsPairs ->
                        pelimList
                            ( \proof proofsRest ->
                                let mintCs = pfstBuiltin # mintCsPair
                                 in pmatch (pfromData proof) $ \case
                                        PTokenExists nodeIdx -> P.do
                                            PTxOut{ptxOut'value = directoryNodeUTxOFValue, ptxOut'datum = directoryNodeUTxOFDatum} <- pmatch $ ptxInInfoResolved (pfromData $ patRefUTxOIdx # pfromData nodeIdx)
                                            POutputDatum paramDat' <- pmatch directoryNodeUTxOFDatum
                                            PDirectorySetNode{pkey = directoryNodeDatumFkey, ptransferLogicScript = directoryNodeDatumFTransferLogicScript} <- pmatch (pfromData $ punsafeCoerce @(PAsData PDirectorySetNode) (pto paramDat'))
                                            let checks =
                                                    pand'List
                                                        [ ptraceInfoIfFalse "Missing required transfer script" (pisScriptInvokedEntries # directoryNodeDatumFTransferLogicScript # withdrawalEntries)
                                                        , ptraceInfoIfFalse "directory mint proof mismatch" (directoryNodeDatumFkey #== mintCs)
                                                        , ptraceInfoIfFalse "invalid dir node m" (phasCSH directoryNodeCS directoryNodeUTxOFValue)
                                                        ]
                                            pif
                                                checks
                                                (self # proofsRest # mintCsPairs # (pcons # mintCsPair # programmableMintValue))
                                                perror
                                        PTokenDoesNotExist notExistNodeIdx -> P.do
                                            PTxOut{ptxOut'value = prevNodeUTxOValue, ptxOut'datum = prevNodeUTxODatum} <- pmatch $ ptxInInfoResolved (pfromData $ patRefUTxOIdx # pfromData notExistNodeIdx)
                                            POutputDatum prevNodeDat' <- pmatch prevNodeUTxODatum
                                            PDirectorySetNode{pkey = nodeDatumKey, pnext = nodeDatumNext} <- pmatch (pfromData $ punsafeCoerce @(PAsData PDirectorySetNode) (pto prevNodeDat'))
                                            let currCS = pfromData mintCs
                                            let nodeKey = pfromData nodeDatumKey
                                            let nodeNext = pfromData nodeDatumNext
                                            let checks =
                                                    pand'List
                                                        [ ptraceInfoIfFalse "dir mint neg-proof node must cover" (nodeKey #< currCS)
                                                        , ptraceInfoIfFalse "dir mint neg-proof node must cover" (currCS #< nodeNext)
                                                        , ptraceInfoIfFalse "invalid dir node n" (phasCSH directoryNodeCS prevNodeUTxOValue)
                                                        ]
                                            pif
                                                checks
                                                (self # proofsRest # mintCsPairs # programmableMintValue)
                                                perror
                            )
                            (ptraceInfoError "mint proof missing")
                            proofs
                    )
                    (pelimList (\_ _ -> ptraceInfoError "extra mint proof") (pcon $ PValue $ pcon $ PMap programmableMintValue) proofs)
                    remainingMintEntries
         in go # proofList # mintedEntries # pnil

data ProgrammableLogicGlobalRedeemer
    = TransferAct
        { plgrTransferProofs :: [TokenProof]
        , plgrMintProofs :: [TokenProof]
        }
    | SeizeAct
        { plgrDirectoryNodeIdx :: Integer
        , plgrInputIdxs :: [Integer]
        , plgrOutputsStartIdx :: Integer
        , plgrLengthInputIdxs :: Integer
        }
    deriving (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
    ''ProgrammableLogicGlobalRedeemer
    [('TransferAct, 0), ('SeizeAct, 1)]

{- | Convert absolute tx-input indexes to seize relative indexes.
Requires a strictly increasing non-negative list of absolute indexes.
-}
absoluteToRelativeInputIdxs :: [Integer] -> [Integer]
absoluteToRelativeInputIdxs [] = []
absoluteToRelativeInputIdxs (firstAbsIdx : remainingAbsIdxs)
    | firstAbsIdx < 0 = error "absoluteToRelativeInputIdxs: negative absolute index"
    | otherwise = firstAbsIdx : go firstAbsIdx remainingAbsIdxs
  where
    go :: Integer -> [Integer] -> [Integer]
    go _ [] = []
    go previousAbsIdx (currentAbsIdx : restAbsIdxs)
        | currentAbsIdx <= previousAbsIdx = error "absoluteToRelativeInputIdxs: absolute indexes must be strictly increasing"
        | otherwise = (currentAbsIdx - previousAbsIdx - 1) : go currentAbsIdx restAbsIdxs

-- | Construct a SeizeAct redeemer from relative indexes.
mkSeizeActRedeemerFromRelativeInputIdxs :: Integer -> [Integer] -> Integer -> ProgrammableLogicGlobalRedeemer
mkSeizeActRedeemerFromRelativeInputIdxs directoryNodeIdx relativeInputIdxs outputsStartIdx
    | any (< 0) relativeInputIdxs = error "mkSeizeActRedeemerFromRelativeInputIdxs: negative relative index"
    | otherwise =
        SeizeAct
            { plgrDirectoryNodeIdx = directoryNodeIdx
            , plgrInputIdxs = relativeInputIdxs
            , plgrOutputsStartIdx = outputsStartIdx
            , plgrLengthInputIdxs = fromIntegral (length relativeInputIdxs)
            }

-- | Construct a SeizeAct redeemer from absolute indexes.
mkSeizeActRedeemerFromAbsoluteInputIdxs :: Integer -> [Integer] -> Integer -> ProgrammableLogicGlobalRedeemer
mkSeizeActRedeemerFromAbsoluteInputIdxs directoryNodeIdx absoluteInputIdxs =
    mkSeizeActRedeemerFromRelativeInputIdxs
        directoryNodeIdx
        (absoluteToRelativeInputIdxs absoluteInputIdxs)

data PProgrammableLogicGlobalRedeemer (s :: S)
    = PTransferAct
        { ptransferProofs :: Term s (PAsData (PBuiltinList (PAsData PTokenProof)))
        , pmintProofs :: Term s (PAsData (PBuiltinList (PAsData PTokenProof)))
        }
    | -- The proofs validate currency-symbol membership against the directory:
      -- ptransferProofs correspond to programmable input value entries.
      -- pmintProofs correspond to tx mint currency-symbol entries.
      PSeizeAct
        { pdirectoryNodeIdx :: Term s (PAsData PInteger)
        , pinputIdxs :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
        , poutputsStartIdx :: Term s (PAsData PInteger)
        , plengthInputIdxs :: Term s (PAsData PInteger)
        }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PProgrammableLogicGlobalRedeemer)

deriving via
    DeriveDataPLiftable PProgrammableLogicGlobalRedeemer ProgrammableLogicGlobalRedeemer
    instance
        PLiftable PProgrammableLogicGlobalRedeemer

mkProgrammableLogicGlobal :: Term s (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
mkProgrammableLogicGlobal = plam $ \protocolParamsCS ctx -> P.do
    PScriptContext{pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <- pmatch ctx
    PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'signatories, ptxInfo'wdrl, ptxInfo'mint, ptxInfo'redeemers} <- pmatch pscriptContext'txInfo
    let red = pfromData $ punsafeCoerce @(PAsData PProgrammableLogicGlobalRedeemer) (pto pscriptContext'redeemer)
    referenceInputs <- plet $ pfromData ptxInfo'referenceInputs

    -- Extract protocol parameter UTxO
    ptraceInfo "Extracting protocol parameter UTxO"

    PProgrammableLogicGlobalParams{pdirectoryNodeCS, pprogLogicCred} <-
        pmatch $
            pfindReferenceInputByCS (pfromData protocolParamsCS) referenceInputs
    progLogicCred <- plet $ pfromData pprogLogicCred

    ptraceInfo "Extracting invoked scripts"
    withdrawalEntries <- plet $ pto (pfromData ptxInfo'wdrl)

    pmatch red $ \case
        PTransferAct transferProofs mintProofs -> P.do
            cachedTransferScript0 <- plet $ pfstBuiltin # (phead @PBuiltinList # withdrawalEntries)
            totalProgTokenValue <-
                plet $
                    pvalueFromCred
                        progLogicCred
                        (pfromData ptxInfo'signatories)
                        withdrawalEntries
                        (pfromData ptxInfo'inputs)
            totalProgTokenValue_ <-
                plet $
                    pcheckTransferLogicAndGetProgrammableValue
                        (pfromData pdirectoryNodeCS)
                        referenceInputs
                        (pfromData transferProofs)
                        withdrawalEntries
                        cachedTransferScript0
                        totalProgTokenValue
            mintValueNoGuarantees <- plet $ punsafeCoerce @(PValue 'Sorted 'NoGuarantees) (pfromData ptxInfo'mint)
            expectedProgrammableOutputValue <-
                plet $
                    pif
                        (pnull # pto (pto mintValueNoGuarantees))
                        totalProgTokenValue_
                        ( punsafeCoerce @(PValue 'Sorted 'Positive) $
                            punsafeCoerce @(PValue 'Sorted 'NoGuarantees) totalProgTokenValue_
                                #<> pcheckMintLogicAndGetProgrammableValue
                                    (pfromData pdirectoryNodeCS)
                                    referenceInputs
                                    (pfromData mintProofs)
                                    withdrawalEntries
                                    mintValueNoGuarantees
                        )

            pvalidateConditions
                [ pisRewardingScript (pdata pscriptContext'scriptInfo)
                , ptraceInfoIfFalse "prog tokens escape" $
                    poutputsContainExpectedValueAtCred
                        progLogicCred
                        (pfromData ptxInfo'outputs)
                        expectedProgrammableOutputValue
                ]
        PSeizeAct{pdirectoryNodeIdx, pinputIdxs, poutputsStartIdx, plengthInputIdxs} -> P.do
            inputIdxsLen <- plet $ pfromData plengthInputIdxs
            let inputIdxsData = punsafeCoerce (pfromData pinputIdxs) :: Term _ (PBuiltinList PData)
            let remainingOutputs = pdropFast # pfromData poutputsStartIdx # pfromData ptxInfo'outputs
            let directoryNodeUTxO = pelemAtFast @PBuiltinList # referenceInputs # pfromData pdirectoryNodeIdx
            PTxOut{ptxOut'value = seizeDirectoryNodeValue, ptxOut'datum = seizeDirectoryNodeDatum} <- pmatch (ptxInInfoResolved $ pfromData directoryNodeUTxO)
            POutputDatum seizeDat' <- pmatch seizeDirectoryNodeDatum
            PDirectorySetNode
                { pkey = directoryNodeDatumFKey
                , pissuerLogicScript = directoryNodeDatumFIssuerLogicScript
                } <-
                pmatch (pfromData $ punsafeCoerce @(PAsData PDirectorySetNode) (pto seizeDat'))
            mintValueNoGuarantees <- plet $ punsafeCoerce @(PValue 'Sorted 'NoGuarantees) (pfromData ptxInfo'mint)
            seizeMintedTokens <- plet $ pmintTokensForCurrencySymbol # pfromData directoryNodeDatumFKey # mintValueNoGuarantees
            let conditions =
                    [ ptraceInfoIfFalse "mini-ledger invariants violated" $ processThirdPartyTransfer directoryNodeDatumFKey progLogicCred (pfromData ptxInfo'inputs) remainingOutputs inputIdxsData seizeMintedTokens
                    , ptraceInfoIfFalse "issuer logic script must be invoked" $ pisScriptInvokedEntries # directoryNodeDatumFIssuerLogicScript # withdrawalEntries
                    , -- directory node is valid (presence of state token)
                      ptraceInfoIfFalse "directory node is not valid" $ phasCSH (pfromData pdirectoryNodeCS) seizeDirectoryNodeValue
                    , -- input indexes must cover all script spends in the transaction.
                      ptraceInfoIfFalse "spending redeemer count mismatch" $ penforceNSpendRedeemers inputIdxsLen (pfromData ptxInfo'redeemers)
                    , -- list payload must match declared list length in the redeemer.
                      ptraceInfoIfFalse "input index length mismatch" $ (pbuiltinListLengthFast # inputIdxsLen # inputIdxsData) #== inputIdxsLen
                    ]
            pvalidateConditions conditions

punionTokens :: Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
punionTokens = pfix #$ plam $ \self tokensA tokensB ->
    pelimList
        ( \tokenPairA tokensRestA ->
            plet (pfstBuiltin # tokenPairA) $ \tokenNameA ->
                pelimList
                    ( \tokenPairB tokensRestB ->
                        pif
                            (pfromData tokenNameA #== pfromData (pfstBuiltin # tokenPairB))
                            ( -- both entries have the same token so we add quantities
                              let quantityA = pfromData (psndBuiltin # tokenPairA)
                                  quantityB = pfromData (psndBuiltin # tokenPairB)
                               in pcons # (ppairDataBuiltin # tokenNameA # pdata (quantityA + quantityB)) # (self # tokensRestA # tokensRestB)
                            )
                            ( pif
                                (pfromData tokenNameA #< pfromData (pfstBuiltin # tokenPairB))
                                -- entry A has a token that entry B does not so we add the token and quantity from entry A.
                                (pcons # tokenPairA # (self # tokensRestA # tokensB))
                                -- entry B has a token that entry A does not so we add the token and quantity from entry B.
                                (pcons # tokenPairB # (self # tokensA # tokensRestB))
                            )
                    )
                    tokensA
                    tokensB
        )
        tokensB
        tokensA

{- | Extract signed minted token pairs for a specific currency symbol from tx mint.
Returns an empty list when the currency symbol is not minted/burned in the tx.
-}
pmintTokensForCurrencySymbol ::
    Term s (PCurrencySymbol :--> PValue 'Sorted 'NoGuarantees :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
pmintTokensForCurrencySymbol =
    phoistAcyclic $
        plam $ \targetCs mintValue ->
            let mintedEntries :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger))))
                mintedEntries = pto (pto mintValue)
                go = pfix #$ plam $ \self remainingMintEntries ->
                    pelimList
                        ( \mintCsPair mintCsPairs ->
                            let mintCs = pfromData (pfstBuiltin # mintCsPair)
                             in pif
                                    (mintCs #== targetCs)
                                    (pto (pfromData (psndBuiltin # mintCsPair)))
                                    (pif (targetCs #< mintCs) pnil (self # mintCsPairs))
                        )
                        pnil
                        remainingMintEntries
             in go # mintedEntries

pcheckCorrespondingThirdPartyTransferInputsAndOutputs ::
    Term s PCurrencySymbol ->
    Term s PCredential ->
    Term _ (PBuiltinList PData :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PAsData PTxOut) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBool) ->
    Term s (PBuiltinList PData) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))) ->
    Term s PTxOut ->
    Term s PBool
pcheckCorrespondingThirdPartyTransferInputsAndOutputs programmableCS progLogicCred self remainingRelativeIdxs remainingInputsAfterIdx programmableOutputs deltaAccumulator programmableInputResolved =
    let inputTxOutConstrPair = pasConstr # pforgetData (pdata programmableInputResolved)
        correspondingOutput = phead # programmableOutputs
        outputTxOutConstrPair = pasConstr # pforgetData correspondingOutput
     in plet (psndBuiltin # inputTxOutConstrPair) $ \inputTxOutFields ->
            plet (psndBuiltin # outputTxOutConstrPair) $ \outputTxOutFields ->
                plet (phead # inputTxOutFields) $ \inputTxOutAddress ->
                    plet (phead # outputTxOutFields) $ \outputTxOutAddress ->
                        plet (ptail # inputTxOutFields) $ \inputTxOutFieldsRest ->
                            plet (ptail # outputTxOutFields) $ \outputTxOutFieldsRest ->
                                plet (phead # (psndBuiltin # (pasConstr # inputTxOutAddress))) $ \inputCredentialData ->
                                    let programmableInputValue = punsafeCoerce @(PAsData (PValue 'Sorted 'Positive)) (phead # inputTxOutFieldsRest)
                                        programmableOutputValue = punsafeCoerce @(PAsData (PValue 'Sorted 'Positive)) (phead # outputTxOutFieldsRest)
                                        programmableInputRest = ptail # inputTxOutFieldsRest
                                        programmableOutputRest = ptail # outputTxOutFieldsRest
                                     in pif
                                            (inputCredentialData #== pforgetData (pdata progLogicCred))
                                            ( pif
                                                ( pand'List
                                                    [ ptraceInfoIfFalse "corresponding output: address mismatch" $
                                                        inputTxOutAddress #== outputTxOutAddress
                                                    , ptraceInfoIfFalse "corresponding output: datum/reference script mismatch" $
                                                        programmableInputRest #== programmableOutputRest
                                                    ]
                                                )
                                                ( let delta = pvalueEqualsDeltaCurrencySymbol programmableCS programmableInputValue programmableOutputValue
                                                   in self # remainingRelativeIdxs # remainingInputsAfterIdx # (ptail # programmableOutputs) # (punionTokens # delta # deltaAccumulator)
                                                )
                                                perror
                                            )
                                            ( pif
                                                ((pfstBuiltin # (pasConstr # inputCredentialData)) #== 1)
                                                (self # remainingRelativeIdxs # remainingInputsAfterIdx # programmableOutputs # deltaAccumulator)
                                                (ptraceInfoError "input index points to pubkey input")
                                            )

processThirdPartyTransfer ::
    Term s (PAsData PCurrencySymbol) ->
    Term s PCredential ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s (PBuiltinList PData) ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))) ->
    Term s PBool
processThirdPartyTransfer programmableCS progLogicCred inputs progOutputs inputIdxs' mintedTokens =
    let
        programmableCS' = pfromData programmableCS
        checkBalanceInvariant :: Term _ (PBuiltinList (PAsData PTxOut)) -> Term _ (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))) -> Term _ PBool
        checkBalanceInvariant remainingOutputs deltaAccumulatorResult =
            let outputAccumulatorResult = go2 # remainingOutputs
                deltaResultValue = punsafeCoerce @(PValue 'Sorted 'Positive) (pconsBuiltin # (ppairDataBuiltinRaw # pforgetData programmableCS # (pmapData # punsafeCoerce deltaAccumulatorResult)) # pnil)
             in pif
                    (pvalueContains # outputAccumulatorResult # deltaResultValue)
                    (pconstant True)
                    perror

        go2 :: Term _ (PBuiltinList (PAsData PTxOut) :--> PValue 'Sorted 'Positive)
        go2 = pfix #$ plam $ \self programmableOutputs ->
            pelimList
                ( \programmableOutput programmableOutputsRest ->
                    pmatch (pfromData programmableOutput) $ \(PTxOut{ptxOut'address = programmableOutputAddress, ptxOut'value = programmableOutputValue}) ->
                        pif
                            (paddressCredential programmableOutputAddress #== progLogicCred)
                            (pvalueUnionFast # pfromData programmableOutputValue # (self # programmableOutputsRest))
                            pmempty
                )
                pmempty
                programmableOutputs

        go :: Term _ (PBuiltinList PData :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PAsData PTxOut) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBool)
        go = pfix #$ plam $ \self relativeInputIdxs remainingInputs programmableOutputs deltaAccumulator ->
            pelimList
                ( \relativeIdxData remainingRelativeIdxs ->
                    let relativeIdx = pasInt # relativeIdxData
                     in plet (pdropFast # relativeIdx # remainingInputs) $ \remainingInputsAtIdx ->
                            plet (phead # remainingInputsAtIdx) $ \programmableInput ->
                                let remainingInputsAfterIdx = ptail # remainingInputsAtIdx
                                 in plet (ptxInInfoResolved $ pfromData programmableInput) $ \programmableInputResolved ->
                                        pcheckCorrespondingThirdPartyTransferInputsAndOutputs
                                            programmableCS'
                                            progLogicCred
                                            self
                                            remainingRelativeIdxs
                                            remainingInputsAfterIdx
                                            programmableOutputs
                                            deltaAccumulator
                                            programmableInputResolved
                )
                (checkBalanceInvariant programmableOutputs (punionTokens # deltaAccumulator # mintedTokens))
                relativeInputIdxs
     in
        go # inputIdxs' # inputs # progOutputs # pnil

-------------------------------------------------------------------------------
-- Corresponding inputs and outputs from and to the programmable token spending script (mini-ledger where all programmable tokens live).
-- Example Inputs:
-- inputA = {
--   progCS: { Foo: 120, Bar: 80 },
--   ADA: { "": 3_000_000 },
--   usdCS: { USDT: 50 },
--   nftCS: { ArtNFT: 1 }
-- }

-- inputB = {
--   progCS: { Foo: 70 },
--   ADA: { "": 2_000_000 },
--   usdCS: { USDT: 10 }
-- }

-- inputC = {
--   progCS: { Foo: 40, Bar: 30 },
--   ADA: { "": 1_500_000 }
-- }

-------------------------------------------------------------------------------
-- Corresponding Outputs:
-- Corresponding outputs are the continuing outputs for their corresponding inputs. They must have the same address as their input (as indicated by their label as continuing outputs)
-- they must also have the same datum and the same reference script hash (if present) as their input.
-- Finally, they must also have the same value as their input except for the balance of tokens with the progCS currency symbol, for tokens of that currency symbol.

-- Example outputs:
-- correspondingOutputA = {
--   progCS: { Foo: 140, Bar: 60 },
--   ADA: { "": 3_000_000 },
--   usdCS: { USDT: 50 },
--   nftCS: { ArtNFT: 1 }
-- }

-- correspondingOutputB = {
--   progCS: { Foo: 20 },
--   ADA: { "": 2_000_000 },
--   usdCS: { USDT: 10 }
-- }

-- correspondingOutputC = {
--   progCS: { Foo: 10, Bar: 10 },
--   ADA: { "": 1_500_000 }
-- }

-------------------------------------------------------------------------------
-- Remaining outputs:
-- Remaining programmable token outputs - these are outputs to the programmable token spending script (mini-ledger where all programmable tokens live)
-- that are not corresponding to any inputs to the programmable token spending script. The accumulated value of these outputs must contain
-- the delta between the amount of programmable asset in the inputs and the amount of programmable asset in the corresponding outputs thus assuring all
-- programmable assets must stay within the programmable token spending script.
-- Example remaining outputs:
-- remainingOutputA = {
--   progCS: { Foo: 40, Bar: 25 },
--   ADA: { "": 2_000_000 }
-- }

-- remainingOutputB = {
--   progCS: { Foo: 20, Bar: 15 },
--   ADA: { "": 2_000_000 }
-- }

-------------------------------------------------------------------------------
-- The below calculation checks that the total amount of programmable tokens spent from the script is equal to the amount sent to the script,
-- and that each correspondingOutput is equal to it's input except for the balance of tokens with the progCS currency symbol, for tokens of that currency symbol
-- each corresponding output contains either more or less than the amount of the tokens in the input.

-- accumulatedValue = amount of programmable asset in input - amount of programmable asset in corresponding output

-- outputValueAccumulator = emptyValue
-- if accumulatedValue > 0
--   for each remainingOutput:
--     outputValueAccumulator = outputValueAccumulator <> remainingOutputValue

-- if (valueContains outputValueAccumulator accumulatedValue)
--    constant True

{- | Negates the quantity of each token in a list of token quantity pairs (ie. the inner map of a `PValue`).
Example:
pnegateTokens [("FooToken", 10), ("BarToken", 20)] = [("FooToken", -10), ("BarToken", -20)]
-}
pnegateTokens :: Term _ (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
pnegateTokens = pfix #$ plam $ \self tokens ->
    pelimList
        ( \tokenPair tokensRest ->
            let tokenName = pfstBuiltin # tokenPair
                tokenAmount = psndBuiltin # tokenPair
             in pcons # (ppairDataBuiltin # tokenName # pdata (pconstantInteger 0 - pfromData tokenAmount)) # (self # tokensRest)
        )
        pnil
        tokens

{- |
`pvalueEqualsDeltaCurrencySymbol progCS inputUTxOValue outputUTxOValue` MUST check that inputUTxOValue is equal to outputUTxOValue for all tokens except those of currency symbol progCS.
The function should return a value consisting of only tokens with the currency symbol progCS, this value is as follows: For each token t of currency symbol progCS, the quantity of the token
in the return value rValue is the quantity of token t in inputUTxOValue minus the quantity of token t in outputUTxOValue.
for the purposes of the subtraction ie. if inputUTxOValue has 0 FooToken and outputUTxOValue has 10 FooToken then rValue should have 0 - 10 = -10 FooToken.
-}
pvalueEqualsDeltaCurrencySymbol ::
    forall anyOrder anyAmount s.
    Term s PCurrencySymbol ->
    Term s (PAsData (PValue anyOrder anyAmount)) ->
    Term s (PAsData (PValue anyOrder anyAmount)) ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
pvalueEqualsDeltaCurrencySymbol progCS inputUTxOValue outputUTxOValue =
    let innerInputValue :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap anyOrder PTokenName PInteger))))
        innerInputValue = pto (pto $ pfromData inputUTxOValue)
        innerOutputValue :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap anyOrder PTokenName PInteger))))
        innerOutputValue = pto (pto $ pfromData outputUTxOValue)

        psubtractTokens ::
            Term
                _
                ( PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
                    :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
                    :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
                )
        psubtractTokens =
            pfix #$ plam $ \self inputTokens outputTokens ->
                pelimList
                    ( \inputPair inputRest ->
                        plet (pfstBuiltin # inputPair) $ \inputTokenName ->
                            let inputTokenAmount = psndBuiltin # inputPair
                             in pelimList
                                    ( \outputPair outputRest ->
                                        let outputTokenName = pfstBuiltin # outputPair
                                            outputTokenAmount = psndBuiltin # outputPair
                                         in pif
                                                (pfromData inputTokenName #<= pfromData outputTokenName)
                                                ( -- inputTokenName <= outputTokenName
                                                  pif
                                                    (inputTokenName #== outputTokenName)
                                                    ( -- names equal → diff = input − output; skip if zero
                                                      let diff = pfromData inputTokenAmount - pfromData outputTokenAmount
                                                       in pif
                                                            (diff #== 0)
                                                            (self # inputRest # outputRest)
                                                            ( pcons
                                                                # (ppairDataBuiltin # inputTokenName # pdata diff)
                                                                # (self # inputRest # outputRest)
                                                            )
                                                    )
                                                    ( -- outputTokenName > inputTokenName → token only in input (nonzero by invariant)
                                                      let diff = pfromData inputTokenAmount
                                                       in pcons
                                                            # (ppairDataBuiltin # inputTokenName # pdata diff)
                                                            # (self # inputRest # outputTokens)
                                                    )
                                                )
                                                ( -- outputTokenName < inputTokenName → token only in output (nonzero by invariant)
                                                  let diff = pconstantInteger 0 - pfromData outputTokenAmount
                                                   in pcons
                                                        # (ppairDataBuiltin # outputTokenName # pdata diff)
                                                        # (self # inputTokens # outputRest)
                                                )
                                    )
                                    -- output exhausted → emit remaining input tokens as positive (nonzero by invariant)
                                    inputRest
                                    outputTokens
                    )
                    -- input exhausted → emit remaining output tokens as negative (nonzero by invariant)
                    (pnegateTokens # outputTokens)
                    inputTokens

        -- no need to check for progCs in "everything should be same" parts
        -- input  : |- everything should be same -| |-progCs-| |-everything should be same-|
        -- output : |- everything should be same -| |-progCs-| |-everything should be same-|
        goOuter ::
            Term
                _
                ( PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap anyOrder PTokenName PInteger)))
                    :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap anyOrder PTokenName PInteger)))
                    :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) -- accumulator (delta for progCS)
                    :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
                )
        goOuter = pfix #$ plam $ \self inputValuePairs outputValuePairs diffAccumulator ->
            pelimList
                ( \inputValueEntry inputValueEntries ->
                    plet (pfstBuiltin # inputValueEntry) $ \inputValueEntryCS ->
                        pelimList
                            ( \outputValueEntry outputValueEntries ->
                                pif
                                    (pfromData inputValueEntryCS #== pfromData (pfstBuiltin # outputValueEntry))
                                    ( pif
                                        (pfromData inputValueEntryCS #== progCS)
                                        ( pif
                                            (pmapData # punsafeCoerce outputValueEntries #== pmapData # punsafeCoerce inputValueEntries)
                                            (psubtractTokens # pto (pfromData (psndBuiltin # inputValueEntry)) # pto (pfromData @(PMap anyOrder PTokenName PInteger) (psndBuiltin # outputValueEntry)))
                                            perror
                                        )
                                        (pif (psndBuiltin # inputValueEntry #== psndBuiltin # outputValueEntry) (self # inputValueEntries # outputValueEntries # diffAccumulator) perror)
                                    )
                                    (pif (psndBuiltin # inputValueEntry #== psndBuiltin # outputValueEntry) diffAccumulator perror)
                            )
                            pnil
                            outputValuePairs
                )
                pnil
                inputValuePairs
     in goOuter # innerInputValue # innerOutputValue # pnil
