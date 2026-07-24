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
    ProgrammableLogicGlobalRedeemer (..),
    PProgrammableLogicGlobalRedeemer (..),
    MintProof (..),
    absoluteToRelativeInputIdxs,
    mkSeizeActRedeemerFromAbsoluteInputIdxs,
    mkSeizeActRedeemerFromRelativeInputIdxs,
    mkProgrammableLogicBase,
    mkProgrammableLogicGlobal,
    mkProgrammableSeize,
    pparamsAtRefIdx,
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
import Plutarch.Builtin.List (pdropList)
import Plutarch.Builtin.Value (PBuiltinValue, pinsertCoin, punValueData, punionValue, pvalueData)
import Plutarch.Builtin.Value qualified as BuiltinValue
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

{- | Unsafely unwrap a `PMaybeData` known by the caller to be `Just`.

High-level purpose:
- Avoid repeated `pmatch` boilerplate when surrounding logic has already established
  presence of the inner value.

Security invariants:
- The caller must prove the input is `Just`; using this on `Nothing` is invalid.
- This helper must not be used to bypass missing-datum or missing-stake checks.
- The extracted payload must be interpreted at the same type it was encoded with.
-}
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

{- | Host-side empty `Value` constant used to build onchain empty values.

High-level purpose:
- Provide a single canonical zero value for coercion into Plutarch terms.

Security invariants:
- This must remain exactly `mempty`; any non-zero asset would corrupt every caller.
- It must not depend on transaction data or redeemer-controlled inputs.
-}
emptyValue :: Value
emptyValue = mempty

{- | Canonical onchain empty non-Ada value.

High-level purpose:
- Seed value accumulators that track programmable assets.

Security invariants:
- The value must stay empty for all policies and token names.
- It is only sound to coerce emptiness to a positive sorted value because emptiness
  trivially satisfies both properties.
-}
pemptyLedgerValue :: Term s (PValue 'Sorted 'Positive)
pemptyLedgerValue = punsafeCoerce $ pconstant @(PValue 'Unsorted 'NoGuarantees) emptyValue

{- | Strip Ada from a ledger-provided value while preserving the remaining order.

High-level purpose:
- Remove the always-present Ada entry so later mini-ledger accounting can operate
  only on non-Ada assets.

Security invariants:
- The input must be a ledger-provided sorted value where Ada is the first entry.
- The helper must preserve the order and quantities of all non-Ada entries.
- It must not be used on synthetic values whose ordering does not follow ledger
  conventions.
-}
pstripAdaH ::
    forall (v :: AmountGuarantees) (s :: S).
    Term s (PValue 'Sorted v) -> Term s (PValue 'Sorted v)
pstripAdaH value =
    let nonAdaValueMapInner = ptail # pto (pto value)
     in pcon (PValue $ pcon $ PMap nonAdaValueMapInner)

{- | Merge two sorted token-name maps by asset-wise addition.

High-level purpose:
- Support fast programmable-token balance accumulation without rebuilding full
  `Value`s.

Security invariants:
- Both inputs must be sorted by token name and internally duplicate-free.
- Matching token names must be added exactly once; non-matching entries must be
  preserved unchanged.
- The output must remain sorted, because later containment and delta checks rely
  on monotonic order.
-}
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

{- | Merge two sorted currency-symbol maps by asset-wise addition.

High-level purpose:
- Provide a linear merge for full `Value` accumulation when multiple policy maps
  must be combined.

Security invariants:
- Inputs must be sorted by currency symbol and each inner token map must itself be
  sorted and duplicate-free.
- Asset quantities must be combined only for equal currency symbols and equal token
  names.
- The resulting outer and inner maps must stay sorted for downstream lookups and
  containment checks.
-}
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

{- | Drop non-positive entries from a sorted currency-pair list (and any policy
whose token map becomes empty).

High-level purpose:
- Normalize a mint/burn-adjusted expected value so downstream containment checks
  can assume strictly positive quantities: a fully burned asset (quantity zero)
  or an over-burned asset (negative) requires nothing to remain at the
  mini-ledger outputs, exactly as a `>= non-positive` lookup would conclude.

Security invariants:
- Only entries with quantity <= 0 may be removed; positive entries must be
  preserved verbatim and in order.
- The result must remain canonically sorted.
-}
pfilterPositiveCurrencyPairs ::
    Term
        s
        ( PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))
            :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))
        )
pfilterPositiveCurrencyPairs = phoistAcyclic $
    let filterTokens ::
            Term
                _
                ( PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
                    :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
                )
        filterTokens = pfix #$ plam $ \self tokenPairs ->
            pelimList
                ( \tokenPair tokenPairsRest ->
                    pif
                        (pfromData (psndBuiltin # tokenPair) #<= 0)
                        (self # tokenPairsRest)
                        (pcons # tokenPair # (self # tokenPairsRest))
                )
                pnil
                tokenPairs
     in pfix #$ plam $ \self csPairs ->
            pelimList
                ( \csPair csPairsRest ->
                    plet (filterTokens # pto (pfromData (psndBuiltin # csPair))) $ \positiveTokens ->
                        pelimList
                            ( \_ _ ->
                                pcons
                                    # punsafeCoerce
                                        ( ppairDataBuiltinRaw
                                            # pforgetData (pfstBuiltin # csPair)
                                            # (pmapData # punsafeCoerce positiveTokens)
                                        )
                                    # (self # csPairsRest)
                            )
                            (self # csPairsRest)
                            positiveTokens
                )
                pnil
                csPairs

{- | Reverse a currency-pair list (accumulator-based, linear).

High-level purpose:
- The transfer/mint proof walks cons matched policies while traversing their
  ascending inputs, so their accumulators come out DESCENDING; this restores
  canonical ascending order before the lists reach order-sensitive consumers.

Security invariants:
- Every downstream consumer of an aggregated programmable value — the mint-delta
  union ('pcurrencyPairsUnionFast') and the output-containment subtract walk —
  REQUIRES canonically sorted input; feeding a reversed list would corrupt the
  merge and could under-require outputs. Callers must apply this reverse to any
  cons-built accumulator before exposing it.
- The reverse must neither drop, duplicate, nor alter entries.
-}
preverseCurrencyPairs ::
    Term
        s
        ( PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))
            :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))
        )
preverseCurrencyPairs = phoistAcyclic $
    plam $ \csPairs ->
        ( pfix #$ plam $ \self acc remaining ->
            pelimList (\x xs -> self # (pcons # x # acc) # xs) acc remaining
        )
            # pnil
            # csPairs

{- | Add two non-Ada sorted `Value`s while preserving canonical ordering.

High-level purpose:
- Centralize asset-wise value addition used throughout transfer and mint
  accounting.

Security invariants:
- Callers must only pass sorted positive values.
- No asset may be dropped, duplicated, or reordered outside the merge rules.
- The output must remain a valid sorted positive value.
-}
pvalueUnionFast :: Term s (PValue 'Sorted 'Positive :--> PValue 'Sorted 'Positive :--> PValue 'Sorted 'Positive)
pvalueUnionFast = phoistAcyclic $ plam $ \valueA valueB ->
    pcon $
        PValue $
            pcon $
                PMap $
                    pcurrencyPairsUnionFast
                        # pto (pto valueA)
                        # pto (pto valueB)

{- | Check whether a specific stake-script credential appears in withdrawals.

High-level purpose:
- Detect whether a stake validator was actually invoked in the current
  transaction.

Security invariants:
- The result must be true iff an entry with that credential is present in the
  withdrawal map.
- This helper only proves stake-script invocation; callers must not treat it as a
  spending or minting witness check.
- Callers must only use it with a non-empty withdrawal list, because the loop
  assumes one.
-}
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

{- | Aggregate all non-Ada input value controlled by a payment credential and
validated owner witness.

High-level purpose:
- Compute the programmable mini-ledger value entering the transaction from inputs
  locked at the shared payment credential.

Security invariants:
- Only inputs whose payment credential equals `cred` may contribute to the result.
- Each contributing input must prove control of its staking credential:
  `PPubKeyCredential` via a signer, `PScriptCredential` via a withdrawal witness.
- Missing owner witnesses must fail validation rather than silently omitting value.
- Ada must be stripped and remaining assets summed exactly once.
-}
pvalueFromCred ::
    Term s PCredential ->
    Term s (PBuiltinList (PAsData PPubKeyHash)) ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace))) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    -- Returns the accumulated non-Ada currency-pair list (sorted), same shape
    -- the lockstep proof walk consumes. Hybrid accumulation strategy:
    -- zero or one contributing input extracts the raw pairs directly (one
    -- UnMapData + tail, exactly the pre-PV11 cost); two or more inputs switch
    -- to the PV11 builtin Value — one unValueData per input and near-constant
    -- memory unionValue merges — which removes the quadratic sorted-merge
    -- component on the inputs axis, then bridges back to pairs once
    -- (insertCoin amount 0 deletes the ada entry).
    Term s (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger))))
pvalueFromCred cred sigs withdrawalEntries inputs =
    let credData = pforgetData (pdata cred)

        -- Shared per-input gate: k receives the input's raw value Data iff the
        -- input sits at `cred` and its owner witness is present; otherwise
        -- skip receives the rest of the walk. Haskell-level, so it inlines
        -- into each of the three loop bodies below.
        withContributing ::
            Term _ (PAsData PTxInInfo) ->
            (Term _ PData -> Term _ r) ->
            Term _ r ->
            Term _ r
        withContributing txIn k skip =
            plet (pdata (ptxInInfoResolved $ pfromData txIn)) $ \resolvedOutData ->
                plet (psndBuiltin # (pasConstr # pforgetData resolvedOutData)) $ \resolvedOutFields ->
                    let resolvedOutAddressData = phead # resolvedOutFields
                        resolvedOutValueData = phead # (ptail # resolvedOutFields)
                        paymentCredData = phead # (psndBuiltin # (pasConstr # resolvedOutAddressData))
                        stakingCredMaybe = punsafeCoerce @(PMaybeData PStakingCredential) (phead # (ptail # (psndBuiltin # (pasConstr # resolvedOutAddressData))))
                     in pif
                            (paymentCredData #== credData)
                            ( pmatch (pjustData stakingCredMaybe) $ \case
                                PStakingHash ownerCred ->
                                    pmatch ownerCred $ \case
                                        PPubKeyCredential pkh ->
                                            pif
                                                (ptxSignedByPkh # pkh # sigs)
                                                (k resolvedOutValueData)
                                                (ptraceInfoError "Missing required pk witness")
                                        PScriptCredential scriptHash_ ->
                                            let scriptCredData = pdata $ pcon (PScriptCredential scriptHash_)
                                             in pif
                                                    (pisScriptInvokedEntries # scriptCredData # withdrawalEntries)
                                                    (k resolvedOutValueData)
                                                    (ptraceInfoError "Missing required script witness")
                                _ -> perror
                            )
                            skip

        -- Phase 3: two or more contributing inputs seen; accumulate builtin.
        goBuiltin = pfix #$ plam $ \self acc remaining ->
            pelimList
                ( \txIn xs ->
                    withContributing
                        txIn
                        (\vd -> self # (punionValue # acc # (punValueData # vd)) # xs)
                        (self # acc # xs)
                )
                ( punsafeCoerce
                    @(PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger))))
                    (pasMap # (pvalueData # (pinsertCoin # pconstant "" # pconstant "" # 0 # acc)))
                )
                remaining
        -- Phase 2: exactly one contributing input so far (raw value Data held).
        goRest = pfix #$ plam $ \self firstVd remaining ->
            pelimList
                ( \txIn xs ->
                    withContributing
                        txIn
                        (\vd -> goBuiltin # (punionValue # (punValueData # firstVd) # (punValueData # vd)) # xs)
                        (self # firstVd # xs)
                )
                ( punsafeCoerce
                    @(PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger))))
                    (ptail # (pasMap # firstVd))
                )
                remaining
        -- Phase 1: no contributing input seen yet.
        goFind = pfix #$ plam $ \self remaining ->
            pelimList
                ( \txIn xs ->
                    withContributing
                        txIn
                        (\vd -> goRest # vd # xs)
                        (self # xs)
                )
                pnil
                remaining
     in goFind # inputs

{- | Aggregate all non-Ada output value at a payment credential.

High-level purpose:
- Measure how much programmable value remains at the shared mini-ledger payment
  credential after the transaction.

Security invariants:
- Only outputs whose payment credential equals `cred` may contribute.
- Stake credentials, datums, and reference scripts are intentionally ignored here;
  callers must validate those separately when required.
- Ada must be stripped and non-Ada assets summed without loss or duplication.
-}
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
asset quantity. Otherwise it walks the outputs ONCE, subtracting each mini-ledger
output's non-Ada assets from the remaining expected list via a sorted linear merge
and exiting early once nothing remains. Neither path allocates an aggregated
output value, and neither re-scans outputs per expected asset (which degenerates
to O(assets x outputs x value-size) with many token names).

PRECONDITION: every quantity in `expectedValue` must be strictly positive. The
transfer-input aggregation satisfies this by construction (input values are
positive); the mint branch filters non-positive entries out of the merged
mint/burn delta before calling this helper (a fully burned asset needs no
remaining output, exactly as a `>= 0` lookup would conclude).

Security invariants:

- The result must be a lower-bound containment check only for outputs at
  `progLogicCred`.
- No value at other payment credentials may satisfy the requirement.
- This helper assumes the caller has already validated that `expectedValue`
  represents the value that must remain inside the mini-ledger.
- The single-asset fast path and the subtract-walk path must be semantically
  equivalent.
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
        -- Multi-asset path (PV11): accumulate every mini-ledger output's value
        -- with the builtin unionValue (near-constant memory per output) and
        -- decide with a single valueContains. This replaces the sorted
        -- subtract-walk entirely. Extra entries at the outputs — including ada,
        -- which is deliberately not stripped here — are irrelevant to a
        -- lower-bound containment check. The expected side is converted once;
        -- its entries are strictly positive by this function's precondition,
        -- which valueContains requires of both arguments.
        progLogicCredData = pforgetData (pdata progLogicCred)
        accumulateOutputsAtCred = pfix #$ plam $ \self acc remainingOutputs ->
            pelimList
                ( \txOut outputsRest ->
                    plet (psndBuiltin # (pasConstr # pforgetData txOut)) $ \txOutFields ->
                        let txOutAddress = phead # txOutFields
                            txOutValueData = phead # (ptail # txOutFields)
                            paymentCredData = phead # (psndBuiltin # (pasConstr # txOutAddress))
                         in self
                                # pif
                                    (paymentCredData #== progLogicCredData)
                                    (punionValue # acc # (punValueData # txOutValueData))
                                    acc
                                # outputsRest
                )
                acc
                remainingOutputs
        checkByBuiltinContains =
            BuiltinValue.pvalueContains
                # (accumulateOutputsAtCred # (punValueData # (pmapData # pnil)) # txOutputs)
                # (punValueData # (pmapData # punsafeCoerce expectedCsPairs))
        -- Wholesale-move fast path: the dominant multi-asset shape sends the
        -- entire expected value to a single recipient output (a full transfer,
        -- or a consolidation of many inputs into one output). In that case the
        -- first mini-ledger output's non-ada value equals the expected map
        -- byte-for-byte, and one Data equality replaces every conversion the
        -- builtin path would pay (unValueData is linear in value size with a
        -- much larger constant than equalsData). On mismatch we fall through to
        -- the full builtin containment over all outputs.
        expectedMapData = pmapData # punsafeCoerce expectedCsPairs
        checkWholesaleThenBuiltin = pfix #$ plam $ \self remainingOutputs ->
            pelimList
                ( \txOut outputsRest ->
                    plet (psndBuiltin # (pasConstr # pforgetData txOut)) $ \txOutFields ->
                        let txOutAddress = phead # txOutFields
                            txOutValueData = phead # (ptail # txOutFields)
                            paymentCredData = phead # (psndBuiltin # (pasConstr # txOutAddress))
                         in pif
                                (paymentCredData #== progLogicCredData)
                                ( pif
                                    ((pmapData # (ptail # (pasMap # txOutValueData))) #== expectedMapData)
                                    (pconstant True)
                                    checkByBuiltinContains
                                )
                                (self # outputsRest)
                )
                checkByBuiltinContains
                remainingOutputs
        expectedCsPairs = pto (pto expectedValue)
     in -- Dispatch: exactly one expected asset (one currency symbol with one
        -- token name — the dominant transfer shape) takes the accumulate-scan
        -- fast path; everything else takes the single-pass subtract walk.
        pelimList
            ( \csPair csPairsRest ->
                plet (pto (pfromData (psndBuiltin # csPair))) $ \tnPairs ->
                    pif
                        ((pnull # csPairsRest) #&& (pelimList (\_ tnRest -> pnull # tnRest) (pconstant False) tnPairs))
                        ( pelimList
                            ( \tnPair _ ->
                                hasAtLeastAssetInProgOutputs
                                    # pfromData (psndBuiltin # tnPair)
                                    # 0
                                    # pfromData (pfstBuiltin # csPair)
                                    # pfromData (pfstBuiltin # tnPair)
                                    # txOutputs
                            )
                            (pconstant True)
                            tnPairs
                        )
                        (checkWholesaleThenBuiltin # txOutputs)
            )
            (pconstant True)
            expectedCsPairs

{- | Base spending validator for programmable-token UTxOs.

High-level purpose:
- Force every spend of the shared programmable-token payment script to be
  accompanied by the global stake validator through the withdraw-zero pattern.

Security invariants:
- Spending must fail unless one of the two designated stake credentials — the
  global (transfer) validator or the seize validator — appears in the transaction
  withdrawals.
- This validator must not independently authorize transfers or minting; it only
  enforces delegation to one of those two validators. Whichever runs enforces its
  full invariants over the whole transaction, so authorizing either is sound.
- The check must be credential-exact, so unrelated withdrawals cannot satisfy it.

Deployment note (why this scan needs no redeemer index): the withdrawal map is
sorted by reward-account bytes, and the global/seize validator hashes are mined
to be lexically minimal at deployment, so their withdrawals sit at the front of
the map and this linear scan terminates within the first entries regardless of
how many other withdrawals a transaction carries.
-}
mkProgrammableLogicBase :: Term s (PAsData PCredential :--> PAsData PCredential :--> PScriptContext :--> PUnit)
mkProgrammableLogicBase = plam $ \globalCred seizeCred ctx ->
    pmatch (pscriptContextTxInfo ctx) $ \txInfo ->
        let wdrls :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace)))
            wdrls = pto $ pfromData $ ptxInfo'wdrl txInfo
            go = pfix #$ plam $ \self withdrawals' ->
                pelimList
                    ( \withdrawal rest ->
                        let c = pfstBuiltin # withdrawal
                         in (c #== globalCred) #|| (c #== seizeCred) #|| (self # rest)
                    )
                    (pconstant False)
                    withdrawals'
         in pvalidateConditions [ptraceInfoIfFalse "programmable global/seize not invoked" (go # wdrls)]

{- | Check that the first non-Ada policy in a ledger value matches a state-token
currency symbol.

High-level purpose:
- Provide the cheapest possible legitimacy check for directory/protocol reference
  UTxOs whose state token is expected to be the first non-Ada entry.

Security invariants:
- This is /not/ a general membership scan; it intentionally treats “first non-Ada
  policy is not the expected state token” as a failed legitimacy check.
- In protocol paths that use this helper to validate a reference UTxO, a mismatch
  means the transaction must be rejected.
- A false result must never be interpreted as “policy absent everywhere”; it only
  means the first non-Ada entry is not the expected currency symbol.
- Values with no non-Ada entries are malformed for this use case and may cause
  evaluation to fail.
-}
phasCSH :: Term s (PCurrencySymbol :--> PAsData (PValue 'Sorted 'Positive) :--> PBool)
phasCSH = phoistAcyclic $ plam $ \directoryNodeCS value ->
    let value' = pto (pto (pfromData value))
     in pfromData (pfstBuiltin # (phead # (ptail # value'))) #== directoryNodeCS

{- | Safe variant of `phasCSH` that returns `False` instead of crashing on missing
non-Ada entries.

High-level purpose:
- Probe candidate reference inputs before attempting datum decoding.

Security invariants:
- Like `phasCSH`, this only inspects the first non-Ada policy entry.
- It must never be used as a general-purpose “contains currency symbol anywhere”
  predicate.
- Returning `False` is only appropriate while scanning candidates; protocol paths
  that require the state token must keep searching or reject the transaction.
- It must not mask malformed data at call sites where the identified reference
  UTxO is mandatory for validation.
-}
phasCSHOrFalse :: Term s PCurrencySymbol -> Term s (PAsData (PValue 'Sorted 'Positive)) -> Term s PBool
phasCSHOrFalse directoryNodeCS value =
    let nonAdaEntries = ptail # pto (pto (pfromData value))
     in pelimList
            (\currencyPair _ -> pfromData (pfstBuiltin # currencyPair) #== directoryNodeCS)
            (pcon PFalse)
            nonAdaEntries

{- | Locate the protocol-parameter reference input by its state-token currency
symbol and decode its datum.

High-level purpose:
- Recover the global protocol configuration that every validator in this module is
  parameterized by.

Security invariants:
- The chosen reference input must be identified by the expected state token.
- The datum must be present and decode as `ProgrammableLogicGlobalParams`.
- If no reference input carries the protocol state token, evaluation must fail and
  the transaction must be rejected.
- A candidate UTxO with the right state token but missing or malformed datum must
  also cause rejection rather than falling back to any default.
-}
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
        go = pfix #$ plam $ \self remainingRefInputs ->
            let txIn = phead # remainingRefInputs
             in plet (ptxInInfoResolved $ pfromData txIn) $ \resolvedOut ->
                    pif
                        (phasCSHOrFalse currencySymbol (ptxOutValue resolvedOut))
                        (extractParams resolvedOut)
                        (self # (ptail # remainingRefInputs))
     in go # referenceInputs

{- | Indexed variant of 'pfindReferenceInputByCS' (spec §11.3/§11.4): resolve the
protocol-params reference input directly at the redeemer-supplied @paramsRefIdx@
rather than scanning. The index is a self-validating hint — a wrong or
out-of-bounds index makes the mandatory 'phasCSH' authentication fail (or
@perror@s), so honesty is a liveness concern only, never a trust assumption. The
hardened anchor policy (§4.1) guarantees the authenticated UTxO's datum is
well-formed, so raw decode is sound.
-}
pparamsAtRefIdx ::
    Term s PCurrencySymbol ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s PInteger ->
    Term s PProgrammableLogicGlobalParams
pparamsAtRefIdx currencySymbol referenceInputs paramsRefIdx =
    plet (ptxInInfoResolved $ pfromData (phead # (pdropList # paramsRefIdx # referenceInputs))) $ \resolvedOut ->
        pif
            (phasCSH # currencySymbol # ptxOutValue resolvedOut)
            ( pmatch (ptxOutDatum resolvedOut) $ \case
                POutputDatum paramDat' ->
                    pfromData $ punsafeCoerce @(PAsData PProgrammableLogicGlobalParams) (pto paramDat')
                _ -> ptraceInfoError "protocol params datum missing"
            )
            (ptraceInfoError "params ref input not authenticated at index")

{- | Filter input value down to programmable policies by validating one directory
node witness per policy entry.

High-level purpose:
- Walk the non-Ada policies in the mini-ledger input value, prove which ones are
  registered in the directory, and retain only those programmable entries.

Security invariants:
- `proofList` must be aligned with the currency-symbol order of `totalValue`.
- A positive proof must reference the exact directory node and the associated
  transfer logic script must be invoked, with adjacent identical scripts allowed to
  reuse the cached witness check.
- A negative proof must reference a covering node whose `(key, next)` interval
  excludes the current currency symbol.
- Every referenced directory node must itself be legitimate, proven by the
  directory state token.
- No non-programmable policy may be added to the returned value, and no validated
  programmable policy may be dropped.
-}
pcheckTransferLogicAndGetProgrammableValue ::
    Term s PCurrencySymbol ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PInteger)) ->
    Term s (PBuiltinList (PAsData PInteger)) ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace))) ->
    Term s (PAsData PCredential) ->
    -- Accepts the aggregated non-Ada currency-pair list directly (as produced by
    -- `pvalueFromCred`), avoiding an unwrap of a re-wrapped PValue.
    Term s (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))) ->
    Term s (PValue 'Sorted 'Positive)
pcheckTransferLogicAndGetProgrammableValue directoryNodeCS refInputs proofList wdrlIdxList withdrawalEntries initialCachedTransferScript mapInnerList =
    let -- Cache transfer-script invocation checks across adjacent positive proofs;
        -- on a cache miss, verify the redeemer-witnessed withdrawal index instead
        -- of scanning the withdrawal map (scan-proof: O(1) per policy regardless
        -- of how many withdrawals the transaction carries).
        go = pfix #$ plam $ \self proofs wdrlIdxs inputInnerValue actualProgrammableTokenValue cachedTransferScript ->
            pelimList
                ( \csPair csPairs ->
                    P.do
                        PTxOut{ptxOut'value = directoryNodeUTxOFValue, ptxOut'datum = directoryNodeUTxOFDatum} <-
                            pmatch $ ptxInInfoResolved (pfromData $ phead # (pdropList # pfromData (phead # proofs) # refInputs))
                        POutputDatum directoryNodeDatum' <- pmatch directoryNodeUTxOFDatum
                        PDirectorySetNode
                            { pkey = directoryNodeDatumFkey
                            , pnext = directoryNodeDatumFNext
                            , ptransferLogicScript = directoryNodeDatumFTransferLogicScript
                            } <-
                            pmatch (pfromData $ punsafeCoerce @(PAsData PDirectorySetNode) (pto directoryNodeDatum'))
                        let currCS = pfromData (pfstBuiltin # csPair)
                            nodeKey = pfromData directoryNodeDatumFkey
                            nodeNext = pfromData directoryNodeDatumFNext
                        pif
                            (nodeKey #< currCS)
                            ( let checks =
                                    pand'List
                                        [ ptraceInfoIfFalse "dir neg-proof node must cover" (currCS #< nodeNext)
                                        , ptraceInfoIfFalse "invalid dir node n" (phasCSH # directoryNodeCS # directoryNodeUTxOFValue)
                                        ]
                               in pif
                                    checks
                                    ( self
                                        # (ptail # proofs)
                                        # (ptail # wdrlIdxs)
                                        # csPairs
                                        # actualProgrammableTokenValue
                                        # cachedTransferScript
                                    )
                                    perror
                            )
                            ( let checks =
                                    pand'List
                                        [ ptraceInfoIfFalse "Missing required transfer script" $
                                            (directoryNodeDatumFTransferLogicScript #== cachedTransferScript)
                                                #|| ( directoryNodeDatumFTransferLogicScript
                                                        #== (pfstBuiltin # (phead # (pdropList # pfromData (phead # wdrlIdxs) # withdrawalEntries)))
                                                    )
                                        , ptraceInfoIfFalse "directory proof mismatch" (nodeKey #== currCS)
                                        , ptraceInfoIfFalse "invalid dir node" (phasCSH # directoryNodeCS # directoryNodeUTxOFValue)
                                        ]
                               in pif
                                    checks
                                    ( self
                                        # (ptail # proofs)
                                        # (ptail # wdrlIdxs)
                                        # csPairs
                                        # (pcons # csPair # actualProgrammableTokenValue)
                                        # directoryNodeDatumFTransferLogicScript
                                    )
                                    perror
                            )
                )
                -- The walk conses matches while traversing the ascending input
                -- list, leaving the accumulator DESCENDING; restore canonical
                -- order (required by the mint-delta union and the containment
                -- subtract walk).
                (pcon $ PValue $ pcon $ PMap $ preverseCurrencyPairs # actualProgrammableTokenValue)
                inputInnerValue
     in go
            # proofList
            # wdrlIdxList
            # mapInnerList
            # pto (pto pemptyLedgerValue)
            # initialCachedTransferScript

-- | Plutarch mirror of 'MintProof' (defined here, ahead of the mint walk that
-- consumes it, because Template Haskell splices further down split the module
-- into scope groups). Constructor indices match the Haskell type: @Member = 0@,
-- @NonMember = 1@.
data PMintProof (s :: S)
    = PMember
    | PNonMember {pnonMemberNodeIdx :: Term s (PAsData PInteger)}
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PMintProof)

{- | Filter the tx mint field down to programmable policies by classifying one
`MintProof` per minted policy (spec §11.3).

High-level purpose:
- Prove which minted or burned policies are programmable and return only those
  signed entries for later output containment checks.

Security invariants:
- `proofList` must be aligned with the currency-symbol order of `totalMintValue`
  (no-omission: one proof per minted policy, no more, no fewer).
- A `Member` proof simply counts its entry toward the base-credential containment
  expectation. It needs NO node reference, datum decode, node authentication, or
  transfer-logic invocation (the §11.3 deletion): a Member claim is self-penalizing
  — it can only ADD the claimant's delta to what must land at the base. Its per-name
  quantity is the ledger-truth mint value, which the attacker cannot inflate.
- A `NonMember` proof MUST reference a covering node whose authenticated
  `(key, next)` interval strictly excludes the minted currency symbol. This is the
  only escape-critical direction (a registered policy must never obtain one), so it
  keeps the covering-interval check and the directory-NFT authentication.
- Missing or extra proofs must fail validation.
-}
pcheckMintLogicAndGetProgrammableValue ::
    Term s PCurrencySymbol ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PMintProof)) ->
    Term s (PValue 'Sorted 'NoGuarantees) ->
    Term s (PValue 'Sorted 'NoGuarantees)
pcheckMintLogicAndGetProgrammableValue directoryNodeCS refInputs proofList totalMintValue =
    let mintedEntries :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger))))
        mintedEntries = pto (pto totalMintValue)
        go = pfix #$ plam $ \self proofs remainingMintEntries programmableMintValue ->
            pelimList
                ( \mintCsPair mintCsPairs ->
                    pelimList
                        ( \mintProofData proofsRest ->
                            let currCS = pfromData (pfstBuiltin # mintCsPair)
                             in pmatch (pfromData mintProofData) $ \case
                                    -- Member: count the entry, touch no node.
                                    PMember ->
                                        self # proofsRest # mintCsPairs # (pcons # mintCsPair # programmableMintValue)
                                    -- NonMember: authenticate a covering directory node.
                                    PNonMember nodeIdx -> P.do
                                        PTxOut{ptxOut'value = directoryNodeUTxOFValue, ptxOut'datum = directoryNodeUTxOFDatum} <-
                                            pmatch $ ptxInInfoResolved (pfromData $ phead # (pdropList # pfromData nodeIdx # refInputs))
                                        POutputDatum paramDat' <- pmatch directoryNodeUTxOFDatum
                                        PDirectorySetNode
                                            { pkey = directoryNodeDatumFkey
                                            , pnext = directoryNodeDatumFNext
                                            } <-
                                            pmatch (pfromData $ punsafeCoerce @(PAsData PDirectorySetNode) (pto paramDat'))
                                        let nodeKey = pfromData directoryNodeDatumFkey
                                            nodeNext = pfromData directoryNodeDatumFNext
                                            checks =
                                                pand'List
                                                    [ ptraceInfoIfFalse "dir mint neg-proof node must cover" (nodeKey #< currCS)
                                                    , ptraceInfoIfFalse "dir mint neg-proof node must cover" (currCS #< nodeNext)
                                                    , ptraceInfoIfFalse "invalid dir node n" (phasCSH # directoryNodeCS # directoryNodeUTxOFValue)
                                                    ]
                                        pif
                                            checks
                                            (self # proofsRest # mintCsPairs # programmableMintValue)
                                            perror
                        )
                        (ptraceInfoError "mint proof missing")
                        proofs
                )
                -- Same reversal note as the transfer walk: the accumulator is
                -- cons-built over the ascending mint entries, so restore
                -- canonical order before it reaches the mint-delta union.
                (pelimList (\_ _ -> ptraceInfoError "extra mint proof") (pcon $ PValue $ pcon $ PMap $ preverseCurrencyPairs # programmableMintValue) proofs)
                remainingMintEntries
     in go # proofList # mintedEntries # pnil

-- | Classification of a single minted currency symbol against the directory
-- (spec §11.3). A @Member@ proof carries no node index: the mint entry is simply
-- counted toward the base-credential containment expectation (self-penalizing —
-- it can only ADD the claimant's delta to what must land at the base). A
-- @NonMember@ proof carries the reference-input index of a covering directory
-- node whose @(key, next)@ interval strictly excludes the symbol, and is the only
-- direction that must be authenticated (a registered policy must never obtain
-- one). Constructor indices are frozen: @Member = 0@, @NonMember = 1@.
data MintProof
    = Member
    | NonMember Integer
    deriving (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
    ''MintProof
    [('Member, 0), ('NonMember, 1)]

data ProgrammableLogicGlobalRedeemer
    = TransferAct
        { plgrTransferProofs :: [Integer]
        , plgrTransferWdrlIdxs :: [Integer]
        -- ^ Per-proof withdrawal index of the policy's transfer-logic script
        -- (scan-proofness: the validator verifies the credential at this index
        -- instead of scanning the withdrawal map).
        , plgrMintProofs :: [MintProof]
        , plgrParamsRefIdx :: Integer
        }
    | SeizeAct
        { plgrDirectoryNodeIdx :: Integer
        , plgrInputIdxs :: [Integer]
        , plgrOutputsStartIdx :: Integer
        , plgrLengthInputIdxs :: Integer
        , plgrSeizeParamsRefIdx :: Integer
        , plgrIssuerWdrlIdx :: Integer
        -- ^ Withdrawal index of the seized policy's issuer-logic script.
        }
    deriving (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
    ''ProgrammableLogicGlobalRedeemer
    [('TransferAct, 0), ('SeizeAct, 1)]

{- | Convert absolute tx-input indexes into the relative-index encoding used by
`SeizeAct`.

High-level purpose:
- Compress seize witnesses so onchain validation can walk the input list in one
  pass without restarting from the head each time.

Security invariants:
- Input indexes must be strictly increasing and non-negative.
- The resulting relative list must identify exactly the same absolute inputs.
- Any malformed witness sequence must be rejected at construction time.
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

{- | Construct a `SeizeAct` redeemer from already-relative input indexes.

High-level purpose:
- Package the third-party transfer witness in the canonical onchain format.

Security invariants:
- All relative indexes must be non-negative.
- `plgrLengthInputIdxs` must equal the true list length.
- The constructor must not reorder or rewrite the supplied witness list.
-}
mkSeizeActRedeemerFromRelativeInputIdxs :: Integer -> [Integer] -> Integer -> Integer -> Integer -> ProgrammableLogicGlobalRedeemer
mkSeizeActRedeemerFromRelativeInputIdxs directoryNodeIdx relativeInputIdxs outputsStartIdx paramsRefIdx issuerWdrlIdx
    | any (< 0) relativeInputIdxs = error "mkSeizeActRedeemerFromRelativeInputIdxs: negative relative index"
    | otherwise =
        SeizeAct
            { plgrDirectoryNodeIdx = directoryNodeIdx
            , plgrInputIdxs = relativeInputIdxs
            , plgrOutputsStartIdx = outputsStartIdx
            , plgrLengthInputIdxs = fromIntegral (length relativeInputIdxs)
            , plgrSeizeParamsRefIdx = paramsRefIdx
            , plgrIssuerWdrlIdx = issuerWdrlIdx
            }

{- | Construct a `SeizeAct` redeemer from absolute input indexes.

High-level purpose:
- Provide the safer offchain API for seize witnesses by deriving the canonical
  relative encoding automatically.

Security invariants:
- The absolute index list must satisfy the invariants of
  `absoluteToRelativeInputIdxs`.
- The resulting redeemer must address the same inputs as the original absolute
  list.
-}
mkSeizeActRedeemerFromAbsoluteInputIdxs :: Integer -> [Integer] -> Integer -> Integer -> Integer -> ProgrammableLogicGlobalRedeemer
mkSeizeActRedeemerFromAbsoluteInputIdxs directoryNodeIdx absoluteInputIdxs =
    mkSeizeActRedeemerFromRelativeInputIdxs
        directoryNodeIdx
        (absoluteToRelativeInputIdxs absoluteInputIdxs)

data PProgrammableLogicGlobalRedeemer (s :: S)
    = PTransferAct
        -- ptransferProofs are reference-input indices for directory nodes (input
        -- side; exact-match vs covering derived onchain from the referenced datum).
        -- ptransferWdrlIdxs are the per-proof withdrawal indices of each policy's
        -- transfer-logic script (verified, never scanned).
        -- pmintProofs are per-minted-symbol Member|NonMember classifications.
        -- pparamsRefIdx indexes the protocol-params reference input.
        { ptransferProofs :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
        , ptransferWdrlIdxs :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
        , pmintProofs :: Term s (PAsData (PBuiltinList (PAsData PMintProof)))
        , pparamsRefIdx :: Term s (PAsData PInteger)
        }
    | -- ptransferProofs correspond to programmable input value entries.
      PSeizeAct
        { pdirectoryNodeIdx :: Term s (PAsData PInteger)
        , pinputIdxs :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
        , poutputsStartIdx :: Term s (PAsData PInteger)
        , plengthInputIdxs :: Term s (PAsData PInteger)
        , pseizeParamsRefIdx :: Term s (PAsData PInteger)
        , pissuerWdrlIdx :: Term s (PAsData PInteger)
        }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PProgrammableLogicGlobalRedeemer)

deriving via
    DeriveDataPLiftable PProgrammableLogicGlobalRedeemer ProgrammableLogicGlobalRedeemer
    instance
        PLiftable PProgrammableLogicGlobalRedeemer

{- | Global stake validator for programmable-token transfers and third-party
seizures.

High-level purpose:
- Enforce the mini-ledger security model for both ordinary `TransferAct`
  transactions and privileged `SeizeAct` transactions.

Security invariants:
- The protocol-parameter reference input must be found and decoded correctly.
-}
mkProgrammableLogicGlobal :: Term s (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
mkProgrammableLogicGlobal = plam $ \protocolParamsCS ctx -> P.do
    PScriptContext{pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <- pmatch ctx
    PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'signatories, ptxInfo'wdrl, ptxInfo'mint, ptxInfo'redeemers} <- pmatch pscriptContext'txInfo
    let red = pfromData $ punsafeCoerce @(PAsData PProgrammableLogicGlobalRedeemer) (pto pscriptContext'redeemer)
    withdrawalEntries <- plet $ pto (pfromData ptxInfo'wdrl)

    pmatch red $ \case
        -- `TransferAct` invariants:
        -- - The expected programmable output value must equal the validated
        --   programmable inputs plus validated programmable mint or burn.
        -- - No programmable value may escape from outputs at `progLogicCred`.
        -- - Transfer and mint proofs must be consumed in lockstep with the
        --   programmable policies they witness.
        PTransferAct transferProofs transferWdrlIdxs mintProofs paramsRefIdx -> P.do
            -- Reference inputs and protocol params are only needed on the transfer
            -- path, so the ref-input decode happens here (not in the shared
            -- preamble). The params UTxO is resolved by the redeemer-supplied
            -- index (§11.3) instead of a scan; a wrong index fails the phasCSH
            -- authentication inside 'pparamsAtRefIdx'.
            referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
            PProgrammableLogicGlobalParams{pdirectoryNodeCS, pprogLogicCred} <-
                pmatch $
                    pparamsAtRefIdx (pfromData protocolParamsCS) referenceInputs (pfromData paramsRefIdx)
            progLogicCred <- plet $ pfromData pprogLogicCred
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
                        (pfromData transferWdrlIdxs)
                        withdrawalEntries
                        cachedTransferScript0
                        totalProgTokenValue
            mintValueNoGuarantees <- plet $ punsafeCoerce @(PValue 'Sorted 'NoGuarantees) (pfromData ptxInfo'mint)
            expectedProgrammableOutputValue <-
                plet $
                    pif
                        (pnull # pto (pto mintValueNoGuarantees))
                        totalProgTokenValue_
                        -- Merge the validated programmable mint/burn delta into the
                        -- transfer value using the raw sorted currency-pair union
                        -- rather than the PValue Semigroup (@#<>@): identical
                        -- asset-wise sum without the PValue normalization overhead.
                        -- The union keeps zero/negative entries (fully or over
                        -- burned assets), so filter them out — the containment
                        -- check requires strictly positive quantities, and a
                        -- non-positive entry requires nothing to remain at the
                        -- mini-ledger outputs. The filter also makes the 'Positive
                        -- coercion below genuinely true.
                        ( pcon $
                            PValue $
                                pcon $
                                    PMap $
                                        pfilterPositiveCurrencyPairs
                                            #$ pcurrencyPairsUnionFast
                                            # pto (pto totalProgTokenValue_)
                                            # pto
                                                ( pto
                                                    ( pcheckMintLogicAndGetProgrammableValue
                                                        (pfromData pdirectoryNodeCS)
                                                        referenceInputs
                                                        (pfromData mintProofs)
                                                        mintValueNoGuarantees
                                                    )
                                                )
                        )

            pvalidateConditions
                [ pisRewardingScript (pdata pscriptContext'scriptInfo)
                , ptraceInfoIfFalse "prog tokens escape" $
                    poutputsContainExpectedValueAtCred
                        progLogicCred
                        (pfromData ptxInfo'outputs)
                        expectedProgrammableOutputValue
                ]
        -- `SeizeAct` invariants:
        -- - Only the seized policy may change across paired programmable
        --   inputs and outputs.
        -- - The resulting seized-policy delta plus mint or burn for that
        --   policy must remain inside the programmable outputs.
        -- - Input witnesses must cover all script spends and match the
        --   redeemer's declared witness count.
        -- Seize is handled by the standalone `mkProgrammableSeize` validator. The
        -- base spend authorizes EITHER the global or the seize credential, so a
        -- seize transaction never invokes this validator — the global handles
        -- transfers only. Reaching here means a malformed transaction routed a
        -- seize redeemer to the global; reject it.
        PSeizeAct{} ->
            ptraceInfoError "global validator does not handle SeizeAct (use the seize validator)"

{- | Standalone `SeizeAct` (mini-ledger clawback) validator.

The seize logic is heavy; keeping it inline in `mkProgrammableLogicGlobal` bloated
that validator's serialised size above the Aiken equivalent. It is therefore
hosted here as its own withdraw-zero (rewarding) validator, parameterized by the
protocol-params state-token currency symbol. `mkProgrammableLogicGlobal` delegates
`SeizeAct` to it by requiring this script's credential in the transaction
withdrawals, so the base spend still forwards to the global while the actual
mini-ledger checks run here — identical behaviour, just relocated so neither
script carries the other's bytes.

All invariants are unchanged from the previous inline implementation (including the
item-1 accounting fix in `pvalueEqualsDeltaCurrencySymbol`).
-}
mkProgrammableSeize :: Term s (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
mkProgrammableSeize = plam $ \protocolParamsCS ctx -> P.do
    PScriptContext{pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <- pmatch ctx
    PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'wdrl, ptxInfo'mint, ptxInfo'redeemers} <- pmatch pscriptContext'txInfo
    let red = pfromData $ punsafeCoerce @(PAsData PProgrammableLogicGlobalRedeemer) (pto pscriptContext'redeemer)
    referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
    withdrawalEntries <- plet $ pto (pfromData ptxInfo'wdrl)
    pmatch red $ \case
        PTransferAct{} -> ptraceInfoError "seize validator invoked with TransferAct"
        -- `pinputIdxs`/`plengthInputIdxs` are no longer read: the seize validator
        -- walks every input and classifies it by credential, so it needs no
        -- redeemer-supplied input index list. Only the directory-node reference
        -- index, the outputs start index, and the params ref index remain (all
        -- verified after lookup).
        PSeizeAct{pdirectoryNodeIdx, poutputsStartIdx, pseizeParamsRefIdx, pissuerWdrlIdx} -> P.do
            PProgrammableLogicGlobalParams{pdirectoryNodeCS, pprogLogicCred} <-
                pmatch $
                    pparamsAtRefIdx (pfromData protocolParamsCS) referenceInputs (pfromData pseizeParamsRefIdx)
            progLogicCred <- plet $ pfromData pprogLogicCred
            let remainingOutputs = pdropList # pfromData poutputsStartIdx # pfromData ptxInfo'outputs
            let directoryNodeUTxO = phead # (pdropList # pfromData pdirectoryNodeIdx # referenceInputs)
            PTxOut{ptxOut'value = seizeDirectoryNodeValue, ptxOut'datum = seizeDirectoryNodeDatum} <- pmatch (ptxInInfoResolved $ pfromData directoryNodeUTxO)
            POutputDatum seizeDat' <- pmatch seizeDirectoryNodeDatum
            PDirectorySetNode
                { pkey = directoryNodeDatumFKey
                , pissuerLogicScript = directoryNodeDatumFIssuerLogicScript
                } <-
                pmatch (pfromData $ punsafeCoerce @(PAsData PDirectorySetNode) (pto seizeDat'))
            mintValueNoGuarantees <- plet $ punsafeCoerce @(PValue 'Sorted 'NoGuarantees) (pfromData ptxInfo'mint)
            seizeMintedTokens <- plet $ ptokensForCurrencySymbol # pfromData directoryNodeDatumFKey # mintValueNoGuarantees
            let conditions =
                    [ pisRewardingScript (pdata pscriptContext'scriptInfo)
                    , ptraceInfoIfFalse "mini-ledger invariants violated" $ processThirdPartyTransfer directoryNodeDatumFKey progLogicCred (pfromData ptxInfo'inputs) remainingOutputs seizeMintedTokens
                    , -- Scan-proof: the redeemer witnesses the issuer withdrawal's
                      -- index; a wrong index resolves to a different credential and
                      -- fails the equality.
                      ptraceInfoIfFalse "issuer logic script must be invoked" $
                        directoryNodeDatumFIssuerLogicScript
                            #== (pfstBuiltin # (phead # (pdropList # pfromData pissuerWdrlIdx # withdrawalEntries)))
                    , ptraceInfoIfFalse "directory node is not valid" $ phasCSH # pfromData pdirectoryNodeCS # seizeDirectoryNodeValue
                    ]
            pvalidateConditions conditions

{- | Extract the token-name map for one currency symbol from a sorted value.

High-level purpose:
- Avoid full-value reconstruction when only one policy entry is relevant to the
  current check.

Security invariants:
- The input value must be sorted by currency symbol.
- The returned token pairs must preserve the original sorted token-name order.
- If the policy is absent, the result must be exactly empty rather than a
  fabricated zero entry.
-}
ptokensForCurrencySymbol ::
    forall anyAmount s.
    Term s (PCurrencySymbol :--> PValue 'Sorted anyAmount :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
ptokensForCurrencySymbol =
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

{- | Check whether `actualTokens` contains at least the signed quantities required
by `requiredTokens`.

High-level purpose:
- Provide a policy-local containment check for seize-path balance invariants.

Security invariants:
- Both inputs must be sorted by token name.
- Positive required quantities must only pass when enough quantity is present in
  `actualTokens`.
- Negative required quantities must be treated as already satisfied by zero, since
  they represent value removed from the required remainder.
- The helper must not introduce false positives by skipping unmatched positive
  requirements.
-}
ptokenPairsContain ::
    Term
        s
        ( PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
            :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
            :--> PBool
        )
ptokenPairsContain = phoistAcyclic $
    pfix #$ plam $ \self actualTokens requiredTokens ->
        pelimList
            ( \requiredPair requiredRest ->
                let requiredTokenName = pfromData (pfstBuiltin # requiredPair)
                    requiredQty = pfromData (psndBuiltin # requiredPair)
                 in pelimList
                        ( \actualPair actualRest ->
                            let actualTokenName = pfromData (pfstBuiltin # actualPair)
                                actualQty = pfromData (psndBuiltin # actualPair)
                             in pif
                                    (actualTokenName #== requiredTokenName)
                                    (pif (actualQty #>= requiredQty) (self # actualRest # requiredRest) (pconstant False))
                                    ( pif
                                        (actualTokenName #< requiredTokenName)
                                        (self # actualRest # requiredTokens)
                                        (pif (0 #>= requiredQty) (self # actualTokens # requiredRest) (pconstant False))
                                    )
                        )
                        (pif (0 #>= requiredQty) (self # pnil # requiredRest) (pconstant False))
                        actualTokens
            )
            (pconstant True)
            requiredTokens

{- | Validate one corresponding programmable input/output pair in the seize path
and accumulate the delta for the seized policy.

High-level purpose:
- Prove that a witness-selected programmable input keeps the same address, datum,
  and reference script in its paired output, with only the seized policy allowed
  to change.

Security invariants:
- Only inputs at `progLogicCred` may be treated as programmable inputs.
- The paired output must preserve address, datum, and reference script exactly.
- All non-target policies must remain unchanged, enforced by
  `pvalueEqualsDeltaCurrencySymbol`.
- Witness indexes that point to pubkey inputs must fail.
- The accumulated delta must contain only the seized policy.
-}
pcheckCorrespondingThirdPartyTransferInputsAndOutputs ::
    Term s PCurrencySymbol ->
    Term s PCredential ->
    Term _ (PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PAsData PTxOut) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBool) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))) ->
    Term s PTxOut ->
    Term s PBool
pcheckCorrespondingThirdPartyTransferInputsAndOutputs programmableCS progLogicCred self remainingInputs programmableOutputs deltaAccumulator programmableInputResolved =
    -- Classify the input by payment credential using ONLY the input address; the
    -- (more expensive) output pairing and value extraction is deferred into the
    -- base-credential branch. This keeps the per-input skip cost minimal — critical
    -- now that every transaction input is walked (e.g. many fee/pubkey inputs).
    plet (psndBuiltin # (pasConstr # pforgetData (pdata programmableInputResolved))) $ \inputTxOutFields ->
        plet (phead # inputTxOutFields) $ \inputTxOutAddress ->
            let inputCredentialData = phead # (psndBuiltin # (pasConstr # inputTxOutAddress))
             in pif
                    (inputCredentialData #== pforgetData (pdata progLogicCred))
                    -- Programmable (base-credential) input: pair it with the next
                    -- remaining output and accumulate the seized-policy delta.
                    ( plet (psndBuiltin # (pasConstr # pforgetData (phead # programmableOutputs))) $ \outputTxOutFields ->
                        plet (ptail # inputTxOutFields) $ \inputTxOutFieldsRest ->
                            plet (ptail # outputTxOutFields) $ \outputTxOutFieldsRest ->
                                let outputTxOutAddress = phead # outputTxOutFields
                                    programmableInputValue = punsafeCoerce @(PAsData (PValue 'Sorted 'Positive)) (phead # inputTxOutFieldsRest)
                                    programmableOutputValue = punsafeCoerce @(PAsData (PValue 'Sorted 'Positive)) (phead # outputTxOutFieldsRest)
                                    programmableInputRest = ptail # inputTxOutFieldsRest
                                    programmableOutputRest = ptail # outputTxOutFieldsRest
                                 in pif
                                        ( pand'List
                                            [ ptraceInfoIfFalse "corresponding output: address mismatch" $
                                                inputTxOutAddress #== outputTxOutAddress
                                            , ptraceInfoIfFalse "corresponding output: datum/reference script mismatch" $
                                                programmableInputRest #== programmableOutputRest
                                            ]
                                        )
                                        ( let delta = pvalueEqualsDeltaCurrencySymbol programmableCS programmableInputValue programmableOutputValue
                                           in self # remainingInputs # (ptail # programmableOutputs) # (ptokenPairsUnionFast # delta # deltaAccumulator)
                                        )
                                        perror
                    )
                    -- Any non-base input (fee/pubkey or unrelated script input) is
                    -- skipped without consuming an output. Walking every input means
                    -- all base inputs are necessarily covered — no redeemer-supplied
                    -- index list, and no compensating spend-redeemer count check.
                    (self # remainingInputs # programmableOutputs # deltaAccumulator)

{- | Validate the full `SeizeAct` mini-ledger transformation.

High-level purpose:
- Consume the witness-selected programmable inputs and continuing outputs, compute
  the net seized-policy delta, include mint/burn for that same policy, and prove
  that the remainder still lives at programmable outputs.

Security invariants:
- The witness list must be interpreted as relative indexes over the remaining
  input suffix.
- Only the seized policy may vary across corresponding input/output pairs.
- The final delta plus minted tokens for the seized policy must be contained in
  the remaining programmable outputs.
- Outputs at other payment credentials must never satisfy the balance invariant.
-}
processThirdPartyTransfer ::
    Term s (PAsData PCurrencySymbol) ->
    Term s PCredential ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))) ->
    Term s PBool
processThirdPartyTransfer programmableCS progLogicCred inputs progOutputs mintedTokens =
    let
        programmableCS' = pfromData programmableCS
        checkBalanceInvariant :: Term _ (PBuiltinList (PAsData PTxOut)) -> Term _ (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))) -> Term _ PBool
        checkBalanceInvariant remainingOutputs deltaAccumulatorResult =
            let outputAccumulatorResult = go2 # remainingOutputs
             in pif
                    (ptokenPairsContain # outputAccumulatorResult # deltaAccumulatorResult)
                    (pconstant True)
                    perror

        go2 :: Term _ (PBuiltinList (PAsData PTxOut) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
        go2 = pfix #$ plam $ \self programmableOutputs ->
            pelimList
                ( \programmableOutput programmableOutputsRest ->
                    pmatch (pfromData programmableOutput) $ \(PTxOut{ptxOut'address = programmableOutputAddress, ptxOut'value = programmableOutputValue}) ->
                        pif
                            (paddressCredential programmableOutputAddress #== progLogicCred)
                            (ptokenPairsUnionFast # (ptokensForCurrencySymbol # programmableCS' # pfromData programmableOutputValue) # (self # programmableOutputsRest))
                            (self # programmableOutputsRest)
                )
                pnil
                programmableOutputs

        -- Walk EVERY transaction input. Base-credential inputs are paired with the
        -- continuing outputs in order; all other inputs are skipped. This removes
        -- the redeemer-supplied input index list (and its compensating
        -- spend-redeemer-count check) entirely: coverage of all programmable inputs
        -- is now structural rather than trusted.
        go :: Term _ (PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PAsData PTxOut) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBool)
        go = pfix #$ plam $ \self remainingInputs programmableOutputs deltaAccumulator ->
            pelimList
                ( \txIn remainingInputsRest ->
                    plet (ptxInInfoResolved $ pfromData txIn) $ \programmableInputResolved ->
                        pcheckCorrespondingThirdPartyTransferInputsAndOutputs
                            programmableCS'
                            progLogicCred
                            self
                            remainingInputsRest
                            programmableOutputs
                            deltaAccumulator
                            programmableInputResolved
                )
                (checkBalanceInvariant programmableOutputs (ptokenPairsUnionFast # deltaAccumulator # mintedTokens))
                remainingInputs
     in
        go # inputs # progOutputs # pnil

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

{- | Negate every signed quantity in a sorted token-name map.

High-level purpose:
- Reuse the same token-pair representation for both positive and negative deltas.

Security invariants:
- Token names and ordering must be preserved exactly.
- Each quantity must be negated exactly once.
-}
pnegateTokens :: Term _ (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
pnegateTokens = phoistAcyclic $ pfix #$ plam $ \self tokens ->
    pelimList
        ( \tokenPair tokensRest ->
            let tokenName = pfstBuiltin # tokenPair
                tokenAmount = psndBuiltin # tokenPair
             in pcons # (ppairDataBuiltin # tokenName # pdata (pconstantInteger 0 - pfromData tokenAmount)) # (self # tokensRest)
        )
        pnil
        tokens

{- | Compare two values, require equality everywhere except one policy, and return
that policy's signed delta.

High-level purpose:
- Encode the core seize-path invariant that only one currency symbol may change
  across a corresponding input/output pair.

Security invariants:
- Any difference outside `progCS` must fail validation.
- The returned token list must contain only entries for `progCS`.
- Each returned quantity must equal `inputQty - outputQty`, including negative
  results when the output gained more than the input held.
- Zero deltas must be omitted so downstream unions and containment checks operate
  on canonical sparse maps.
-}
-- | Does the (already-decoded, CS-sorted) currency list contain the target policy?
-- Early-exits once the sorted list passes the target. Phoisted, so the per-pair
-- seize check pays no allocation and reuses the value decode already performed by
-- `pvalueEqualsDeltaCurrencySymbol`.
pcurrencyListHasCS ::
    forall anyOrder s.
    Term s (PCurrencySymbol :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap anyOrder PTokenName PInteger))) :--> PBool)
pcurrencyListHasCS = phoistAcyclic $ plam $ \targetCS ->
    pfix #$ plam $ \self entries ->
        pelimList
            ( \entry rest ->
                plet (pfromData (pfstBuiltin # entry)) $ \cs ->
                    pif (cs #== targetCS) (pconstant True) (pif (targetCS #< cs) (pconstant False) (self # rest))
            )
            (pconstant False)
            entries

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
                                    -- output exhausted → emit the current input token and the
                                    -- remaining input tokens as positive (nonzero by invariant).
                                    -- NB: must re-emit `inputPair`; returning `inputRest` alone
                                    -- silently dropped the current token, letting a seize move it
                                    -- out of the base address undetected.
                                    (pcons # inputPair # inputRest)
                                    outputTokens
                    )
                    -- input exhausted → emit remaining output tokens as negative (nonzero by invariant)
                    (pnegateTokens # outputTokens)
                    inputTokens

        -- | Remaining currency-symbol entries when one value list is exhausted while
        -- the other still holds entries. Because a sorted value contains each policy
        -- at most once, the leftover is either empty or a single entry that MUST be
        -- the seized policy (a fully added/removed progCS holding). Anything else is
        -- value moved outside the seized policy — illegal, so `perror`. Non-recursive
        -- (no per-pair closure allocation); `emit` maps the progCS token map to the
        -- signed delta.
        remainingProgCSDelta ::
            ( Term _ (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))) ->
              Term _ (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
            ) ->
            Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap anyOrder PTokenName PInteger)))) ->
            Term _ (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
        remainingProgCSDelta emit entries =
            pelimList
                ( \entry rest ->
                    pif
                        (pfromData (pfstBuiltin # entry) #== progCS)
                        -- exactly one leftover entry allowed (the seized policy); any
                        -- further leftover is illegal value movement.
                        (pelimList (\_ _ -> perror) (emit (pto (pfromData @(PMap anyOrder PTokenName PInteger) (psndBuiltin # entry)))) rest)
                        perror
                )
                pnil
                entries

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
                                    ( -- Currency symbols differ: the smaller-CS side holds a policy
                                      -- the other side lacks. That policy MUST be the seized progCS
                                      -- (a full add/remove); any other divergence is illegal value
                                      -- movement outside the seized policy.
                                      pif
                                        (pfromData inputValueEntryCS #< pfromData (pfstBuiltin # outputValueEntry))
                                        ( pif
                                            (pfromData inputValueEntryCS #== progCS)
                                            (ptokenPairsUnionFast # (psubtractTokens # pto (pfromData @(PMap anyOrder PTokenName PInteger) (psndBuiltin # inputValueEntry)) # pnil) # (self # inputValueEntries # outputValuePairs # diffAccumulator))
                                            perror
                                        )
                                        ( pif
                                            (pfromData (pfstBuiltin # outputValueEntry) #== progCS)
                                            (ptokenPairsUnionFast # (pnegateTokens # pto (pfromData @(PMap anyOrder PTokenName PInteger) (psndBuiltin # outputValueEntry))) # (self # inputValuePairs # outputValueEntries # diffAccumulator))
                                            perror
                                        )
                                    )
                            )
                            (ptokenPairsUnionFast # remainingProgCSDelta id inputValuePairs # diffAccumulator)
                            outputValuePairs
                )
                (ptokenPairsUnionFast # remainingProgCSDelta (\toks -> pnegateTokens # toks) outputValuePairs # diffAccumulator)
                inputValuePairs
     in -- Non-contamination (Aiken Finding 12): the seized input must actually hold
        -- the seized policy. Checked here (reusing the decoded input list) so no
        -- extra decode or per-pair allocation is needed.
        pif
            (pcurrencyListHasCS # progCS # innerInputValue)
            (goOuter # innerInputValue # innerOutputValue # pnil)
            (ptraceInfoError "seize: paired input does not hold the seized policy")
