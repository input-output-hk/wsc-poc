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
    BaseSpendRedeemer (..),
    mkProgrammableLogicBase,
    mkProgrammableLogicGlobal,
    mkProgrammableSeize,
    pparamsAtRefIdx,
    pisScriptInvokedEntries,
    pvalueEqualsDeltaCurrencySymbol,
    pvalueFromCred,
    pvalueToCred,
    poutputsContainExpectedValueAtCred,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Integer (pconstantInteger)
import Plutarch.Core.Context (
    ptxInInfoResolved,
    ptxOutDatum,
    ptxOutValue,
 )
import Plutarch.Core.Integrity (pisRewardingScript)
import Plutarch.Core.Internal.Builtins (pmapData, ppairDataBuiltinRaw)
import SmartTokens.Core.Builtins (pdropList)
import Plutarch.Builtin.Value (pinsertCoin, pscaleValue, punValueData, punionValue, pvalueData)
import Plutarch.Builtin.Value qualified as BuiltinValue
import Plutarch.Core.Utils
import Plutarch.Core.ValidationLogic hiding (pemptyLedgerValue, pvalidateConditions, pvalueFromCred, pvalueToCred)
import Plutarch.Core.Value (pledgerValueCsPairs, pmkSortedValue, ptokenPairs,
                            punsortedMapPairs, pvalueCsPairs)
import Plutarch.Internal.Case (punsafeCase)
import Plutarch.Internal.Lift
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3
import Plutarch.LedgerApi.Value qualified as Value
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

{- | Fold conditions with right-nested Case instead of pcondsAll's applied
pand' closure: each condition costs one Case step here versus ~5-6 machine
steps of closure application there. The first false condition short-circuits
the rest; since every caller rejects on False (perror), skipping a
later condition that would itself have errored changes nothing observable.
-}
pcondsAll :: [Term s PBool] -> Term s PBool
pcondsAll [] = pconstant True
pcondsAll [x] = x -- a singleton IS its own conjunction; wrapping it in a Case costs 2 steps per evaluation, which the base validator pays per input
pcondsAll (x : xs) = pif x (pcondsAll xs) (pconstant False)

-- | 'Plutarch.Core.ValidationLogic.pvalidateConditions' over 'pcondsAll'.
pvalidateConditions' :: [Term s PBool] -> Term s PUnit
pvalidateConditions' conds = pif (pcondsAll conds) (pconstant ()) perror

-- TODO: Replace current corresponding input / output comparison (which compares address, reference script and datum) for multi-seize
-- with constructing the expected output from the input with this function and comparing it to the actual output.
-- Further optimize this with the optimization in the "Everything is possible" UPLC fest presentation.
-- pconstructExpectedOutputWithOutputDatum :: Term s PAddress -> Term s (PAsData PLedgerValue) -> Term s POutputDatum -> Term s (PAsData PTxOut)
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
-}
pemptyProgValue :: Term s PSortedValue
pemptyProgValue = Value.pemptySortedValue

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
    forall (s :: S).
    Term s PLedgerValue -> Term s PSortedValue
pstripAdaH value =
    let nonAdaValueMapInner = ptail # pledgerValueCsPairs value
     in pmkSortedValue nonAdaValueMapInner

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
    pfix $ \self -> plam $ \tokensA tokensB ->
        pelimList
            ( \tokenPairA tokensARest ->
                pelimList
                    ( \tokenPairB tokensBRest ->
                        -- Case-destructure each pair once (PV11): one Case
                        -- binding both components replaces FstPair+SndPair
                        -- builtin calls, and unlike a Haskell-level `let` of
                        -- `pfstBuiltin # p` the binding cannot be re-evaluated
                        -- at each use site.
                        pmatch tokenPairA $ \(PBuiltinPair tokenNameA quantityAD) ->
                            pmatch tokenPairB $ \(PBuiltinPair tokenNameB quantityBD) ->
                                let tokenNameABytes = pasByteStr # pforgetData tokenNameA
                                    tokenNameBBytes = pasByteStr # pforgetData tokenNameB
                                 in pif
                                        (tokenNameABytes #== tokenNameBBytes)
                                        ( pcons
                                            # (ppairDataBuiltin # tokenNameA # pdata (pfromData quantityAD + pfromData quantityBD))
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
        ( PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (AssocMap.PSortedMap PTokenName PInteger)))
            :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (AssocMap.PSortedMap PTokenName PInteger)))
            :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (AssocMap.PSortedMap PTokenName PInteger)))
        )
pcurrencyPairsUnionFast = phoistAcyclic $
    pfix $ \self -> plam $ \csPairsA csPairsB ->
        pelimList
            ( \csPairA csPairsARest ->
                pelimList
                    ( \csPairB csPairsBRest ->
                        -- One Case per pair instead of FstPair (+ SndPair on
                        -- the merge path); see the note in
                        -- ptokenPairsUnionFast.
                        pmatch csPairA $ \(PBuiltinPair currencySymbolA tokenMapAD) ->
                            pmatch csPairB $ \(PBuiltinPair currencySymbolB tokenMapBD) ->
                                let currencySymbolABytes = pasByteStr # pforgetData currencySymbolA
                                    currencySymbolBBytes = pasByteStr # pforgetData currencySymbolB
                                 in pif
                                        (currencySymbolABytes #== currencySymbolBBytes)
                                        ( let mergedTokenPairs =
                                                ptokenPairsUnionFast
                                                    # ptokenPairs (pfromData tokenMapAD)
                                                    # ptokenPairs (pfromData tokenMapBD)
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

{- | Add two non-Ada sorted `Value`s while preserving canonical ordering.

High-level purpose:
- Centralize asset-wise value addition used throughout transfer and mint
  accounting.

Security invariants:
- Callers must only pass sorted positive values.
- No asset may be dropped, duplicated, or reordered outside the merge rules.
- The output must remain a valid sorted positive value.
-}
pvalueUnionFast :: Term s (PSortedValue :--> PSortedValue :--> PSortedValue)
pvalueUnionFast = phoistAcyclic $ plam $ \valueA valueB ->
    pmkSortedValue $
        pcurrencyPairsUnionFast
            # pvalueCsPairs valueA
            # pvalueCsPairs valueB

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
NOTE: no validator calls this any more. The transfer path used to search this
map for each script-owned input's owner, which made an issuer's cost depend on
where their script hash sorted against the other participants' -- including
this validator's own. 'TransferAct' now witnesses the index instead. This is
retained as the benchmark's scan baseline (decision.h.owner.* in the function
benchmark), which is what justifies that redeemer field.
-}
pisScriptInvokedEntries :: Term s (PAsData PCredential :--> PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace)) :--> PBool)
pisScriptInvokedEntries = phoistAcyclic $ plam $ \scriptCredData withdrawalEntries ->
    let go = pfix $ \self -> plam $ \entries ->
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
    -- Withdrawal indices of the script owners, in input order. Consumed only by
    -- script-owned inputs; a pubkey owner is witnessed by its signature and
    -- takes no entry.
    Term s (PBuiltinList (PAsData PInteger)) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    -- Returns the accumulated non-Ada currency-pair list (sorted), same shape
    -- the lockstep proof walk consumes. Hybrid accumulation strategy:
    -- zero or one contributing input extracts the raw pairs directly (one
    -- UnMapData + tail, exactly the pre-PV11 cost); two or more inputs switch
    -- to the PV11 builtin Value — one unValueData per input and near-constant
    -- memory unionValue merges — which removes the quadratic sorted-merge
    -- component on the inputs axis, then bridges back to pairs once
    -- (insertCoin amount 0 deletes the ada entry).
    Term s (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (AssocMap.PSortedMap PTokenName PInteger))))
pvalueFromCred cred sigs withdrawalEntries ownerWdrlIdxs inputs =
    let credData = pforgetData (pdata cred)

        -- Shared per-input gate: k receives the input's raw value Data iff the
        -- input sits at `cred` and its owner witness is present; otherwise
        -- skip receives the rest of the walk. Haskell-level, so it inlines
        -- into each of the three loop bodies below.
        withContributing ::
            Term _ (PAsData PTxInInfo) ->
            Term _ (PBuiltinList (PAsData PInteger)) ->
            (Term _ PData -> Term _ (PBuiltinList (PAsData PInteger)) -> Term _ r) ->
            (Term _ (PBuiltinList (PAsData PInteger)) -> Term _ r) ->
            Term _ r
        -- The address fields are 'plet'-bound because BOTH the payment credential
        -- and the staking credential are read out of them; left unshared, the
        -- address is 'unConstrData'-ed twice for every input the transaction
        -- carries.
        withContributing txIn idxs k skip =
            plet (pdata (ptxInInfoResolved $ pfromData txIn)) $ \resolvedOutData ->
                pmatch (pasConstr # pforgetData resolvedOutData) $ \(PBuiltinPair _ resolvedOutFields) ->
                  pheadTailBuiltin resolvedOutFields $ \resolvedOutAddrData resolvedOutFieldsRest ->
                   pmatch (pasConstr # resolvedOutAddrData) $ \(PBuiltinPair _ resolvedOutAddressFields) ->
                    let resolvedOutValueData = phead # resolvedOutFieldsRest
                        paymentCredData = phead # resolvedOutAddressFields
                     in pif
                            (paymentCredData #== credData)
                            -- Reach the owner credential as Data instead of
                            -- decoding it. The withdrawal map is keyed by
                            -- credential Data, so a script owner can be looked up
                            -- with the bytes already in hand; decoding to a
                            -- 'PCredential' only to rebuild it with 'pdata . pcon'
                            -- paid a constrData/listData/bData re-encode on every
                            -- script-owned input.
                            --
                            -- Fail-closed on anything that is not
                            -- @Just (StakingHash _)@: 'Nothing' has no fields so
                            -- 'phead' errors, and a 'StakingPtr' holds integers so
                            -- the following 'pasConstr' errors. An unstaked or
                            -- pointer-staked mini-ledger UTxO has no owner to
                            -- witness and must not be spendable.
                            ( pmatch (pasConstr # (phead # (ptail # resolvedOutAddressFields))) $ \(PBuiltinPair _ stakingFields) ->
                                pmatch (pasConstr # (phead # stakingFields)) $ \(PBuiltinPair _ stakingHashFields) ->
                                    plet (phead # stakingHashFields) $ \ownerCredData ->
                                     pmatch (pasConstr # ownerCredData) $ \(PBuiltinPair ownerCredTag ownerCredFields) ->
                                        -- Integer Case on the credential tag
                                        -- (0 = PubKeyCredential, 1 = ScriptCredential;
                                        -- ledger-validated addresses admit no other
                                        -- tag, and the Case errors if one appears).
                                        punsafeCase
                                            ownerCredTag
                                            [ popaque
                                                ( pif
                                                    (ptxSignedByPkh # punsafeCoerce (phead # ownerCredFields) # sigs)
                                                    (k resolvedOutValueData idxs)
                                                    (ptraceInfoError "Missing required pk witness")
                                                )
                                            , -- Scan-proof: the redeemer witnesses
                                              -- where this owner's withdrawal sits,
                                              -- so the check is one comparison at a
                                              -- known position rather than a search.
                                              -- Self-validating: a wrong index
                                              -- resolves to some other credential and
                                              -- fails this equality, and a misaligned
                                              -- list fails the same way at the next
                                              -- script-owned input.
                                              popaque
                                                ( pif
                                                    ( ownerCredData
                                                        #== pforgetData
                                                            (pmatch (phead # (pdropList # pfromData (phead # idxs) # withdrawalEntries)) (\(PBuiltinPair wCredD _) -> wCredD))
                                                    )
                                                    (k resolvedOutValueData (ptail # idxs))
                                                    (ptraceInfoError "Missing required script witness")
                                                )
                                            ]
                            )
                            (skip idxs)

        -- Phase 3: two or more contributing inputs seen; accumulate builtin.
        goBuiltin = pfix $ \self -> plam $ \acc idxs remaining ->
            pelimList
                ( \txIn xs ->
                    withContributing
                        txIn
                        idxs
                        (\vd idxs' -> self # (punionValue # acc # (punValueData # vd)) # idxs' # xs)
                        (\idxs' -> self # acc # idxs' # xs)
                )
                ( punsafeCoerce
                    @(PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (AssocMap.PSortedMap PTokenName PInteger))))
                    (pasMap # (pvalueData # (pinsertCoin # pconstant "" # pconstant "" # 0 # acc)))
                )
                remaining
        -- Phase 2: exactly one contributing input so far (raw value Data held).
        goRest = pfix $ \self -> plam $ \firstVd idxs remaining ->
            pelimList
                ( \txIn xs ->
                    withContributing
                        txIn
                        idxs
                        (\vd idxs' -> goBuiltin # (punionValue # (punValueData # firstVd) # (punValueData # vd)) # idxs' # xs)
                        (\idxs' -> self # firstVd # idxs' # xs)
                )
                ( punsafeCoerce
                    @(PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (AssocMap.PSortedMap PTokenName PInteger))))
                    (ptail # (pasMap # firstVd))
                )
                remaining
        -- Phase 1: no contributing input seen yet.
        goFind = pfix $ \self -> plam $ \idxs remaining ->
            pelimList
                ( \txIn xs ->
                    withContributing
                        txIn
                        idxs
                        (\vd idxs' -> goRest # vd # idxs' # xs)
                        (\idxs' -> self # idxs' # xs)
                )
                pnil
                remaining
     in goFind # ownerWdrlIdxs # inputs

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
    Term s PSortedValue
pvalueToCred cred inputs =
    let credData = pforgetData (pdata cred)
     in ( pfix $ \self -> plam $ \acc ->
            pelimList
                ( \txOut xs ->
                    pmatch (pasConstr # pforgetData txOut) $ \(PBuiltinPair _ txOutFields) ->
                        pheadTailBuiltin txOutFields $ \txOutAddress txOutFieldsRest ->
                            let txOutValue = punsafeCoerce @(PAsData PLedgerValue) (phead # txOutFieldsRest)
                                paymentCredData = pmatch (pasConstr # txOutAddress) (\(PBuiltinPair _ addrFields) -> phead # addrFields)
                             in self
                                    # pif (paymentCredData #== credData) (pvalueUnionFast # acc # pstripAdaH (pfromData txOutValue)) acc
                                    # xs
                )
                acc
        )
            # pemptyProgValue
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
    Term s PSortedValue ->
    Term s PBool
poutputsContainExpectedValueAtCred progLogicCred txOutputs expectedValue =
    let
        passetQtyInPairs ::
            Term
                _
                ( PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (AssocMap.PSortedMap PTokenName PInteger)))
                    :--> PCurrencySymbol
                    :--> PTokenName
                    :--> PInteger
                )
        passetQtyInPairs = phoistAcyclic $ plam $ \csPairs cs tn ->
            let tokenQtyInTokenPairs = pfix $ \self -> plam $ \remainingTokenPairs ->
                    pelimList
                        ( \tokenPair tokenPairsRest ->
                            -- One Case for both components; cheaper than even a
                            -- single FstPair call, so the mismatch path wins too.
                            pmatch tokenPair $ \(PBuiltinPair tokenNameD tokenQtyD) ->
                                pif
                                    (pfromData tokenNameD #== tn)
                                    (pfromData tokenQtyD)
                                    ( pif
                                        (tn #< pfromData tokenNameD)
                                        0
                                        (self # tokenPairsRest)
                                    )
                        )
                        0
                        remainingTokenPairs
                tokenQtyInCurrencyPairs = pfix $ \self -> plam $ \remainingCurrencyPairs ->
                    pelimList
                        ( \currencyPair currencyPairsRest ->
                            pmatch currencyPair $ \(PBuiltinPair currencySymbolD tokenMapD) ->
                                pif
                                    (pfromData currencySymbolD #== cs)
                                    (tokenQtyInTokenPairs # ptokenPairs (pfromData tokenMapD))
                                    ( pif
                                        (cs #< pfromData currencySymbolD)
                                        0
                                        (self # currencyPairsRest)
                                    )
                        )
                        0
                        remainingCurrencyPairs
             in tokenQtyInCurrencyPairs # csPairs
        hasAtLeastAssetInProgOutputs = pfix $ \self -> plam $ \requiredQty currentQty cs tn remainingOutputs ->
            pif
                (currentQty #>= requiredQty)
                (pconstant True)
                ( pelimList
                    ( \txOut outputsRest ->
                        -- Index the constructor and compare the payment credential
                        -- as Data, matching the other two output walks. Plutarch's
                        -- typed PEq on PCredential expands to 'unConstrData' on both
                        -- sides plus a tag comparison; one 'equalsData' is cheaper on
                        -- every input shape (see decision.f.cred.* and
                        -- decision.g.contain.* in the function benchmark).
                        pmatch (pasConstr # pforgetData txOut) $ \(PBuiltinPair _ txOutFields) ->
                            pheadTailBuiltin txOutFields $ \txOutAddress txOutFieldsRest ->
                                let paymentCredData = pmatch (pasConstr # txOutAddress) (\(PBuiltinPair _ addrFields) -> phead # addrFields)
                                    txOutValueData = phead # txOutFieldsRest
                                 in pif
                                        (paymentCredData #== progLogicCredData)
                                        (self # requiredQty # (currentQty + (passetQtyInPairs # punsafeCoerce (pasMap # txOutValueData) # cs # tn)) # cs # tn # outputsRest)
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
        accumulateOutputsAtCred = pfix $ \self -> plam $ \acc remainingOutputs ->
            pelimList
                ( \txOut outputsRest ->
                    pmatch (pasConstr # pforgetData txOut) $ \(PBuiltinPair _ txOutFields) ->
                        pheadTailBuiltin txOutFields $ \txOutAddress txOutFieldsRest ->
                            let txOutValueData = phead # txOutFieldsRest
                                paymentCredData = pmatch (pasConstr # txOutAddress) (\(PBuiltinPair _ addrFields) -> phead # addrFields)
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
        checkWholesaleThenBuiltin = pfix $ \self -> plam $ \remainingOutputs ->
            pelimList
                ( \txOut outputsRest ->
                    pmatch (pasConstr # pforgetData txOut) $ \(PBuiltinPair _ txOutFields) ->
                        pheadTailBuiltin txOutFields $ \txOutAddress txOutFieldsRest ->
                            let txOutValueData = phead # txOutFieldsRest
                                paymentCredData = pmatch (pasConstr # txOutAddress) (\(PBuiltinPair _ addrFields) -> phead # addrFields)
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
        expectedCsPairs = pvalueCsPairs expectedValue
     in -- Dispatch: exactly one expected asset (one currency symbol with one
        -- token name — the dominant transfer shape) takes the accumulate-scan
        -- fast path; everything else takes the single-pass subtract walk.
        pelimList
            ( \csPair csPairsRest ->
                pmatch csPair $ \(PBuiltinPair csD tokenMapD) ->
                    plet (ptokenPairs (pfromData tokenMapD)) $ \tnPairs ->
                        pif
                            (pif (pnull # csPairsRest) (pelimList (\_ tnRest -> pnull # tnRest) (pconstant False) tnPairs) (pconstant False))
                            ( pelimList
                                ( \tnPair _ ->
                                    pmatch tnPair $ \(PBuiltinPair tnD tnQtyD) ->
                                        hasAtLeastAssetInProgOutputs
                                            # pfromData tnQtyD
                                            # 0
                                            # pfromData csD
                                            # pfromData tnD
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

The spend witnesses WHICH of the two validators it delegates to and at WHICH
withdrawal index, so this runs a single credential comparison at a known
position instead of scanning. Both halves of the witness are self-validating: a
wrong index resolves to some other credential and a wrong arm names the other
validator, and either way the equality fails, so a dishonest witness can only
invalidate its own transaction.

The index is load-bearing rather than a micro-optimisation. The withdrawal map
is sorted by credential, and the other withdrawal a seize transaction always
carries is the seized token's issuer-logic script — a hash the ISSUER chooses,
not the protocol. Whether it sorts before or after this validator's credential
therefore decided how far the old scan walked, and that is worth roughly 4.2M
CPU per spend, multiplied by every programmable input in the transaction.
-}

-- | Which of the two stake validators a base spend delegates to, and the index
-- of that validator's entry in the (credential-sorted) withdrawal map.
data BaseSpendRedeemer
    = SpendViaGlobal Integer
    | SpendViaSeize Integer
    deriving (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed ''BaseSpendRedeemer [('SpendViaGlobal, 0), ('SpendViaSeize, 1)]

mkProgrammableLogicBase :: Term s (PAsData PCredential :--> PAsData PCredential :--> PScriptContext :--> PUnit)
mkProgrammableLogicBase = plam $ \globalCred seizeCred ctx -> P.do
    -- This validator runs once per programmable input, so its cost is multiplied
    -- by every input in the transaction and is worth reaching for the two fields
    -- it needs by hand. 'pmatch' on 'PScriptContext'/'PTxInfo' walks the field
    -- list one 'tailList' at a time; one 'dropList' covers the same distance in a
    -- single builtin call. Both shared subterms are 'plet'-bound: without that
    -- the context's 'unConstrData' is duplicated and the hand-rolled walk is
    -- SLOWER than 'pmatch', not faster.
    PBuiltinPair _ ctxFields <- pmatch $ pasConstr # pforgetData (punsafeCoerce @(PAsData PScriptContext) ctx)
    pheadTailBuiltin ctxFields $ \txInfoData ctxFieldsRest -> P.do
        PBuiltinPair witnessTag witnessFields <- pmatch $ pasConstr # (phead # ctxFieldsRest)
        -- Field 6 of the Plutus V3 'TxInfo' constructor is 'wdrl' (inputs, refInputs,
        -- outputs, fee, mint, txCerts, wdrl, ...). This index is part of the V3
        -- ledger ABI and changes only with a new script language version, which would
        -- require a new script anyway. A wrong index is not a silent weakening: it
        -- resolves to a field of the wrong shape and 'pasMap' errors.
        let wdrls = pasMap # (phead # (pdropList # pconstantInteger 6 # pmatch (pasConstr # txInfoData) (\(PBuiltinPair _ txInfoFields) -> txInfoFields)))
            -- Integer Case on the redeemer's constructor tag: branch 0 is
            -- SpendViaGlobal, branch 1 SpendViaSeize. Strictly tighter than the
            -- pif (#== 0) it replaces: that sent EVERY nonzero tag down the
            -- seize arm, so a malformed tag (2+) could ride along whenever a
            -- genuine seize was witnessed; the Case errors on any tag outside
            -- 0..1. Anything this validator accepts still requires the claimed
            -- arm's withdrawal at the witnessed index below.
            claimed = punsafeCase witnessTag [popaque (pforgetData globalCred), popaque (pforgetData seizeCred)]
            witnessed = pmatch (phead # (pdropList # (pasInt # (phead # witnessFields)) # wdrls)) (\(PBuiltinPair wCredD _) -> wCredD)
         in pvalidateConditions'
                [ptraceInfoIfFalse "programmable global/seize not invoked at the witnessed index" (witnessed #== claimed)]

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
phasCSH :: Term s (PCurrencySymbol :--> PAsData PLedgerValue :--> PBool)
phasCSH = phoistAcyclic $ plam $ \directoryNodeCS value ->
    let value' = pledgerValueCsPairs (pfromData value)
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
phasCSHOrFalse :: Term s PCurrencySymbol -> Term s (PAsData PLedgerValue) -> Term s PBool
phasCSHOrFalse directoryNodeCS value =
    let nonAdaEntries = ptail # pledgerValueCsPairs (pfromData value)
     in pelimList
            (\currencyPair _ -> pmatch currencyPair $ \(PBuiltinPair csD _) -> pfromData csD #== directoryNodeCS)
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
        go = pfix $ \self -> plam $ \remainingRefInputs ->
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
    Term s (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (AssocMap.PSortedMap PTokenName PInteger)))) ->
    Term s PSortedValue
pcheckTransferLogicAndGetProgrammableValue directoryNodeCS refInputs proofList wdrlIdxList withdrawalEntries initialCachedTransferScript mapInnerList =
    let -- Cache transfer-script invocation checks across adjacent positive proofs;
        -- on a cache miss, verify the redeemer-witnessed withdrawal index instead
        -- of scanning the withdrawal map (scan-proof: O(1) per policy regardless
        -- of how many withdrawals the transaction carries).
        -- Matches are consed onto the RESULT of the recursive call rather than
        -- onto a forward accumulator, so the list comes back in canonical
        -- ascending order without a reversing pass.
        go = pfix $ \self -> plam $ \proofs wdrlIdxs inputInnerValue cachedTransferScript ->
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
                        PBuiltinPair currCSD _ <- pmatch csPair
                        let currCS = pfromData currCSD
                            nodeKey = pfromData directoryNodeDatumFkey
                            nodeNext = pfromData directoryNodeDatumFNext
                        pif
                            (nodeKey #< currCS)
                            ( let checks =
                                    pcondsAll
                                        [ ptraceInfoIfFalse "dir neg-proof node must cover" (currCS #< nodeNext)
                                        , ptraceInfoIfFalse "invalid dir node n" (phasCSH # directoryNodeCS # directoryNodeUTxOFValue)
                                        ]
                               in pif
                                    checks
                                    ( self
                                        # (ptail # proofs)
                                        # (ptail # wdrlIdxs)
                                        # csPairs
                                        # cachedTransferScript
                                    )
                                    perror
                            )
                            ( let checks =
                                    pcondsAll
                                        [ ptraceInfoIfFalse "Missing required transfer script" $
                                            pif
                                                (directoryNodeDatumFTransferLogicScript #== cachedTransferScript)
                                                (pconstant True)
                                                ( directoryNodeDatumFTransferLogicScript
                                                    #== pmatch (phead # (pdropList # pfromData (phead # wdrlIdxs) # withdrawalEntries)) (\(PBuiltinPair wdrlCredD _) -> wdrlCredD)
                                                )
                                        , ptraceInfoIfFalse "directory proof mismatch" (nodeKey #== currCS)
                                        , ptraceInfoIfFalse "invalid dir node" (phasCSH # directoryNodeCS # directoryNodeUTxOFValue)
                                        ]
                               in pif
                                    checks
                                    ( pcons
                                        # csPair
                                        #$ self
                                        # (ptail # proofs)
                                        # (ptail # wdrlIdxs)
                                        # csPairs
                                        # directoryNodeDatumFTransferLogicScript
                                    )
                                    perror
                            )
                )
                pnil
                inputInnerValue
     in pmkSortedValue $
            go
                # proofList
                # wdrlIdxList
                # mapInnerList
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
    Term s PSortedValue ->
    Term s PSortedValue
pcheckMintLogicAndGetProgrammableValue directoryNodeCS refInputs proofList totalMintValue =
    let mintedEntries :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (AssocMap.PSortedMap PTokenName PInteger))))
        mintedEntries = pvalueCsPairs totalMintValue
        -- Same shape as the transfer walk: cons onto the recursive result so the
        -- entries come back ascending without a reversing pass.
        go = pfix $ \self -> plam $ \proofs remainingMintEntries ->
            pelimList
                ( \mintCsPair mintCsPairs ->
                    pelimList
                        ( \mintProofData proofsRest ->
                            pmatch mintCsPair $ \(PBuiltinPair mintCsD _) ->
                             let currCS = pfromData mintCsD
                              in pmatch (pfromData mintProofData) $ \case
                                    -- Member: count the entry, touch no node.
                                    PMember ->
                                        pcons # mintCsPair #$ self # proofsRest # mintCsPairs
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
                                                pcondsAll
                                                    [ ptraceInfoIfFalse "dir mint neg-proof node must cover" (nodeKey #< currCS)
                                                    , ptraceInfoIfFalse "dir mint neg-proof node must cover" (currCS #< nodeNext)
                                                    , ptraceInfoIfFalse "invalid dir node n" (phasCSH # directoryNodeCS # directoryNodeUTxOFValue)
                                                    ]
                                        pif
                                            checks
                                            (self # proofsRest # mintCsPairs)
                                            perror
                        )
                        (ptraceInfoError "mint proof missing")
                        proofs
                )
                (pelimList (\_ _ -> ptraceInfoError "extra mint proof") pnil proofs)
                remainingMintEntries
     in pmkSortedValue $ go # proofList # mintedEntries

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
        , plgrOwnerWdrlIdxs :: [Integer]
        -- ^ Withdrawal index of the OWNER script of each script-owned
        -- mini-ledger input, in input order. Pubkey-owned inputs contribute no
        -- entry -- they are witnessed by a signature instead. Scan-proofness:
        -- without this the validator searched the withdrawal map for each such
        -- owner, so an issuer's cost depended on where their script hash sorted
        -- against the other participants' -- something they cannot control.
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
        -- pownerWdrlIdxs are the withdrawal indices of the OWNER script of each
        -- script-owned mini-ledger input, in input order (verified, never scanned).
        -- pmintProofs are per-minted-symbol Member|NonMember classifications.
        -- pparamsRefIdx indexes the protocol-params reference input.
        { ptransferProofs :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
        , ptransferWdrlIdxs :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
        , pownerWdrlIdxs :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
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
    withdrawalEntries <- plet $ punsortedMapPairs (pfromData ptxInfo'wdrl)

    pmatch red $ \case
        -- `TransferAct` invariants:
        -- - The expected programmable output value must equal the validated
        --   programmable inputs plus validated programmable mint or burn.
        -- - No programmable value may escape from outputs at `progLogicCred`.
        -- - Transfer and mint proofs must be consumed in lockstep with the
        --   programmable policies they witness.
        PTransferAct transferProofs transferWdrlIdxs ownerWdrlIdxs mintProofs paramsRefIdx -> P.do
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
            cachedTransferScript0 <- plet $ pmatch (phead @PBuiltinList # withdrawalEntries) (\(PBuiltinPair credD _) -> credD)
            totalProgTokenValue <-
                plet $
                    pvalueFromCred
                        progLogicCred
                        (pfromData ptxInfo'signatories)
                        withdrawalEntries
                        (pfromData ownerWdrlIdxs)
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
            mintValueNoGuarantees <- plet $ punsafeCoerce @PSortedValue (pfromData ptxInfo'mint)
            expectedProgrammableOutputValue <-
                plet $
                    pif
                        (pnull # pvalueCsPairs mintValueNoGuarantees)
                        totalProgTokenValue_
                        -- Merge the validated programmable mint/burn delta into the
                        -- transfer value with the CIP-153 builtin union rather than
                        -- a hand-rolled sorted walk plus a positivity filter. The
                        -- builtin sums asset-wise and, because its representation is
                        -- canonical, drops anything that cancels to zero — which is
                        -- exactly what the filter existed to do for fully burned
                        -- assets, so the 'Positive coercion below stays honest.
                        --
                        -- A NEGATIVE entry would survive the union, and the
                        -- containment check errors on non-positive operands rather
                        -- than ignoring them. That is the safe direction and it is
                        -- unreachable: burning a programmable asset requires spending
                        -- it, programmable assets live only at the base credential,
                        -- so every burned unit is already counted in the transfer
                        -- value and the sum cannot go below zero.
                        ( pmkSortedValue $
                            punsafeCoerce
                                ( pasMap
                                    #$ pvalueData
                                    #$ punionValue
                                    # (punValueData # (pmapData # punsafeCoerce (pvalueCsPairs totalProgTokenValue_)))
                                    # ( punValueData
                                            #$ pmapData
                                            #$ punsafeCoerce
                                            $ pvalueCsPairs
                                                ( pcheckMintLogicAndGetProgrammableValue
                                                    (pfromData pdirectoryNodeCS)
                                                    referenceInputs
                                                    (pfromData mintProofs)
                                                    mintValueNoGuarantees
                                                )
                                      )
                                )
                        )

            pvalidateConditions'
                [ ptraceInfoIfFalse "prog tokens escape" $
                    poutputsContainExpectedValueAtCred
                        progLogicCred
                        (pfromData ptxInfo'outputs)
                        expectedProgrammableOutputValue
                , pisRewardingScript (pdata pscriptContext'scriptInfo)
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
    withdrawalEntries <- plet $ punsortedMapPairs (pfromData ptxInfo'wdrl)
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
            mintValueNoGuarantees <- plet $ punsafeCoerce @PSortedValue (pfromData ptxInfo'mint)
            seizeMintedTokens <- plet $ ptokensForCurrencySymbol # pfromData directoryNodeDatumFKey # mintValueNoGuarantees
            let conditions =
                    [ pisRewardingScript (pdata pscriptContext'scriptInfo)
                    , ptraceInfoIfFalse "mini-ledger invariants violated" $ processThirdPartyTransfer directoryNodeDatumFKey progLogicCred (pfromData ptxInfo'inputs) remainingOutputs seizeMintedTokens
                    , -- Scan-proof: the redeemer witnesses the issuer withdrawal's
                      -- index; a wrong index resolves to a different credential and
                      -- fails the equality.
                      ptraceInfoIfFalse "issuer logic script must be invoked" $
                        directoryNodeDatumFIssuerLogicScript
                            #== pmatch (phead # (pdropList # pfromData pissuerWdrlIdx # withdrawalEntries)) (\(PBuiltinPair credD _) -> credD)
                    , ptraceInfoIfFalse "directory node is not valid" $ phasCSH # pfromData pdirectoryNodeCS # seizeDirectoryNodeValue
                    ]
            pvalidateConditions' conditions

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
    forall s.
    Term s (PCurrencySymbol :--> PSortedValue :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
ptokensForCurrencySymbol =
    phoistAcyclic $
        plam $ \targetCs mintValue ->
            ptokensForCurrencyPairs # targetCs # pvalueCsPairs mintValue

-- | 'ptokensForCurrencySymbol' over a raw currency-pair list, for callers that
-- already hold the value as Data and would otherwise pay a typed decode.
ptokensForCurrencyPairs ::
    forall s.
    Term s (PCurrencySymbol :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (AssocMap.PSortedMap PTokenName PInteger))) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
ptokensForCurrencyPairs =
    phoistAcyclic $
        plam $ \targetCs mintedEntries ->
            let go = pfix $ \self -> plam $ \remainingMintEntries ->
                    pelimList
                        ( \mintCsPair mintCsPairs ->
                            pmatch mintCsPair $ \(PBuiltinPair mintCsD tokenMapD) ->
                                let mintCs = pfromData mintCsD
                                 in pif
                                        (mintCs #== targetCs)
                                        (ptokenPairs (pfromData tokenMapD))
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
    pfix $ \self -> plam $ \actualTokens requiredTokens ->
        pelimList
            ( \requiredPair requiredRest ->
                pmatch requiredPair $ \(PBuiltinPair requiredTokenNameD requiredQtyD) ->
                 let requiredTokenName = pfromData requiredTokenNameD
                     requiredQty = pfromData requiredQtyD
                  in pelimList
                        ( \actualPair actualRest ->
                            pmatch actualPair $ \(PBuiltinPair actualTokenNameD actualQtyD) ->
                             let actualTokenName = pfromData actualTokenNameD
                                 actualQty = pfromData actualQtyD
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
    Term s (PAsData PCurrencySymbol) ->
    Term s PData ->
    Term _ (PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PAsData PTxOut) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBool) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))) ->
    Term s PData ->
    Term s PBool
pcheckCorrespondingThirdPartyTransferInputsAndOutputs programmableCS progLogicCredData self remainingInputs programmableOutputs deltaAccumulator programmableInputResolvedData =
    -- Classify the input by payment credential using ONLY the input address; the
    -- (more expensive) output pairing and value extraction is deferred into the
    -- base-credential branch. This keeps the per-input skip cost minimal — critical
    -- now that every transaction input is walked (e.g. many fee/pubkey inputs).
    pmatch (pasConstr # programmableInputResolvedData) $ \(PBuiltinPair _ inputTxOutFields) ->
        plet (phead # inputTxOutFields) $ \inputTxOutAddress ->
            let inputCredentialData = pmatch (pasConstr # inputTxOutAddress) (\(PBuiltinPair _ addrFields) -> phead # addrFields)
             in pif
                    (inputCredentialData #== progLogicCredData)
                    -- Programmable (base-credential) input: pair it with the next
                    -- remaining output and accumulate the seized-policy delta.
                    ( pmatch (pasConstr # pforgetData (phead # programmableOutputs)) $ \(PBuiltinPair _ outputTxOutFields) ->
                        plet (ptail # inputTxOutFields) $ \inputTxOutFieldsRest ->
                            plet (ptail # outputTxOutFields) $ \outputTxOutFieldsRest ->
                                let outputTxOutAddress = phead # outputTxOutFields
                                    programmableInputValue = phead # inputTxOutFieldsRest
                                    programmableOutputValue = phead # outputTxOutFieldsRest
                                    programmableInputRest = ptail # inputTxOutFieldsRest
                                    programmableOutputRest = ptail # outputTxOutFieldsRest
                                 in pif
                                        -- Address, datum and reference script must all be preserved.
                                        -- Re-consing the address in front of the (datum, refScript)
                                        -- suffix and comparing the two `listData`s costs one
                                        -- `equalsData` for all three fields instead of one for the
                                        -- address plus another for the suffix: `PEq (PBuiltinList
                                        -- PData)` is itself `listData` + `equalsData`, so the second
                                        -- comparison was pure overhead. `equalsData` on lists is
                                        -- length-checking and element-wise, so this is exactly the
                                        -- conjunction it replaces.
                                        ( ptraceInfoIfFalse "corresponding output: address/datum/reference script mismatch" $
                                            pdata (pcons # inputTxOutAddress # programmableInputRest)
                                                #== pdata (pcons # outputTxOutAddress # programmableOutputRest)
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
    plet (pforgetData (pdata progLogicCred)) $ \progLogicCredData ->
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
        go2 = pfix $ \self -> plam $ \programmableOutputs ->
            pelimList
                ( \programmableOutput programmableOutputsRest ->
                    pmatch (pasConstr # pforgetData programmableOutput) $ \(PBuiltinPair _ outFields) ->
                        pheadTailBuiltin outFields $ \outAddrData outFieldsRest ->
                            let paymentCredData = pmatch (pasConstr # outAddrData) (\(PBuiltinPair _ addrFields) -> phead # addrFields)
                                outValueData = phead # outFieldsRest
                             in pif
                                    (paymentCredData #== progLogicCredData)
                                    (ptokenPairsUnionFast # (ptokensForCurrencyPairs # programmableCS' # punsafeCoerce (pasMap # outValueData)) # (self # programmableOutputsRest))
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
        go = pfix $ \self -> plam $ \remainingInputs programmableOutputs deltaAccumulator ->
            pelimList
                ( \txIn remainingInputsRest ->
                    -- The base credential arrives as a decoded 'PCredential' but is
                    -- only ever compared as Data. Encoding it inside the checker
                    -- repeated that 'pdata' on EVERY transaction input, including
                    -- the fee/pubkey inputs this walk merely skips; hoisting it
                    -- above the walk pays for it once.
                    pcheckCorrespondingThirdPartyTransferInputsAndOutputs
                        programmableCS
                        progLogicCredData
                        self
                        remainingInputsRest
                        programmableOutputs
                        deltaAccumulator
                        (pforgetData (pdata (ptxInInfoResolved $ pfromData txIn)))
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

{- | Does the (already-decoded, CS-sorted) currency list contain the target policy?
Early-exits once the sorted list passes the target.

NB: the fixpoint is deliberately built *inside* the `targetCS` lambda rather than
threaded through the recursion. Now that the difference itself witnesses
non-contamination in the common case (see `pvalueEqualsDeltaCurrencySymbol`), this
scan is cold: keeping `pfixHoisted` under the lambda defers its construction to the calls
that actually happen instead of paying it once at script start-up. Threading
`targetCS` through a top-level fixpoint measured +0.27% CPU / +0.65% memory on
`SeizeAct1` and +2 script bytes for exactly that reason.
-}
pcurrencyListHasCS ::
    forall s.
    Term s (PCurrencySymbol :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (AssocMap.PSortedMap PTokenName PInteger))) :--> PBool)
pcurrencyListHasCS = phoistAcyclic $ plam $ \targetCS ->
    pfixHoisted #$ plam $ \self entries ->
        pelimList
            ( \entry rest ->
                pmatch entry $ \(PBuiltinPair csD _) ->
                    plet (pfromData csD) $ \cs ->
                        pif (cs #== targetCS) (pconstant True) (pif (targetCS #< cs) (pconstant False) (self # rest))
            )
            (pconstant False)
            entries

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
- The paired input must actually hold `progCS` (non-contamination, Aiken
  Finding 12): a seize may not drag along unrelated programmable UTxOs.

Implementation:
The signed difference @input - output@ is computed with the PV11 / CIP-153
builtin `Value` operations rather than a hand-rolled sorted lockstep walk. The
builtin representation is canonical — entries are strictly ascending, inner maps
are non-empty and no quantity is zero — so every policy on which the two values
agree cancels to zero and is *dropped*. "Only `progCS` may differ" therefore
becomes the structural statement "the difference has at most one entry, and that
entry's key is `progCS`", and the delta itself falls out of the same object. The
previous walk paid a full `equalsData` per shared policy to prove it unchanged.

Cost/overflow notes:
- `punValueData` rejects non-canonical `Data`. Ledger-supplied `TxOut` values are
  canonical by construction (strictly ascending symbols/names, no zero or empty
  entries), and this module already decodes `TxOut` values with the same builtin
  in the transfer path, so this is a strengthening rather than a new failure mode.
- Builtin quantities are signed 128-bit. Ledger quantities are int64-bounded per
  entry, so @in - out@ is bounded by 2^64 in magnitude: neither `pscaleValue`
  (negation) nor `punionValue` (addition) can overflow on ledger-supplied values.
- `pvalueContains` is deliberately NOT used anywhere near this delta: it *errors*
  on negative operands, and the seize delta is signed by construction.
-}
pvalueEqualsDeltaCurrencySymbol ::
    forall s.
    Term s (PAsData PCurrencySymbol) ->
    Term s PData ->
    Term s PData ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
pvalueEqualsDeltaCurrencySymbol progCSAsData inputUTxOValue outputUTxOValue =
    let progCSData = pforgetData progCSAsData
        -- Policy ids are B-shaped Data: comparing payload bytes replaces
        -- equalsData's ~950k-CPU intercept with equalsByteString (~31k). The
        -- target bytes are bound once for the whole seize walk, so only the
        -- per-entry pasByteStr is paid inside the loop.

        -- input - output, canonicalised by the builtin: shared policies cancel and
        -- are dropped, so only genuinely differing policies survive. Ada is left
        -- in the operands deliberately: when it is unchanged -- the norm -- it
        -- cancels here and costs nothing downstream.
        diffEntries :: Term _ (PBuiltinList (PBuiltinPair PData PData))
        diffEntries =
            pasMap
                #$ pvalueData
                #$ punionValue
                # (punValueData # inputUTxOValue)
                # (pscaleValue # pconstantInteger (-1) # (punValueData # outputUTxOValue))

        -- Non-contamination fallback: scan the input's own currency list. Only
        -- reached when the difference cannot already witness the holding (see
        -- below), so the extra `unMapData` + walk is off the common path.
        inputHoldsProgCS =
            pcurrencyListHasCS
                # pfromData progCSAsData
                # punsafeCoerce
                    @(PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (AssocMap.PSortedMap PTokenName PInteger))))
                    (pasMap # inputUTxOValue)

        notHeld :: forall a. Term s a
        notHeld = ptraceInfoError "seize: paired input does not hold the seized policy"

        movedOtherPolicy :: forall a. Term s a
        movedOtherPolicy = ptraceInfoError "corresponding output: value changed outside the seized policy"

        -- The one non-seized policy a pair may legitimately differ on is ada, and
        -- only upward. A protocol-parameter change can raise the min-UTxO
        -- requirement above what a UTxO already holds; demanding the continuing
        -- output carry exactly the input's lovelace would make every such UTxO
        -- permanently unseizable, since the ledger would require more ada than
        -- this validator allowed. Ada's policy id is the empty bytestring, so it
        -- sorts first and can only ever be the leading diff entry; the delta is
        -- `input - output`, so "topped up" is a non-positive quantity.
        adaToppedUp entryCsD entryMapD =
            pif
                (pasByteStr # entryCsD #== pconstant "")
                ( pmatch
                    (phead # (pasMap # entryMapD))
                    (\(PBuiltinPair _ adaQtyD) -> pasInt # adaQtyD #<= pconstantInteger 0)
                )
                (pconstant False)

        -- The seized policy's delta, given its diff entry and everything after it.
        -- Bound once so the two call sites below share one copy in the UPLC.
        progCSDelta = plam $ \entryMapD rest ->
            pelimList
                -- A further differing policy is value moved outside the seize.
                (\_ _ -> movedOtherPolicy)
                ( plet
                    ( punsafeCoerce
                        @(PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
                        (pasMap # entryMapD)
                    )
                    $ \delta ->
                        -- A positive quantity anywhere in the delta is itself the
                        -- non-contamination proof: `in = out + delta` and TxOut
                        -- quantities are non-negative, so `in > 0` for that token
                        -- and the input demonstrably holds progCS. Testing only
                        -- the FIRST token keeps this O(1); a seize removes tokens,
                        -- so the honest path always takes it. A leading
                        -- non-positive quantity is sound but inconclusive, and
                        -- falls back to the explicit scan.
                        pif
                            (pmatch (phead # delta) $ \(PBuiltinPair _ qtyD) -> pconstantInteger 0 #< pfromData qtyD)
                            delta
                            (pif inputHoldsProgCS delta notHeld)
                )
                rest

        -- No difference at all: the pair is a pure pass-through, which is legal
        -- only if the input really holds the seized policy.
        purePassThrough = pif inputHoldsProgCS pnil notHeld
     in plet (pasByteStr # progCSData) $ \progCSBytes ->
        plet progCSDelta $ \onProgCS ->
            pelimList
                ( \entry rest ->
                    pmatch entry $ \(PBuiltinPair entryCsD entryMapD) ->
                        pif
                            ((pasByteStr # entryCsD) #== progCSBytes)
                            (onProgCS # entryMapD # rest)
                            -- Not the seized policy: tolerated only as an ada top-up,
                            -- after which the seized policy may still follow.
                            ( pif
                                (adaToppedUp entryCsD entryMapD)
                                ( pelimList
                                    ( \nextEntry nextRest ->
                                        pmatch nextEntry $ \(PBuiltinPair nextCsD nextMapD) ->
                                            pif
                                                ((pasByteStr # nextCsD) #== progCSBytes)
                                                (onProgCS # nextMapD # nextRest)
                                                movedOtherPolicy
                                    )
                                    purePassThrough
                                    rest
                                )
                                movedOtherPolicy
                            )
                )
                purePassThrough
                diffEntries
