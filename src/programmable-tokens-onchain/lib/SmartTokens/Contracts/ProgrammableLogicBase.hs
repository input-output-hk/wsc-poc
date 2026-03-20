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
asset quantity. Otherwise it aggregates the full non-Ada value at `progLogicCred`
outputs and checks each expected asset against that aggregate.

Security invariants:

- The result must be a lower-bound containment check only for outputs at
  `progLogicCred`.
- No value at other payment credentials may satisfy the requirement.
- This helper assumes the caller has already validated that `expectedValue`
  represents the value that must remain inside the mini-ledger.
- The single-asset fast path and the multi-asset path must be semantically
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

{- | Base spending validator for programmable-token UTxOs.

High-level purpose:
- Force every spend of the shared programmable-token payment script to be
  accompanied by the global stake validator through the withdraw-zero pattern.

Security invariants:
- Spending must fail unless the designated global stake credential appears in the
  transaction withdrawals.
- This validator must not independently authorize transfers or minting; it only
  enforces delegation to the global validator.
- The check must be credential-exact, so unrelated withdrawals cannot satisfy it.
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
phasCSH :: Term s PCurrencySymbol -> Term s (PAsData (PValue 'Sorted 'Positive)) -> Term s PBool
phasCSH directoryNodeCS value =
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
    Term s (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace))) ->
    Term s (PAsData PCredential) ->
    Term s (PValue 'Sorted 'Positive) ->
    Term s (PValue 'Sorted 'Positive)
pcheckTransferLogicAndGetProgrammableValue directoryNodeCS refInputs proofList withdrawalEntries initialCachedTransferScript totalValue =
    let mapInnerList :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger))))
        mapInnerList = pto (pto totalValue)
        -- Cache transfer-script invocation checks across adjacent positive proofs.
        go = pfix #$ plam $ \self proofs inputInnerValue actualProgrammableTokenValue cachedTransferScript ->
            pelimList
                ( \csPair csPairs ->
                    P.do
                        PTxOut{ptxOut'value = directoryNodeUTxOFValue, ptxOut'datum = directoryNodeUTxOFDatum} <-
                            pmatch $ ptxInInfoResolved (pfromData $ phead # (pdropFast # pfromData (phead # proofs) # refInputs))
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
                                        , ptraceInfoIfFalse "invalid dir node n" (phasCSH directoryNodeCS directoryNodeUTxOFValue)
                                        ]
                               in pif
                                    checks
                                    ( self
                                        # (ptail # proofs)
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
                                                #|| (pisScriptInvokedEntries # directoryNodeDatumFTransferLogicScript # withdrawalEntries)
                                        , ptraceInfoIfFalse "directory proof mismatch" (nodeKey #== currCS)
                                        , ptraceInfoIfFalse "invalid dir node" (phasCSH directoryNodeCS directoryNodeUTxOFValue)
                                        ]
                               in pif
                                    checks
                                    ( self
                                        # (ptail # proofs)
                                        # csPairs
                                        # (pcons # csPair # actualProgrammableTokenValue)
                                        # directoryNodeDatumFTransferLogicScript
                                    )
                                    perror
                            )
                )
                (pcon $ PValue $ pcon $ PMap actualProgrammableTokenValue)
                inputInnerValue
     in go
            # proofList
            # mapInnerList
            # pto (pto pemptyLedgerValue)
            # initialCachedTransferScript

{- | Filter the tx mint field down to programmable policies by validating one
directory-node witness per minted policy.

High-level purpose:
- Prove which minted or burned policies are programmable and return only those
  signed entries for later output containment checks.

Security invariants:
- `proofList` must be aligned with the currency-symbol order of `totalMintValue`.
- A positive proof must reference the exact directory node and the referenced
  transfer logic script must be invoked.
- A negative proof must reference a covering node whose `(key, next)` interval
  excludes the minted currency symbol.
- Every referenced directory node must be legitimate, proven by the directory
  state token.
- Missing or extra proofs must fail validation.
-}
pcheckMintLogicAndGetProgrammableValue ::
    Term s PCurrencySymbol ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PInteger)) ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace))) ->
    Term s (PValue 'Sorted 'NoGuarantees) ->
    Term s (PValue 'Sorted 'NoGuarantees)
pcheckMintLogicAndGetProgrammableValue directoryNodeCS refInputs proofList withdrawalEntries totalMintValue =
    let mintedEntries :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger))))
        mintedEntries = pto (pto totalMintValue)
        go = pfix #$ plam $ \self proofs remainingMintEntries programmableMintValue ->
            pelimList
                ( \mintCsPair mintCsPairs ->
                    pelimList
                        ( \nodeIdx proofsRest ->
                            let mintCs = pfstBuiltin # mintCsPair
                             in P.do
                                    PTxOut{ptxOut'value = directoryNodeUTxOFValue, ptxOut'datum = directoryNodeUTxOFDatum} <-
                                        pmatch $ ptxInInfoResolved (pfromData $ phead # (pdropFast # pfromData nodeIdx # refInputs))
                                    POutputDatum paramDat' <- pmatch directoryNodeUTxOFDatum
                                    PDirectorySetNode
                                        { pkey = directoryNodeDatumFkey
                                        , pnext = directoryNodeDatumFNext
                                        , ptransferLogicScript = directoryNodeDatumFTransferLogicScript
                                        } <-
                                        pmatch (pfromData $ punsafeCoerce @(PAsData PDirectorySetNode) (pto paramDat'))
                                    let currCS = pfromData mintCs
                                        nodeKey = pfromData directoryNodeDatumFkey
                                        nodeNext = pfromData directoryNodeDatumFNext
                                    pif
                                        (nodeKey #== currCS)
                                        ( let checks =
                                                pand'List
                                                    [ ptraceInfoIfFalse "Missing required transfer script" (pisScriptInvokedEntries # directoryNodeDatumFTransferLogicScript # withdrawalEntries)
                                                    , ptraceInfoIfFalse "invalid dir node m" (phasCSH directoryNodeCS directoryNodeUTxOFValue)
                                                    ]
                                           in pif
                                                checks
                                                (self # proofsRest # mintCsPairs # (pcons # mintCsPair # programmableMintValue))
                                                perror
                                        )
                                        ( let checks =
                                                pand'List
                                                    [ ptraceInfoIfFalse "dir mint neg-proof node must cover" (nodeKey #< currCS)
                                                    , ptraceInfoIfFalse "dir mint neg-proof node must cover" (currCS #< nodeNext)
                                                    , ptraceInfoIfFalse "invalid dir node n" (phasCSH directoryNodeCS directoryNodeUTxOFValue)
                                                    ]
                                           in pif
                                                checks
                                                (self # proofsRest # mintCsPairs # programmableMintValue)
                                                perror
                                        )
                        )
                        (ptraceInfoError "mint proof missing")
                        proofs
                )
                (pelimList (\_ _ -> ptraceInfoError "extra mint proof") (pcon $ PValue $ pcon $ PMap programmableMintValue) proofs)
                remainingMintEntries
     in go # proofList # mintedEntries # pnil

data ProgrammableLogicGlobalRedeemer
    = TransferAct
        { plgrTransferProofs :: [Integer]
        , plgrMintProofs :: [Integer]
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
mkSeizeActRedeemerFromAbsoluteInputIdxs :: Integer -> [Integer] -> Integer -> ProgrammableLogicGlobalRedeemer
mkSeizeActRedeemerFromAbsoluteInputIdxs directoryNodeIdx absoluteInputIdxs =
    mkSeizeActRedeemerFromRelativeInputIdxs
        directoryNodeIdx
        (absoluteToRelativeInputIdxs absoluteInputIdxs)

data PProgrammableLogicGlobalRedeemer (s :: S)
    = PTransferAct
        -- The witness lists contain reference-input indices for directory nodes.
        -- Exact-match vs covering-node proofs are derived onchain from the referenced datum.
        { ptransferProofs :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
        , pmintProofs :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
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
        -- `TransferAct` invariants:
        -- - The expected programmable output value must equal the validated
        --   programmable inputs plus validated programmable mint or burn.
        -- - No programmable value may escape from outputs at `progLogicCred`.
        -- - Transfer and mint proofs must be consumed in lockstep with the
        --   programmable policies they witness.
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
        -- `SeizeAct` invariants:
        -- - Only the seized policy may change across paired programmable
        --   inputs and outputs.
        -- - The resulting seized-policy delta plus mint or burn for that
        --   policy must remain inside the programmable outputs.
        -- - Input witnesses must cover all script spends and match the
        --   redeemer's declared witness count.
        PSeizeAct{pdirectoryNodeIdx, pinputIdxs, poutputsStartIdx, plengthInputIdxs} -> P.do
            inputIdxsLen <- plet $ pfromData plengthInputIdxs
            let inputIdxsData = punsafeCoerce (pfromData pinputIdxs) :: Term _ (PBuiltinList PData)
            let remainingOutputs = pdropFast # pfromData poutputsStartIdx # pfromData ptxInfo'outputs
            let directoryNodeUTxO = phead # (pdropFast # pfromData pdirectoryNodeIdx # referenceInputs)
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
                                                   in self # remainingRelativeIdxs # remainingInputsAfterIdx # (ptail # programmableOutputs) # (ptokenPairsUnionFast # delta # deltaAccumulator)
                                                )
                                                perror
                                            )
                                            ( pif
                                                ((pfstBuiltin # (pasConstr # inputCredentialData)) #== 1)
                                                (self # remainingRelativeIdxs # remainingInputsAfterIdx # programmableOutputs # deltaAccumulator)
                                                (ptraceInfoError "input index points to pubkey input")
                                            )

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
    Term s (PBuiltinList PData) ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))) ->
    Term s PBool
processThirdPartyTransfer programmableCS progLogicCred inputs progOutputs inputIdxs' mintedTokens =
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
                (checkBalanceInvariant programmableOutputs (ptokenPairsUnionFast # deltaAccumulator # mintedTokens))
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

{- | Negate every signed quantity in a sorted token-name map.

High-level purpose:
- Reuse the same token-pair representation for both positive and negative deltas.

Security invariants:
- Token names and ordering must be preserved exactly.
- Each quantity must be negated exactly once.
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
