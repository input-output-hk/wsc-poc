{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import BenchmarkOnchain.ScriptHelpers (bs28, mkValue, pubKeyAddress)
import BenchmarkOnchain.SimpleRunner (BenchCase, mkTermCase, runSimpleBenchmark)
import Data.ByteString qualified as BS
import Plutarch.Core.Context (
    pscriptContextTxInfo,
    ptxInInfoResolved,
 )
import Plutarch.Core.Internal.Builtins (pmapData, ppairDataBuiltinRaw)
import Plutarch.Core.Utils
import Plutarch.LedgerApi.V3
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Value (Value, assetClass, assetClassValue)
import PlutusLedgerApi.V3 (
    Address (Address),
    Credential (PubKeyCredential, ScriptCredential),
    CurrencySymbol (CurrencySymbol),
    Data,
    PubKeyHash (PubKeyHash),
    ScriptContext,
    ScriptHash (ScriptHash),
    StakingCredential (StakingHash),
    TokenName (TokenName),
 )
import PlutusTx qualified
import ProgrammableTokens.Test.ScriptContext.Builder
import SmartTokens.Contracts.ProgrammableLogicBase qualified as Actual

main :: IO ()
main =
    runSimpleBenchmark
        "Onchain function benchmark (NoTracing, isolated utility terms)"
        benchCases

mkCase :: String -> (forall s. Term s a) -> [Data] -> BenchCase
mkCase = mkTermCase

-- Small synthetic fixture builders used to isolate specific helper terms.
credentialAtSortedIndex :: Int -> Credential
credentialAtSortedIndex idx =
    ScriptCredential (ScriptHash (bs28 (fromIntegral (idx + 1))))

withdrawalCtxWithMatchAt :: Int -> Int -> ScriptContext
withdrawalCtxWithMatchAt totalCount _matchIdx =
    buildScriptContext $
        foldMap (`withWithdrawal` 0) [credentialAtSortedIndex idx | idx <- [0 .. totalCount - 1]]

hasCredEqualsData :: Term s (PAsData PCredential :--> PScriptContext :--> PBool)
hasCredEqualsData = phoistAcyclic $ plam $ \stakeCred ctx ->
    pmatch (pscriptContextTxInfo ctx) $ \txInfo ->
        let withdrawals = pto $ pfromData $ ptxInfo'wdrl txInfo
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
         in hasCred

-- Head-to-head isolation of the two credential-comparison strategies used by
-- `mkProgrammableLogicBase`. In the real validator the expected credential is an
-- applied script parameter (so it is a baked-in constant) while the credential under
-- test comes from the script context as `Data`; these fixtures mirror that split so
-- the measured delta is purely the comparison itself.

-- | Candidate: unwrap the credential once and compare the 28-byte script hash.
pisBaseCred :: Term s (PByteString :--> PAsData PCredential :--> PBool)
pisBaseCred = plam $ \baseCredHash cred ->
    pasByteStr # (phead # (psndBuiltin # (pasConstr # pforgetData cred))) #== baseCredHash

-- | Current: compare the whole credential as `Data` (compiles to the `equalsData` builtin).
pisBaseCredCurr :: Term s (PAsData PCredential :--> PAsData PCredential :--> PBool)
pisBaseCredCurr = plam $ \baseCred cred -> baseCred #== cred

baseCredHashRaw :: BS.ByteString
baseCredHashRaw = BS.replicate 28 1

-- | The credential the base validator is parameterised with.
baseCredential :: Credential
baseCredential = ScriptCredential (ScriptHash (bs28 1))

-- | A different script credential (the dominant case: scanning past non-matches).
otherScriptCredential :: Credential
otherScriptCredential = ScriptCredential (ScriptHash (bs28 2))

-- | A pubkey credential carrying the *same* 28 bytes as the base script hash.
-- Distinguishes the two strategies semantically, not just on cost.
pubKeyCredentialSameHash :: Credential
pubKeyCredentialSameHash = PubKeyCredential (PubKeyHash (bs28 1))

isBaseCredByteStringTerm :: Term s (PAsData PCredential :--> PBool)
isBaseCredByteStringTerm = pisBaseCred # pconstant baseCredHashRaw

isBaseCredEqualsDataTerm :: Term s (PAsData PCredential :--> PBool)
isBaseCredEqualsDataTerm = pisBaseCredCurr # pdata (pconstant baseCredential)

pemptyLedgerValue :: Term s (PValue 'Sorted 'Positive)
pemptyLedgerValue = punsafeCoerce $ pconstant @(PValue 'Unsorted 'NoGuarantees) (mempty :: Value)

pjustData :: Term s (PMaybeData a) -> Term s a
pjustData term =
    punsafeCoerce $ phead # (psndBuiltin # (pasConstr # pforgetData (pdata term)))

pstripAdaHBench ::
    forall (v :: AmountGuarantees) (s :: S).
    Term s (PValue 'Sorted v) -> Term s (PValue 'Sorted v)
pstripAdaHBench value =
    let nonAdaValueMapInner = ptail # pto (pto value)
     in pcon (PValue $ pcon $ PMap nonAdaValueMapInner)

ptokenPairsUnionFastBench ::
    Term
        s
        ( PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
            :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
            :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
        )
ptokenPairsUnionFastBench = phoistAcyclic $
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

pcurrencyPairsUnionFastBench ::
    Term
        s
        ( PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))
            :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))
            :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))
        )
pcurrencyPairsUnionFastBench = phoistAcyclic $
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
                                      mergedTokenPairs = ptokenPairsUnionFastBench # tokenPairsA # tokenPairsB
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

pvalueUnionFastBench ::
    Term s (PValue 'Sorted 'Positive :--> PValue 'Sorted 'Positive :--> PValue 'Sorted 'Positive)
pvalueUnionFastBench = phoistAcyclic $ plam $ \valueA valueB ->
    pcon $
        PValue $
            pcon $
                PMap $
                    pcurrencyPairsUnionFastBench
                        # pto (pto valueA)
                        # pto (pto valueB)

passetQtyInValueBench ::
    Term s (PValue 'Sorted 'Positive :--> PCurrencySymbol :--> PTokenName :--> PInteger)
passetQtyInValueBench = phoistAcyclic $ plam $ \value cs tn ->
    let csBytes = pasByteStr # pforgetData (pdata cs)
        tnBytes = pasByteStr # pforgetData (pdata tn)
        tokenQtyInTokenPairs = pfix #$ plam $ \self remainingTokenPairs ->
            pelimList
                ( \tokenPair tokenPairsRest ->
                    let tokenNameData = pfstBuiltin # tokenPair
                        tokenNameBytes = pasByteStr # pforgetData tokenNameData
                        tokenQty = pfromData (psndBuiltin # tokenPair)
                     in pif
                            (tokenNameBytes #== tnBytes)
                            tokenQty
                            ( pif
                                (tnBytes #< tokenNameBytes)
                                0
                                (self # tokenPairsRest)
                            )
                )
                0
                remainingTokenPairs
        tokenQtyInCurrencyPairs = pfix #$ plam $ \self remainingCurrencyPairs ->
            pelimList
                ( \currencyPair currencyPairsRest ->
                    let currencySymbolData = pfstBuiltin # currencyPair
                        currencySymbolBytes = pasByteStr # pforgetData currencySymbolData
                        tokenPairs = pto (pfromData (psndBuiltin # currencyPair))
                     in pif
                            (currencySymbolBytes #== csBytes)
                            (tokenQtyInTokenPairs # tokenPairs)
                            ( pif
                                (csBytes #< currencySymbolBytes)
                                0
                                (self # currencyPairsRest)
                            )
                )
                0
                remainingCurrencyPairs
     in tokenQtyInCurrencyPairs # pto (pto value)

pvalueToCredBench ::
    Term s PCredential ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s (PValue 'Sorted 'Positive)
pvalueToCredBench cred outputs =
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
                                # (pif (paymentCredData #== credData) (pvalueUnionFastBench # acc # pstripAdaHBench (pfromData txOutValue)) acc)
                                # xs
                )
                acc
        )
            # pemptyLedgerValue
            # outputs

pvalueFromCredBench ::
    Term s PCredential ->
    Term s (PBuiltinList (PAsData PPubKeyHash)) ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace))) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PValue 'Sorted 'Positive)
pvalueFromCredBench cred sigs withdrawalEntries inputs =
    let credData = pforgetData (pdata cred)
     in ( pfix #$ plam $ \self acc ->
            pelimList
                ( \txIn xs ->
                    plet (pdata (ptxInInfoResolved $ pfromData txIn)) $ \resolvedOutData ->
                        let resolvedOutFields = psndBuiltin # (pasConstr # pforgetData resolvedOutData)
                            resolvedOutAddressData = phead # resolvedOutFields
                            resolvedOutFieldsRest = ptail # resolvedOutFields
                            resolvedOutValue = punsafeCoerce @(PAsData (PValue 'Sorted 'Positive)) (phead # resolvedOutFieldsRest)
                            paymentCredData = phead # (psndBuiltin # (pasConstr # resolvedOutAddressData))
                            stakingCredMaybe = punsafeCoerce @(PMaybeData PStakingCredential) (phead # (ptail # (psndBuiltin # (pasConstr # resolvedOutAddressData))))
                         in self
                                # ( pif
                                        (paymentCredData #== credData)
                                        ( pmatch (pjustData stakingCredMaybe) $ \case
                                            PStakingHash ownerCred ->
                                                pmatch ownerCred $ \case
                                                    PPubKeyCredential pkh ->
                                                        pif
                                                            (ptxSignedByPkh # pkh # sigs)
                                                            (pvalueUnionFastBench # acc # pstripAdaHBench (pfromData resolvedOutValue))
                                                            (ptraceInfoError "Missing required pk witness")
                                                    PScriptCredential scriptHash_ ->
                                                        let scriptCredData = pdata $ pcon (PScriptCredential scriptHash_)
                                                         in pif
                                                                (Actual.pisScriptInvokedEntries # scriptCredData # withdrawalEntries)
                                                                (pvalueUnionFastBench # acc # pstripAdaHBench (pfromData resolvedOutValue))
                                                                (ptraceInfoError "Missing required script witness")
                                            _ -> perror
                                        )
                                        acc
                                  )
                                # xs
                )
                acc
        )
            # pemptyLedgerValue
            # inputs

poutputsContainExpectedValueAtCredBench ::
    Term s PCredential ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s (PValue 'Sorted 'Positive) ->
    Term s PBool
poutputsContainExpectedValueAtCredBench progLogicCred txOutputs expectedValue =
    let progLogicCredData = pforgetData (pdata progLogicCred)
        hasAtLeastAssetInProgOutputs = pfix #$ plam $ \self requiredQty currentQty cs tn remainingOutputs ->
            pif
                (currentQty #>= requiredQty)
                (pconstant True)
                ( pelimList
                    ( \txOut outputsRest ->
                        let txOutFields = psndBuiltin # (pasConstr # pforgetData txOut)
                            txOutAddressData = phead # txOutFields
                            txOutFieldsRest = ptail # txOutFields
                            txOutValue = punsafeCoerce @(PAsData (PValue 'Sorted 'Positive)) (phead # txOutFieldsRest)
                            paymentCredData = phead # (psndBuiltin # (pasConstr # txOutAddressData))
                         in pif
                                (paymentCredData #== progLogicCredData)
                                (self # requiredQty # (currentQty + (passetQtyInValueBench # (pfromData txOutValue) # cs # tn)) # cs # tn # outputsRest)
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
                     in (passetQtyInValueBench # actualValue # expectedCurrencySymbol # expectedTokenName #>= expectedTokenQty)
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
        actualValueAtCred = pvalueToCredBench progLogicCred txOutputs
     in pelimList
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

progLogicBaseHash :: ScriptHash
progLogicBaseHash = ScriptHash (bs28 0xaa)

progLogicBaseCred :: Credential
progLogicBaseCred = ScriptCredential progLogicBaseHash

pubKeyHashAt :: Int -> PubKeyHash
pubKeyHashAt idx = PubKeyHash (bs28 (fromIntegral (idx + 1)))

ownerScriptHashAt :: Int -> ScriptHash
ownerScriptHashAt idx = ScriptHash (bs28 (fromIntegral (idx + 101)))

currencySymbolAt :: Int -> CurrencySymbol
currencySymbolAt idx = CurrencySymbol (bs28 (fromIntegral (idx + 1)))

tokenNameAt :: Int -> TokenName
tokenNameAt idx = TokenName (PV1.toBuiltin (BS.singleton (fromIntegral (idx + 1))))

progWalletPubKeyOwnerAddress :: PubKeyHash -> Address
progWalletPubKeyOwnerAddress pkh =
    Address progLogicBaseCred (Just (StakingHash (PubKeyCredential pkh)))

progWalletScriptOwnerAddress :: ScriptHash -> Address
progWalletScriptOwnerAddress sh =
    Address progLogicBaseCred (Just (StakingHash (ScriptCredential sh)))

targetAssetValue :: Integer -> Value
targetAssetValue qty = assetClassValue (assetClass (currencySymbolAt 0) (tokenNameAt 0)) qty

multiAssetValue :: Int -> Integer -> Value
multiAssetValue assetCount qty =
    mkValue [(currencySymbolAt idx, tokenNameAt idx, qty) | idx <- [0 .. assetCount - 1]]

outputCtxDense :: Int -> ScriptContext
outputCtxDense outputCount =
    buildScriptContext $
        foldMap
            (\_ -> withOutput (withTxOutAddress (Address progLogicBaseCred Nothing) <> withTxOutValue (mkAdaValue 2_000_000 <> targetAssetValue 1)))
            [0 .. outputCount - 1]

outputCtxSparse :: Int -> Int -> ScriptContext
outputCtxSparse totalOutputs matchingOutputs =
    buildScriptContext $
        foldMap
            (\_ -> withOutput (withTxOutAddress (Address progLogicBaseCred Nothing) <> withTxOutValue (mkAdaValue 2_000_000 <> targetAssetValue 1)))
            [0 .. matchingOutputs - 1]
            <> foldMap
                (\idx -> withOutput (withTxOutAddress (pubKeyAddress (pubKeyHashAt (idx + 150))) <> withTxOutValue (mkAdaValue 2_000_000 <> targetAssetValue 1)))
                [0 .. totalOutputs - matchingOutputs - 1]

outputCtxMultiAsset :: Int -> Int -> ScriptContext
outputCtxMultiAsset outputCount assetCount =
    buildScriptContext $
        foldMap
            (\_ -> withOutput (withTxOutAddress (Address progLogicBaseCred Nothing) <> withTxOutValue (mkAdaValue 2_000_000 <> multiAssetValue assetCount 1)))
            [0 .. outputCount - 1]

inputCtxPubKeyOwners :: Int -> ScriptContext
inputCtxPubKeyOwners inputCount =
    buildScriptContext $
        withSigners [pubKeyHashAt idx | idx <- [0 .. inputCount - 1]]
            <> foldMap
                ( \idx ->
                    withScriptInput
                        (PlutusTx.toBuiltinData ())
                        ( withAddress (progWalletPubKeyOwnerAddress (pubKeyHashAt idx))
                            <> withValue (mkAdaValue 2_000_000 <> targetAssetValue 1)
                        )
                )
                [0 .. inputCount - 1]

inputCtxScriptOwners :: Int -> ScriptContext
inputCtxScriptOwners inputCount =
    buildScriptContext $
        foldMap (\idx -> withWithdrawal (ScriptCredential (ownerScriptHashAt idx)) 0) [0 .. inputCount - 1]
            <> foldMap
                ( \idx ->
                    withScriptInput
                        (PlutusTx.toBuiltinData ())
                        ( withAddress (progWalletScriptOwnerAddress (ownerScriptHashAt idx))
                            <> withValue (mkAdaValue 2_000_000 <> targetAssetValue 1)
                        )
                )
                [0 .. inputCount - 1]

inputCtxMixedOwners :: Int -> ScriptContext
inputCtxMixedOwners inputCount =
    let half = inputCount `div` 2
     in buildScriptContext $
            withSigners [pubKeyHashAt idx | idx <- [0 .. half - 1]]
                <> foldMap (\idx -> withWithdrawal (ScriptCredential (ownerScriptHashAt idx)) 0) [half .. inputCount - 1]
                <> foldMap
                    ( \idx ->
                        withScriptInput
                            (PlutusTx.toBuiltinData ())
                            ( withAddress (progWalletPubKeyOwnerAddress (pubKeyHashAt idx))
                                <> withValue (mkAdaValue 2_000_000 <> targetAssetValue 1)
                            )
                    )
                    [0 .. half - 1]
                <> foldMap
                    ( \idx ->
                        withScriptInput
                            (PlutusTx.toBuiltinData ())
                            ( withAddress (progWalletScriptOwnerAddress (ownerScriptHashAt idx))
                                <> withValue (mkAdaValue 2_000_000 <> targetAssetValue 1)
                            )
                    )
                    [half .. inputCount - 1]

inputCtxSparse :: Int -> Int -> ScriptContext
inputCtxSparse totalInputs matchingInputs =
    buildScriptContext $
        withSigners [pubKeyHashAt idx | idx <- [0 .. matchingInputs - 1]]
            <> foldMap
                ( \idx ->
                    withScriptInput
                        (PlutusTx.toBuiltinData ())
                        ( withAddress (progWalletPubKeyOwnerAddress (pubKeyHashAt idx))
                            <> withValue (mkAdaValue 2_000_000 <> targetAssetValue 1)
                        )
                )
                [0 .. matchingInputs - 1]
            <> foldMap
                ( \idx ->
                    withInput
                        ( withAddress (pubKeyAddress (pubKeyHashAt (idx + 200)))
                            <> withValue (mkAdaValue 2_000_000 <> targetAssetValue 1)
                        )
                )
                [0 .. totalInputs - matchingInputs - 1]

mkValueToCredTerm :: Credential -> CurrencySymbol -> TokenName -> Integer -> Term s (PScriptContext :--> PBool)
mkValueToCredTerm cred cs tn expectedQty = plam $ \ctx ->
    pmatch (pscriptContextTxInfo ctx) $ \txInfo ->
        let actualValue = pvalueToCredBench (pconstant cred) (pfromData $ ptxInfo'outputs txInfo)
         in passetQtyInValueBench # actualValue # pconstant cs # pconstant tn #== pconstant expectedQty

mkOutputsContainExpectedValueTerm :: Credential -> Value -> Term s (PScriptContext :--> PBool)
mkOutputsContainExpectedValueTerm cred expectedValue = plam $ \ctx ->
    pmatch (pscriptContextTxInfo ctx) $ \txInfo ->
        let expectedValueTerm =
                punsafeCoerce $
                    pconstant @(PValue 'Unsorted 'NoGuarantees) expectedValue
         in poutputsContainExpectedValueAtCredBench
                (pconstant cred)
                (pfromData $ ptxInfo'outputs txInfo)
                expectedValueTerm

mkValueFromCredTerm :: Credential -> CurrencySymbol -> TokenName -> Integer -> Term s (PScriptContext :--> PBool)
mkValueFromCredTerm cred cs tn expectedQty = plam $ \ctx ->
    pmatch (pscriptContextTxInfo ctx) $ \txInfo ->
        let actualValue =
                pvalueFromCredBench
                    (pconstant cred)
                    (pfromData $ ptxInfo'signatories txInfo)
                    (pto $ pfromData $ ptxInfo'wdrl txInfo)
                    (pfromData $ ptxInfo'inputs txInfo)
         in passetQtyInValueBench # actualValue # pconstant cs # pconstant tn #== pconstant expectedQty

mkActualPisScriptInvokedEntriesTerm :: Credential -> Term s (PScriptContext :--> PBool)
mkActualPisScriptInvokedEntriesTerm cred = plam $ \ctx ->
    pmatch (pscriptContextTxInfo ctx) $ \txInfo ->
        Actual.pisScriptInvokedEntries
            # pdata (pconstant cred)
            # (pto $ pfromData $ ptxInfo'wdrl txInfo)

mkActualValueToCredTerm :: Credential -> CurrencySymbol -> TokenName -> Integer -> Term s (PScriptContext :--> PBool)
mkActualValueToCredTerm cred cs tn expectedQty = plam $ \ctx ->
    pmatch (pscriptContextTxInfo ctx) $ \txInfo ->
        let actualValue = Actual.pvalueToCred (pconstant cred) (pfromData $ ptxInfo'outputs txInfo)
         in passetQtyInValueBench # actualValue # pconstant cs # pconstant tn #== pconstant expectedQty

mkActualOutputsContainExpectedValueTerm :: Credential -> Value -> Term s (PScriptContext :--> PBool)
mkActualOutputsContainExpectedValueTerm cred expectedValue = plam $ \ctx ->
    pmatch (pscriptContextTxInfo ctx) $ \txInfo ->
        let expectedValueTerm =
                punsafeCoerce $
                    pconstant @(PValue 'Unsorted 'NoGuarantees) expectedValue
         in Actual.poutputsContainExpectedValueAtCred
                (pconstant cred)
                (pfromData $ ptxInfo'outputs txInfo)
                expectedValueTerm

mkActualValueFromCredTerm :: Credential -> CurrencySymbol -> TokenName -> Integer -> Term s (PScriptContext :--> PBool)
mkActualValueFromCredTerm cred cs tn expectedQty = plam $ \ctx ->
    pmatch (pscriptContextTxInfo ctx) $ \txInfo ->
        let actualValue =
                Actual.pvalueFromCred
                    (pconstant cred)
                    (pfromData $ ptxInfo'signatories txInfo)
                    (pto $ pfromData $ ptxInfo'wdrl txInfo)
                    (pfromData $ ptxInfo'inputs txInfo)
         in -- `pvalueFromCred` returns the raw currency-pair list rather than a wrapped
            -- `PValue`; the representations are identical, so coerce it back here.
            passetQtyInValueBench # punsafeCoerce actualValue # pconstant cs # pconstant tn #== pconstant expectedQty

-- Benchmark catalogue for the isolated utility-term harness.
benchCases :: [BenchCase]
benchCases =
    [ mkCase "isBaseCred.byteString.match" isBaseCredByteStringTerm [PlutusTx.toData baseCredential]
    , mkCase "isBaseCred.byteString.mismatch" isBaseCredByteStringTerm [PlutusTx.toData otherScriptCredential]
    , mkCase "isBaseCred.byteString.pubKeySameHash" isBaseCredByteStringTerm [PlutusTx.toData pubKeyCredentialSameHash]
    , mkCase "isBaseCred.equalsData.match" isBaseCredEqualsDataTerm [PlutusTx.toData baseCredential]
    , mkCase "isBaseCred.equalsData.mismatch" isBaseCredEqualsDataTerm [PlutusTx.toData otherScriptCredential]
    , mkCase "isBaseCred.equalsData.pubKeySameHash" isBaseCredEqualsDataTerm [PlutusTx.toData pubKeyCredentialSameHash]
    , mkHasCredCase "withdrawalScan.equalsData.bestCase.n001" 1 0
    , mkHasCredCase "withdrawalScan.equalsData.bestCase.n020" 20 0
    , mkHasCredCase "withdrawalScan.equalsData.bestCase.n100" 100 0
    , mkHasCredCase "withdrawalScan.equalsData.midCase.n020" 20 10
    , mkHasCredCase "withdrawalScan.equalsData.midCase.n100" 100 50
    , mkHasCredCase "withdrawalScan.equalsData.worstCase.n020" 20 19
    , mkHasCredCase "withdrawalScan.equalsData.worstCase.n050" 50 49
    , mkHasCredCase "withdrawalScan.equalsData.worstCase.n100" 100 99
    , mkActualPisScriptInvokedEntriesCase "actual.pisScriptInvokedEntries.worstCase.n100" (withdrawalCtxWithMatchAt 100 99) 99
    , mkValueToCredCase "local.valueToCred.dense.outputs.n020" (outputCtxDense 20) 20
    , mkValueToCredCase "local.valueToCred.dense.outputs.n100" (outputCtxDense 100) 100
    , mkValueToCredCase "local.valueToCred.sparse.outputs.n100.matching.n020" (outputCtxSparse 100 20) 20
    , mkActualValueToCredCase "actual.valueToCred.dense.outputs.n020" (outputCtxDense 20) 20
    , mkActualValueToCredCase "actual.valueToCred.dense.outputs.n100" (outputCtxDense 100) 100
    , mkActualValueToCredCase "actual.valueToCred.sparse.outputs.n100.matching.n020" (outputCtxSparse 100 20) 20
    , mkOutputsContainSingleCase "local.outputsContain.singleAsset.outputs.n020" (outputCtxDense 20) 20
    , mkOutputsContainSingleCase "local.outputsContain.singleAsset.outputs.n100" (outputCtxDense 100) 100
    , mkOutputsContainMultiCase "local.outputsContain.multiAsset.outputs.n020.assets.n005" (outputCtxMultiAsset 20 5) 5 20
    , mkOutputsContainMultiCase "local.outputsContain.multiAsset.outputs.n050.assets.n005" (outputCtxMultiAsset 50 5) 5 50
    , mkActualOutputsContainSingleCase "actual.outputsContain.singleAsset.outputs.n020" (outputCtxDense 20) 20
    , mkActualOutputsContainSingleCase "actual.outputsContain.singleAsset.outputs.n100" (outputCtxDense 100) 100
    , mkActualOutputsContainMultiCase "actual.outputsContain.multiAsset.outputs.n020.assets.n005" (outputCtxMultiAsset 20 5) 5 20
    , mkActualOutputsContainMultiCase "actual.outputsContain.multiAsset.outputs.n050.assets.n005" (outputCtxMultiAsset 50 5) 5 50
    , mkValueFromCredCase "local.valueFromCred.pubKeyOwners.inputs.n010" (inputCtxPubKeyOwners 10) 10
    , mkValueFromCredCase "local.valueFromCred.pubKeyOwners.inputs.n050" (inputCtxPubKeyOwners 50) 50
    , mkValueFromCredCase "local.valueFromCred.scriptOwners.inputs.n010" (inputCtxScriptOwners 10) 10
    , mkValueFromCredCase "local.valueFromCred.scriptOwners.inputs.n050" (inputCtxScriptOwners 50) 50
    , mkValueFromCredCase "local.valueFromCred.mixedOwners.inputs.n020" (inputCtxMixedOwners 20) 20
    , mkValueFromCredCase "local.valueFromCred.sparse.total.n100.matching.n020" (inputCtxSparse 100 20) 20
    , mkActualValueFromCredCase "actual.valueFromCred.pubKeyOwners.inputs.n010" (inputCtxPubKeyOwners 10) 10
    , mkActualValueFromCredCase "actual.valueFromCred.pubKeyOwners.inputs.n050" (inputCtxPubKeyOwners 50) 50
    , mkActualValueFromCredCase "actual.valueFromCred.scriptOwners.inputs.n010" (inputCtxScriptOwners 10) 10
    , mkActualValueFromCredCase "actual.valueFromCred.scriptOwners.inputs.n050" (inputCtxScriptOwners 50) 50
    , mkActualValueFromCredCase "actual.valueFromCred.mixedOwners.inputs.n020" (inputCtxMixedOwners 20) 20
    , mkActualValueFromCredCase "actual.valueFromCred.sparse.total.n100.matching.n020" (inputCtxSparse 100 20) 20
    ]

mkHasCredCase :: String -> Int -> Int -> BenchCase
mkHasCredCase name totalCount matchIdx =
    mkCase
        name
        hasCredEqualsData
        [ PlutusTx.toData (credentialAtSortedIndex matchIdx)
        , PlutusTx.toData (withdrawalCtxWithMatchAt totalCount matchIdx)
        ]

mkValueToCredCase :: String -> ScriptContext -> Integer -> BenchCase
mkValueToCredCase name ctx expectedQty =
    mkCase
        name
        (mkValueToCredTerm progLogicBaseCred (currencySymbolAt 0) (tokenNameAt 0) expectedQty)
        [PlutusTx.toData ctx]

mkActualPisScriptInvokedEntriesCase :: String -> ScriptContext -> Int -> BenchCase
mkActualPisScriptInvokedEntriesCase name ctx matchIdx =
    mkCase
        name
        (mkActualPisScriptInvokedEntriesTerm (credentialAtSortedIndex matchIdx))
        [PlutusTx.toData ctx]

mkActualValueToCredCase :: String -> ScriptContext -> Integer -> BenchCase
mkActualValueToCredCase name ctx expectedQty =
    mkCase
        name
        (mkActualValueToCredTerm progLogicBaseCred (currencySymbolAt 0) (tokenNameAt 0) expectedQty)
        [PlutusTx.toData ctx]

mkOutputsContainSingleCase :: String -> ScriptContext -> Integer -> BenchCase
mkOutputsContainSingleCase name ctx requiredQty =
    mkCase
        name
        (mkOutputsContainExpectedValueTerm progLogicBaseCred (targetAssetValue requiredQty))
        [PlutusTx.toData ctx]

mkOutputsContainMultiCase :: String -> ScriptContext -> Int -> Integer -> BenchCase
mkOutputsContainMultiCase name ctx assetCount perAssetQty =
    mkCase
        name
        (mkOutputsContainExpectedValueTerm progLogicBaseCred (multiAssetValue assetCount perAssetQty))
        [PlutusTx.toData ctx]

mkActualOutputsContainSingleCase :: String -> ScriptContext -> Integer -> BenchCase
mkActualOutputsContainSingleCase name ctx requiredQty =
    mkCase
        name
        (mkActualOutputsContainExpectedValueTerm progLogicBaseCred (targetAssetValue requiredQty))
        [PlutusTx.toData ctx]

mkActualOutputsContainMultiCase :: String -> ScriptContext -> Int -> Integer -> BenchCase
mkActualOutputsContainMultiCase name ctx assetCount perAssetQty =
    mkCase
        name
        (mkActualOutputsContainExpectedValueTerm progLogicBaseCred (multiAssetValue assetCount perAssetQty))
        [PlutusTx.toData ctx]

mkValueFromCredCase :: String -> ScriptContext -> Integer -> BenchCase
mkValueFromCredCase name ctx expectedQty =
    mkCase
        name
        (mkValueFromCredTerm progLogicBaseCred (currencySymbolAt 0) (tokenNameAt 0) expectedQty)
        [PlutusTx.toData ctx]

mkActualValueFromCredCase :: String -> ScriptContext -> Integer -> BenchCase
mkActualValueFromCredCase name ctx expectedQty =
    mkCase
        name
        (mkActualValueFromCredTerm progLogicBaseCred (currencySymbolAt 0) (tokenNameAt 0) expectedQty)
        [PlutusTx.toData ctx]
