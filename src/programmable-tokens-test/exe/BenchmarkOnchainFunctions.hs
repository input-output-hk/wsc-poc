{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import BenchmarkOnchain.ScriptHelpers (bs28, mkValue, pubKeyAddress)
import BenchmarkOnchain.SimpleRunner (BenchCase, mkTermCase, runSimpleBenchmark)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Plutarch.Core.Context (
    paddressCredential,
    pscriptContextTxInfo,
    ptxInInfoResolved,
 )
import Plutarch.Core.Internal.Builtins (pmapData, ppairDataBuiltinRaw)
import SmartTokens.Core.Builtins (pdropList)
import Plutarch.Core.List (pdropFast)
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
    Datum (Datum),
    OutputDatum (NoOutputDatum, OutputDatum),
    PubKeyHash (PubKeyHash),
    ScriptContext,
    ScriptHash (ScriptHash),
    StakingCredential (StakingHash),
    TokenName (TokenName),
    TxId (TxId),
    TxInInfo (TxInInfo),
    TxOut (TxOut),
    TxOutRef (TxOutRef),
 )
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
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
                    #|| let go = pfixHoisted #$ plam $ \self withdrawals' ->
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
    pfixHoisted #$ plam $ \self tokensA tokensB ->
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
    pfixHoisted #$ plam $ \self csPairsA csPairsB ->
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
        tokenQtyInTokenPairs = pfixHoisted #$ plam $ \self remainingTokenPairs ->
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
        tokenQtyInCurrencyPairs = pfixHoisted #$ plam $ \self remainingCurrencyPairs ->
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
     in ( pfixHoisted #$ plam $ \self acc ->
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
     in ( pfixHoisted #$ plam $ \self acc ->
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
        hasAtLeastAssetInProgOutputs = pfixHoisted #$ plam $ \self requiredQty currentQty cs tn remainingOutputs ->
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
        checkExpectedTokenPairsAgainstActualValue = pfixHoisted #$ plam $ \self actualValue expectedCurrencySymbol remainingExpectedTokenPairs ->
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
        checkExpectedCurrencyPairsAgainstActualValue = pfixHoisted #$ plam $ \self actualValue remainingExpectedCurrencyPairs ->
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
                    pnil
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
        <> decisionBenchCases

-- =====================================================================
-- Decision benchmarks for design-issuance-dual-arm-custody.md pending
-- gates: (a) base forwarding scan vs spend-redeemer index; (b) params
-- datum lookup vs baked compile params; (c) LocalFullScan vs LocalIndexed.
-- =====================================================================

decisionBenchCases :: [BenchCase]
decisionBenchCases =
    [ mkBaseFwdScanCase "decision.a.baseFwd.scan.wdrl002.pos01" 2 1
    , mkBaseFwdScanCase "decision.a.baseFwd.scan.wdrl011.pos05" 11 5
    , mkBaseFwdScanCase "decision.a.baseFwd.scan.wdrl011.pos10" 11 10
    , mkBaseFwdScanCase "decision.a.baseFwd.scan.wdrl020.pos19" 20 19
    , mkBaseFwdIndexedCase "decision.a.baseFwd.indexed.wdrl002.pos01" 2 1
    , mkBaseFwdIndexedCase "decision.a.baseFwd.indexed.wdrl011.pos05" 11 5
    , mkBaseFwdIndexedCase "decision.a.baseFwd.indexed.wdrl011.pos10" 11 10
    , mkBaseFwdIndexedCase "decision.a.baseFwd.indexed.wdrl020.pos19" 20 19
    , mkParamsLookupCase "decision.b.params.datumSourced.refs01.idx00" 1 0
    , mkParamsLookupCase "decision.b.params.datumSourced.refs03.idx02" 3 2
    , mkParamsLookupCase "decision.b.params.datumSourced.refs05.idx04" 5 4
    , mkCase "decision.b.params.bakedFloor" pparamsBakedFloor [PlutusTx.toData (0 :: Integer), PlutusTx.toData (0 :: Integer)]
    , mkFullScanCase "decision.c.local.fullScan.outs002" (localCustodyCtx 2 1 False 1 False)
    , mkFullScanCase "decision.c.local.fullScan.outs020" (localCustodyCtx 20 2 False 1 False)
    , mkFullScanCase "decision.c.local.fullScan.outs050" (localCustodyCtx 50 2 False 1 False)
    , mkLocalIndexedCase "decision.c.local.indexed.outs002.dests01.ins01" (localCustodyCtx 2 1 False 1 False) [0] 100
    , mkLocalIndexedCase "decision.c.local.indexed.outs020.dests02.ins01" (localCustodyCtx 20 2 False 1 False) [0, 1] 200
    , mkLocalIndexedCase "decision.c.local.indexed.outs020.dests02.late.ins01" (localCustodyCtx 20 2 True 1 False) [18, 19] 200
    , mkLocalIndexedCase "decision.c.local.indexed.outs050.dests02.ins01" (localCustodyCtx 50 2 False 1 False) [0, 1] 200
    , mkLocalIndexedCase "decision.c.local.indexed.outs020.dests02.ins05" (localCustodyCtx 20 2 False 5 False) [0, 1] 200
    , mkLocalIndexedCase "decision.c.local.indexed.outs020.dests02.ins20tok" (localCustodyCtx 20 2 False 20 True) [0, 1] 200
    , mkLocalIndexedCase "decision.c.local.indexed.outs020.dests02.ins50tok" (localCustodyCtx 20 2 False 50 True) [0, 1] 200
    ]
        <> dropDecisionCases
        <> refWalkCases
        <> credCompareCases
        <> containScanCases
        <> ownerWitnessCases
        <> seizeDiffCases

-- Decision benchmarks for the Van Rossem dropList adoption: the
-- plutarch-onchain-lib tail-walk ('pdropFast', ptails30/20/10 unrolling +
-- remainder loop) versus the PV11 dropList builtin, fetching the element at
-- index n from a 300-element list.
dropDecisionCases :: [BenchCase]
dropDecisionCases =
    [ mkCase ("decision.d.drop.tailLoop.n" <> pad n) pdropFastHead (dropArgs n)
    | n <- dropSizes
    ]
        <> [ mkCase ("decision.d.drop.builtin.n" <> pad n) pdropListHead (dropArgs n)
           | n <- dropSizes
           ]
  where
    dropSizes = [1, 5, 20, 100, 255]
    pad n = let s = show n in replicate (3 - length s) '0' <> s
    dropInput :: [Integer]
    dropInput = [0 .. 299]
    dropArgs n = [PlutusTx.toData (n :: Integer), PlutusTx.toData dropInput]

-- Decision benchmarks for reference-input index encoding in the transfer proof
-- walk. The walk resolves one directory node per programmable policy. With
-- ABSOLUTE indices every lookup restarts at the head of the reference-input
-- list, so the drops sum to 1+2+...+p; with RELATIVE indices (the encoding
-- 'absoluteToRelativeInputIdxs' already uses for seize input indices) each
-- lookup continues from the previous position, so the drops sum to p. Both
-- terms end at the same element and pay the same pasInt/pasList decodes, so the
-- difference is exactly the quadratic term.
refWalkCases :: [BenchCase]
refWalkCases =
    [ mkCase ("decision.e.refwalk.absolute.p" <> pad p) pRefWalkAbsolute (walkArgs p [1 .. toInteger p])
    | p <- walkSizes
    ]
        <> [ mkCase ("decision.e.refwalk.relative.p" <> pad p) pRefWalkRelative (walkArgs p (replicate p (1 :: Integer)))
           | p <- walkSizes
           ]
        -- Leanest possible pair: identical loop shape and identical arity, the
        -- ONLY difference being whether the drop starts from the closed-over
        -- full list (absolute) or from the threaded suffix (relative).
        <> [ mkCase ("decision.e.refwalk2.absolute.p" <> pad p) pRefWalk2Absolute (walkArgs p [1 .. toInteger p])
           | p <- walkSizes
           ]
        <> [ mkCase ("decision.e.refwalk2.relative.p" <> pad p) pRefWalk2Relative (walkArgs p (replicate p (1 :: Integer)))
           | p <- walkSizes
           ]
  where
    walkSizes = [5, 10, 20, 40, 80]
    pad n = let str = show n in replicate (3 - length str) '0' <> str
    walkArgs :: Int -> [Integer] -> [Data]
    walkArgs p idxs =
        [ PlutusTx.toData idxs
        , PlutusTx.toData [0 .. toInteger p]
        ]

-- ===================================================================
-- (h) Owner-script witness lookup: scan versus witnessed index.
--
-- Every mini-ledger input owned by a SCRIPT must prove that script is
-- invoked, which today means scanning the credential-sorted withdrawal map.
-- The cost therefore depends on where the owner's hash happens to sort
-- against the other participants' -- including the global validator's own,
-- which nobody controls. A redeemer-witnessed index would make it flat.
-- These cases measure one lookup at varying map size and target position.
-- ===================================================================
ownerWitnessCases :: [BenchCase]
ownerWitnessCases =
    concat
        [ [ mkCase ("decision.h.owner.scan.wdrl" <> pad3 n <> ".pos" <> pad3 i) pOwnerScan (ownerArgs n i)
          , mkCase ("decision.h.owner.indexed.wdrl" <> pad3 n <> ".pos" <> pad3 i) pOwnerIndexed (ownerArgs n i)
          ]
        | (n, i) <- [(2, 0), (2, 1), (4, 0), (4, 3), (8, 7), (20, 19)]
        ]
  where
    pad3 n = let str = show n in replicate (3 - length str) '0' <> str

-- | A withdrawal map of @n@ script credentials in ledger order, and the
-- credential sitting at position @i@ as the lookup target.
ownerArgs :: Int -> Int -> [Data]
ownerArgs n i =
    [ PlutusTx.toData (ScriptCredential (ScriptHash (bs28 (fromIntegral (i + 1)))))
    , PlutusTx.toData (AssocMap.unsafeFromList [(ScriptCredential (ScriptHash (bs28 (fromIntegral (k + 1)))), 0 :: Integer) | k <- [0 .. n - 1]])
    , PlutusTx.toData (toInteger i)
    ]

pOwnerScan :: forall s. Term s (PData :--> PData :--> PData :--> PUnit)
pOwnerScan = plam $ \credD wdrlD _idxD ->
    pif
        ( Actual.pisScriptInvokedEntries
            # punsafeCoerce credD
            # (punsafeCoerce (pasMap # wdrlD) :: Term s (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace))))
        )
        (pconstant ())
        perror

pOwnerIndexed :: forall s. Term s (PData :--> PData :--> PData :--> PUnit)
pOwnerIndexed = plam $ \credD wdrlD idxD ->
    pif
        ( pforgetData (pfstBuiltin # (phead # (pdropList # (pasInt # idxD) # (punsafeCoerce (pasMap # wdrlD) :: Term s (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace)))))))
            #== credD
        )
        (pconstant ())
        perror

-- ===================================================================
-- (i) Seize per-pair value difference.
--
-- For each seized input/output pair the validator must establish that only
-- the seized policy changed (ada may be topped up) and extract that
-- policy's delta. Today it does so with the CIP-153 Value builtins:
-- unValueData both sides, negate the output, union, then convert back.
--
-- The candidate never materialises a builtin Value. Both values are
-- canonical and ada-first, so: drop the ada entry from each, walk the
-- remaining policy list to lift out the seized policy, compare what is left
-- with ONE equalsData, and check ada separately with >=.
-- ===================================================================
seizeDiffCases :: [BenchCase]
seizeDiffCases =
    concat
        [ [ mkCase ("decision.i.seizediff.builtins.pol" <> pad2 k) pSeizeDiffBuiltins (seizeArgs k)
          , mkCase ("decision.i.seizediff.stripeq.pol" <> pad2 k) pSeizeDiffStripEq (seizeArgs k)
          , mkCase ("decision.i.seizediff.lockstep.pol" <> pad2 k) pSeizeDiffLockstep (seizeArgs k)
          ]
        | k <- [1, 2, 4, 10]
        ]
  where
    pad2 n = let str = show n in replicate (2 - length str) '0' <> str

seizeProgCS :: CurrencySymbol
seizeProgCS = CurrencySymbol (bs28 0x40)

{- | An input holding ada, @k@ untouched policies, and the seized policy; and
the corresponding output, identical except that the seized policy is reduced.
The seized symbol sorts after the untouched ones so the walk has to pass them.
-}
seizeArgs :: Int -> [Data]
seizeArgs k =
    [ PlutusTx.toData seizeProgCS
    , PlutusTx.toData (seizeValue 100)
    , PlutusTx.toData (seizeValue 40)
    ]
  where
    seizeValue progQty =
        assetClassValue (assetClass (CurrencySymbol "") (TokenName "")) 5_000_000
            <> mconcat
                [ assetClassValue (assetClass (CurrencySymbol (bs28 (fromIntegral (0x10 + j)))) (TokenName "tok")) 7
                | j <- [1 .. k]
                ]
            <> assetClassValue (assetClass seizeProgCS (TokenName "SEIZED")) progQty

-- | Deployed implementation, called directly so the comparison cannot drift.
pSeizeDiffBuiltins :: Term s (PData :--> PData :--> PData :--> PInteger)
pSeizeDiffBuiltins = plam $ \progCSD inV outV ->
    pSumTokenQtys # Actual.pvalueEqualsDeltaCurrencySymbol (punsafeCoerce progCSD) inV outV

{- | Candidate: strip ada and the seized policy from both sides, prove the
remainder identical with one 'equalsData', and subtract the seized policy's
token maps.
-}
pSeizeDiffStripEq :: forall s. Term s (PData :--> PData :--> PData :--> PInteger)
pSeizeDiffStripEq = plam $ \progCSD inV outV ->
    plet (punsafeCoerce (pasMap # inV) :: Term s CsPairs) $ \inPairs ->
        plet (punsafeCoerce (pasMap # outV) :: Term s CsPairs) $ \outPairs ->
            -- Ledger invariant: ada is present and sorts first, so the head is
            -- lovelace without needing to look at its key.
            plet (pAdaQty # (phead # inPairs)) $ \inAda ->
                plet (pAdaQty # (phead # outPairs)) $ \outAda ->
                    plet (ptail # inPairs) $ \inRest ->
                        plet (ptail # outPairs) $ \outRest ->
                            pif
                                ( -- everything except ada and the seized policy is untouched
                                  (pmapData # punsafeCoerce (pDropCS # progCSD # inRest))
                                    #== (pmapData # punsafeCoerce (pDropCS # progCSD # outRest))
                                    -- ada may only be topped up, never reduced
                                    #&& (inAda #<= outAda)
                                )
                                ( plet (pTokensOfCS # progCSD # inRest) $ \inTok ->
                                    pelimList
                                        (\_ _ -> pSumTokenPairDiff # inTok # (pTokensOfCS # progCSD # outRest))
                                        (ptraceInfoError "seize: paired input does not hold the seized policy")
                                        inTok
                                )
                                (ptraceInfoError "corresponding output: value changed outside the seized policy")

{- | Third candidate: never allocate. Walk both currency lists in lockstep,
skipping the seized policy on each side and proving every other entry equal
with 'equalsData' on the key and the value. No 'pmapData' rebuild, no
intermediate lists -- the objection to 'stripeq'.
-}
pSeizeDiffLockstep :: forall s. Term s (PData :--> PData :--> PData :--> PInteger)
pSeizeDiffLockstep = plam $ \progCSD inV outV ->
    plet (punsafeCoerce (pasMap # inV) :: Term s CsPairs) $ \inPairs ->
        plet (punsafeCoerce (pasMap # outV) :: Term s CsPairs) $ \outPairs ->
            pif
                (pAdaQty # (phead # inPairs) #<= pAdaQty # (phead # outPairs))
                ( plet (pTokensOfCS # progCSD # (ptail # inPairs)) $ \inTok ->
                    pelimList
                        ( \_ _ ->
                            pif
                                (pLockstepEqExcept # progCSD # (ptail # inPairs) # (ptail # outPairs))
                                (pSumTokenPairDiff # inTok # (pTokensOfCS # progCSD # (ptail # outPairs)))
                                (ptraceInfoError "corresponding output: value changed outside the seized policy")
                        )
                        (ptraceInfoError "seize: paired input does not hold the seized policy")
                        inTok
                )
                (ptraceInfoError "corresponding output: value changed outside the seized policy")

-- | Both lists equal once the target symbol is skipped on either side.
pLockstepEqExcept :: Term s (PData :--> CsPairs :--> CsPairs :--> PBool)
pLockstepEqExcept = phoistAcyclic $
    pfixHoisted #$ plam $ \self target as bs ->
        pelimList
            ( \a as' ->
                pif
                    (pforgetData (pfstBuiltin # a) #== target)
                    (self # target # as' # bs)
                    ( pelimList
                        ( \b bs' ->
                            pif
                                (pforgetData (pfstBuiltin # b) #== target)
                                (self # target # as # bs')
                                ( pif
                                    ( (pforgetData (pfstBuiltin # a) #== pforgetData (pfstBuiltin # b))
                                        #&& (pforgetData (psndBuiltin # a) #== pforgetData (psndBuiltin # b))
                                    )
                                    (self # target # as' # bs')
                                    (pconstant False)
                                )
                        )
                        (pconstant False)
                        bs
                    )
            )
            (pelimList (\b bs' -> pif (pforgetData (pfstBuiltin # b) #== target) (self # target # pnil # bs') (pconstant False)) (pconstant True) bs)
            as

type CsPairs = PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))

type TokPairs = PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))

-- | Lovelace quantity out of the leading (ada) currency pair.
pAdaQty :: Term s (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)) :--> PInteger)
pAdaQty = phoistAcyclic $ plam $ \pair ->
    pfromData (psndBuiltin # (phead # pto (pfromData (psndBuiltin # pair))))

-- | The currency-pair list with the target symbol removed, canonical order kept.
pDropCS :: Term s (PData :--> CsPairs :--> CsPairs)
pDropCS = phoistAcyclic $
    pfixHoisted #$ plam $ \self target pairs ->
        pelimList
            ( \pair rest ->
                pif
                    (pforgetData (pfstBuiltin # pair) #== target)
                    rest
                    (pcons # pair #$ self # target # rest)
            )
            pnil
            pairs

-- | The target symbol's token pairs, or empty when it is absent.
pTokensOfCS :: Term s (PData :--> CsPairs :--> TokPairs)
pTokensOfCS = phoistAcyclic $
    pfixHoisted #$ plam $ \self target pairs ->
        pelimList
            ( \pair rest ->
                pif
                    (pforgetData (pfstBuiltin # pair) #== target)
                    (pto (pfromData (psndBuiltin # pair)))
                    (self # target # rest)
            )
            pnil
            pairs

-- | Sum a token-pair list's quantities; forces the whole result.
pSumTokenQtys :: Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PInteger)
pSumTokenQtys = phoistAcyclic $
    pfixHoisted #$ plam $ \self pairs ->
        pelimList (\pair rest -> pfromData (psndBuiltin # pair) + (self # rest)) 0 pairs

-- | Sum of (input - output) over two sorted token-pair lists.
pSumTokenPairDiff :: Term s (TokPairs :--> TokPairs :--> PInteger)
pSumTokenPairDiff = phoistAcyclic $
    pfixHoisted #$ plam $ \self insT outsT ->
        pelimList
            ( \i is ->
                pelimList
                    ( \o os ->
                        pif
                            (pfstBuiltin # i #== pfstBuiltin # o)
                            (pfromData (psndBuiltin # i) - pfromData (psndBuiltin # o) + (self # is # os))
                            (pfromData (psndBuiltin # i) + (self # is # outsT))
                    )
                    (pfromData (psndBuiltin # i) + (self # is # pnil))
                    outsT
            )
            0
            insT

-- Isolated benchmark of the SINGLE-ASSET containment scan in
-- 'poutputsContainExpectedValueAtCred' -- the loop that walks the transaction
-- outputs, picks the ones at the programmable base credential, and sums one
-- asset until the required quantity is reached.
--
-- Two variants, byte-identical except for the two lines under test: the
-- deployed one decodes each output into a Plutarch 'PTxOut' and compares
-- 'PCredential' structurally; the candidate indexes the constructor and
-- compares the credential as Data. Both share one 'passetQtyInPairs', so the
-- asset lookup is common to both and drops out of the difference.
--
-- Run over two shapes: ~17 matching outputs (the mainnet DEX transaction) and
-- 80 (the ManyOutputs benchmark), because those two disagreed about which
-- variant is faster when measured as whole transactions.
containScanCases :: [BenchCase]
containScanCases =
    concat
        [ [ mkCase ("decision.g.contain.typed." <> label) pContainScanTyped (scanArgs matching padding)
          , mkCase ("decision.g.contain.rawdata." <> label) pContainScanRaw (scanArgs matching padding)
          ]
        | (label, matching, padding) <-
            [ ("match017", 17, 1)
            , ("match080", 80, 1)
            , ("match017.skip064", 17, 64)
            , ("match001.skip016", 1, 16)
            ]
        ]

scanBaseCred :: Credential
scanBaseCred = ScriptCredential (ScriptHash (bs28 0x12))

scanNightCS :: CurrencySymbol
scanNightCS = CurrencySymbol (bs28 0x1b)

scanNightTN :: TokenName
scanNightTN = TokenName "NIGHT"

-- | One output holding ada plus the tracked asset. 'atBase' decides whether it
-- sits at the programmable base credential or at an unrelated pubkey address.
scanOutput :: Bool -> Integer -> TxOut
scanOutput atBase qty =
    (
          TxOut
            ( if atBase
                then Address scanBaseCred (Just (StakingHash (PubKeyCredential (PubKeyHash (bs28 0x01)))))
                else Address (PubKeyCredential (PubKeyHash (bs28 0x07))) Nothing
            )
            ( assetClassValue (assetClass (CurrencySymbol "") (TokenName "")) 2_000_000
                <> assetClassValue (assetClass scanNightCS scanNightTN) qty
            )
            NoOutputDatum
            Nothing
        )

-- | Required quantity equals the total held by the matching outputs, so the
-- scan must visit every one of them -- no early exit skewing the comparison.
scanArgs :: Integer -> Integer -> [Data]
scanArgs matching padding =
    [ PlutusTx.toData scanBaseCred
    , PlutusTx.toData
        ( [scanOutput False 5 | _ <- [1 .. padding]]
            <> [scanOutput True 3 | _ <- [1 .. matching]]
        )
    , PlutusTx.toData scanNightCS
    , PlutusTx.toData scanNightTN
    , PlutusTx.toData (3 * matching)
    ]

-- | Shared asset lookup: identical in both variants.
pScanAssetQty ::
    Term
        s
        ( PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))
            :--> PCurrencySymbol
            :--> PTokenName
            :--> PInteger
        )
pScanAssetQty = phoistAcyclic $ plam $ \csPairs cs tn ->
    let tokenQtyInTokenPairs = pfixHoisted #$ plam $ \self remainingTokenPairs ->
            pelimList
                ( \tokenPair tokenPairsRest ->
                    let tokenName = pfromData (pfstBuiltin # tokenPair)
                        tokenQty = pfromData (psndBuiltin # tokenPair)
                     in pif (tokenName #== tn) tokenQty (pif (tn #< tokenName) 0 (self # tokenPairsRest))
                )
                0
                remainingTokenPairs
        tokenQtyInCurrencyPairs = pfixHoisted #$ plam $ \self remainingCurrencyPairs ->
            pelimList
                ( \currencyPair currencyPairsRest ->
                    let currencySymbol = pfromData (pfstBuiltin # currencyPair)
                        tokenPairs = pto (pfromData (psndBuiltin # currencyPair))
                     in pif
                            (currencySymbol #== cs)
                            (tokenQtyInTokenPairs # tokenPairs)
                            (pif (cs #< currencySymbol) 0 (self # currencyPairsRest))
                )
                0
                remainingCurrencyPairs
     in tokenQtyInCurrencyPairs # csPairs

-- | Deployed variant: typed 'PTxOut' decode, structural 'PCredential' compare.
pContainScanTyped :: forall s. Term s (PData :--> PData :--> PData :--> PData :--> PData :--> PUnit)
pContainScanTyped = plam $ \credD outsD csD tnD reqD ->
    plet (pfromData (punsafeCoerce credD :: Term s (PAsData PCredential))) $ \progLogicCred ->
        plet (punsafeCoerce (pasList # outsD) :: Term s (PBuiltinList (PAsData PTxOut))) $ \outs ->
            plet (punsafeCoerce (pasByteStr # csD) :: Term s PCurrencySymbol) $ \cs0 ->
                plet (punsafeCoerce (pasByteStr # tnD) :: Term s PTokenName) $ \tn0 ->
                    let go = pfixHoisted #$ plam $ \self requiredQty currentQty cs tn remainingOutputs ->
                            pif
                                (currentQty #>= requiredQty)
                                (pconstant True)
                                ( pelimList
                                    ( \txOut outputsRest ->
                                        pmatch (pfromData txOut) $ \(PTxOut{ptxOut'address, ptxOut'value}) ->
                                            pif
                                                (paddressCredential ptxOut'address #== progLogicCred)
                                                (self # requiredQty # (currentQty + (pScanAssetQty # pto (pto (pfromData ptxOut'value)) # cs # tn)) # cs # tn # outputsRest)
                                                (self # requiredQty # currentQty # cs # tn # outputsRest)
                                    )
                                    (currentQty #>= requiredQty)
                                    remainingOutputs
                                )
                     in pif (go # (pasInt # reqD) # 0 # cs0 # tn0 # outs) (pconstant ()) perror

-- | Candidate variant: index the constructor, compare the credential as Data.
pContainScanRaw :: forall s. Term s (PData :--> PData :--> PData :--> PData :--> PData :--> PUnit)
pContainScanRaw = plam $ \credD outsD csD tnD reqD ->
    plet credD $ \progLogicCredData ->
        plet (punsafeCoerce (pasList # outsD) :: Term s (PBuiltinList (PAsData PTxOut))) $ \outs ->
            plet (punsafeCoerce (pasByteStr # csD) :: Term s PCurrencySymbol) $ \cs0 ->
                plet (punsafeCoerce (pasByteStr # tnD) :: Term s PTokenName) $ \tn0 ->
                    let go = pfixHoisted #$ plam $ \self requiredQty currentQty cs tn remainingOutputs ->
                            pif
                                (currentQty #>= requiredQty)
                                (pconstant True)
                                ( pelimList
                                    ( \txOut outputsRest ->
                                        plet (psndBuiltin # (pasConstr # pforgetData txOut)) $ \txOutFields ->
                                            let paymentCredData = phead # (psndBuiltin # (pasConstr # (phead # txOutFields)))
                                                txOutValueData = phead # (ptail # txOutFields)
                                             in pif
                                                    (paymentCredData #== progLogicCredData)
                                                    (self # requiredQty # (currentQty + (pScanAssetQty # punsafeCoerce (pasMap # txOutValueData) # cs # tn)) # cs # tn # outputsRest)
                                                    (self # requiredQty # currentQty # cs # tn # outputsRest)
                                    )
                                    (currentQty #>= requiredQty)
                                    remainingOutputs
                                )
                     in pif (go # (pasInt # reqD) # 0 # cs0 # tn0 # outs) (pconstant ()) perror

-- Decision benchmarks for how the output walks should compare a payment
-- credential. Plutarch's typed PEq on PCredential compiles to unConstrData on
-- both sides, an equalsInteger on the constructor tags, and only then a
-- comparison of the payload -- so a tag mismatch costs almost nothing. A single
-- equalsData is fewer builtins, but Data-comparison builtins are charged on the
-- SIZE of their arguments, not on where the difference is found. Which one wins
-- therefore depends on how often the walk sees a credential that matches.
credCompareCases :: [BenchCase]
credCompareCases =
    [ mkCase "decision.f.cred.equalsData.match" pCredEqualsData [scriptCredData 0x12, scriptCredData 0x12]
    , mkCase "decision.f.cred.equalsData.tagDiffers" pCredEqualsData [scriptCredData 0x12, pubKeyCredData 0x12]
    , mkCase "decision.f.cred.equalsData.hashDiffers" pCredEqualsData [scriptCredData 0x12, scriptCredData 0x99]
    , mkCase "decision.f.cred.typed.match" pCredTyped [scriptCredData 0x12, scriptCredData 0x12]
    , mkCase "decision.f.cred.typed.tagDiffers" pCredTyped [scriptCredData 0x12, pubKeyCredData 0x12]
    , mkCase "decision.f.cred.typed.hashDiffers" pCredTyped [scriptCredData 0x12, scriptCredData 0x99]
    ]

scriptCredData :: Word8 -> Data
scriptCredData w = PlutusTx.toData (ScriptCredential (ScriptHash (bs28 w)))

pubKeyCredData :: Word8 -> Data
pubKeyCredData w = PlutusTx.toData (PubKeyCredential (PubKeyHash (bs28 w)))

pCredEqualsData :: Term s (PData :--> PData :--> PBool)
pCredEqualsData = plam $ \a b -> a #== b

pCredTyped :: Term s (PAsData PCredential :--> PAsData PCredential :--> PBool)
pCredTyped = plam $ \a b -> pfromData a #== pfromData b

pRefWalk2Absolute :: Term s (PData :--> PData :--> PData)
pRefWalk2Absolute = plam $ \idxsData xsData ->
    plet (pasList # xsData) $ \xs ->
        let go = pfixHoisted #$ plam $ \self is acc ->
                pelimList
                    (\i is' -> self # is' # (pdropList # (pasInt # i) # xs))
                    acc
                    is
         in phead # (go # (pasList # idxsData) # xs)

pRefWalk2Relative :: Term s (PData :--> PData :--> PData)
pRefWalk2Relative = plam $ \idxsData xsData ->
    plet (pasList # xsData) $ \xs ->
        let go = pfixHoisted #$ plam $ \self is cur ->
                pelimList
                    (\i is' -> self # is' # (pdropList # (pasInt # i) # cur))
                    cur
                    is
         in phead # (go # (pasList # idxsData) # xs)

pRefWalkAbsolute :: Term s (PData :--> PData :--> PData)
pRefWalkAbsolute = plam $ \idxsData xsData ->
    plet (pasList # xsData) $ \xs ->
        let go = pfixHoisted #$ plam $ \self is acc ->
                pelimList
                    (\i is' -> self # is' # (phead # (pdropList # (pasInt # i) # xs)))
                    acc
                    is
         in go # (pasList # idxsData) # (phead # xs)

pRefWalkRelative :: Term s (PData :--> PData :--> PData)
pRefWalkRelative = plam $ \idxsData xsData ->
    plet (pasList # xsData) $ \xs ->
        let go = pfixHoisted #$ plam $ \self is cur acc ->
                pelimList
                    ( \i is' ->
                        plet (pdropList # (pasInt # i) # cur) $ \cur' ->
                            self # is' # cur' # (phead # cur')
                    )
                    acc
                    is
         in go # (pasList # idxsData) # xs # (phead # xs)

-- Args arrive as Data constants from the harness; both candidates pay the
-- identical pasInt/pasList decode so the comparison isolates the drop itself.
pdropFastHead :: Term s (PData :--> PData :--> PData)
pdropFastHead = plam $ \n xs -> phead # (pdropFast # (pasInt # n) # (pasList # xs))

pdropListHead :: Term s (PData :--> PData :--> PData)
pdropListHead = plam $ \n xs -> phead # (pdropList # (pasInt # n) # (pasList # xs))

-- ---- (a) base forwarding ----

-- Exact replica of the deployed `mkProgrammableLogicBase` loop, including the
-- unshared `pfstBuiltin` application in the two compares.
pbaseFwdScan :: Term s (PAsData PCredential :--> PAsData PCredential :--> PScriptContext :--> PBool)
pbaseFwdScan = plam $ \globalCred seizeCred ctx ->
    pmatch (pscriptContextTxInfo ctx) $ \txInfo ->
        let wdrls = pto $ pfromData $ ptxInfo'wdrl txInfo
            go = pfixHoisted #$ plam $ \self withdrawals' ->
                pelimList
                    ( \withdrawal rest ->
                        let c = pfstBuiltin # withdrawal
                         in (c #== globalCred) #|| (c #== seizeCred) #|| (self # rest)
                    )
                    (pconstant False)
                    withdrawals'
         in go # wdrls

-- Candidate: spend redeemer carries the withdrawal-list index (decode included).
pbaseFwdIndexed :: Term s (PData :--> PAsData PCredential :--> PAsData PCredential :--> PScriptContext :--> PBool)
pbaseFwdIndexed = plam $ \idxData globalCred seizeCred ctx ->
    pmatch (pscriptContextTxInfo ctx) $ \txInfo ->
        let wdrls = pto $ pfromData $ ptxInfo'wdrl txInfo
         in plet (pfstBuiltin # (phead # (pdropFast # (pasInt # idxData) # wdrls))) $ \c ->
                (c #== globalCred) #|| (c #== seizeCred)

mkBaseFwdScanCase :: String -> Int -> Int -> BenchCase
mkBaseFwdScanCase name totalCount matchIdx =
    mkCase
        name
        pbaseFwdScan
        [ PlutusTx.toData (credentialAtSortedIndex matchIdx)
        , PlutusTx.toData (credentialAtSortedIndex (totalCount + 40))
        , PlutusTx.toData (withdrawalCtxWithMatchAt totalCount matchIdx)
        ]

mkBaseFwdIndexedCase :: String -> Int -> Int -> BenchCase
mkBaseFwdIndexedCase name totalCount matchIdx =
    mkCase
        name
        pbaseFwdIndexed
        [ PlutusTx.toData (fromIntegral matchIdx :: Integer)
        , PlutusTx.toData (credentialAtSortedIndex matchIdx)
        , PlutusTx.toData (credentialAtSortedIndex (totalCount + 40))
        , PlutusTx.toData (withdrawalCtxWithMatchAt totalCount matchIdx)
        ]

-- ---- (b) params datum lookup ----

paramsAnchorCS :: CurrencySymbol
paramsAnchorCS = currencySymbolAt 40

paramsDirCS :: CurrencySymbol
paramsDirCS = currencySymbolAt 30

paramsGlobalCred :: Credential
paramsGlobalCred = ScriptCredential (ScriptHash (bs28 0xb1))

paramsSeizeCred :: Credential
paramsSeizeCred = ScriptCredential (ScriptHash (bs28 0xb2))

paramsDatum4 :: Data
paramsDatum4 =
    PlutusTx.List
        [ PlutusTx.toData paramsDirCS
        , PlutusTx.toData progLogicBaseCred
        , PlutusTx.toData paramsGlobalCred
        , PlutusTx.toData paramsSeizeCred
        ]

mkRefTxIn :: Integer -> TxOut -> TxInInfo
mkRefTxIn ix out = TxInInfo (TxOutRef (TxId (PV1.toBuiltin (BS.replicate 32 7))) ix) out

paramsAnchorTxOut :: TxOut
paramsAnchorTxOut =
    TxOut
        (Address (ScriptCredential (ScriptHash (bs28 0xee))) Nothing)
        (mkAdaValue 2_000_000 <> assetClassValue (assetClass paramsAnchorCS (TokenName "pp")) 1)
        (OutputDatum (Datum (PlutusTx.dataToBuiltinData paramsDatum4)))
        Nothing

decoyRefTxOut :: Int -> TxOut
decoyRefTxOut i =
    TxOut
        (Address (ScriptCredential (ScriptHash (bs28 0xdd))) Nothing)
        (mkAdaValue 2_000_000 <> assetClassValue (assetClass (currencySymbolAt (20 + i)) (tokenNameAt 0)) 1)
        (OutputDatum (Datum (PlutusTx.dataToBuiltinData (PlutusTx.List [PlutusTx.I 0]))))
        Nothing

refInputsFixture :: Int -> Int -> [TxInInfo]
refInputsFixture total pos =
    [mkRefTxIn (fromIntegral i) (if i == pos then paramsAnchorTxOut else decoyRefTxOut i) | i <- [0 .. total - 1]]

-- Datum-sourced: indexed ref-input access + first-non-Ada authentication + raw
-- field reads (defer-arm shape: directory CS at field 0, global cred at field 2),
-- consumed by the same two comparisons the baked floor performs.
pparamsLookupDatum :: Term s (PData :--> PData :--> PBool)
pparamsLookupDatum = plam $ \idxData refInputsData ->
    plet (psndBuiltin # (pasConstr # pforgetData (phead # (pdropFast # (pasInt # idxData) # punsafeCoerce @(PBuiltinList (PAsData PTxInInfo)) (pasList # refInputsData))))) $ \txInFields ->
        plet (psndBuiltin # (pasConstr # (phead # (ptail # txInFields)))) $ \outFields ->
            let valuePairs = pto (pto (pfromData (punsafeCoerce @(PAsData (PValue 'Sorted 'Positive)) (phead # (ptail # outFields)))))
                firstNonAdaCS = pforgetData (pfstBuiltin # (phead # (ptail # valuePairs)))
                datumField = phead # (ptail # (ptail # outFields))
                payload = phead # (psndBuiltin # (pasConstr # datumField))
             in plet (pasList # payload) $ \fields ->
                    (firstNonAdaCS #== pforgetData (pdata (pconstant @PCurrencySymbol paramsAnchorCS)))
                        #&& ((phead # (ptail # (ptail # fields))) #== pforgetData (pdata (pconstant @PCredential paramsGlobalCred)))
                        #&& ((phead # fields) #== pforgetData (pdata (pconstant @PCurrencySymbol paramsDirCS)))

-- Baked floor: the same two consuming comparisons with zero lookup machinery.
pparamsBakedFloor :: Term s (PData :--> PData :--> PBool)
pparamsBakedFloor = plam $ \_ _ ->
    (pforgetData (pdata (pconstant @PCredential paramsGlobalCred)) #== pforgetData (pdata (pconstant @PCredential paramsGlobalCred)))
        #&& (pforgetData (pdata (pconstant @PCurrencySymbol paramsDirCS)) #== pforgetData (pdata (pconstant @PCurrencySymbol paramsDirCS)))

mkParamsLookupCase :: String -> Int -> Int -> BenchCase
mkParamsLookupCase name total pos =
    mkCase
        name
        pparamsLookupDatum
        [ PlutusTx.toData (fromIntegral pos :: Integer)
        , PlutusTx.toData (refInputsFixture total pos)
        ]

-- ---- (c) LocalFullScan vs LocalIndexed ----

localOwnCS :: CurrencySymbol
localOwnCS = currencySymbolAt 9

localUnrelatedCS :: CurrencySymbol
localUnrelatedCS = currencySymbolAt 0

localTn :: TokenName
localTn = tokenNameAt 0

pownCSAbsent ::
    Term s PByteString ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))) ->
    Term s PBool
pownCSAbsent ownCSBytes pairs =
    ( pfixHoisted #$ plam $ \self remaining ->
        pelimList
            ( \pair rest ->
                plet (pasByteStr # pforgetData (pfstBuiltin # pair)) $ \csBytes ->
                    pif
                        (csBytes #== ownCSBytes)
                        (pconstant False)
                        (pif (ownCSBytes #< csBytes) (pconstant True) (self # rest))
            )
            (pconstant True)
            remaining
    )
        # pairs

mkFullScanTerm :: CurrencySymbol -> Term s (PScriptContext :--> PBool)
mkFullScanTerm ownCS = plam $ \ctx ->
    pmatch (pscriptContextTxInfo ctx) $ \txInfo ->
        plet (pasByteStr # pforgetData (pdata (pconstant @PCurrencySymbol ownCS))) $ \ownCSBytes ->
            plet (pforgetData (pdata (pconstant @PCredential progLogicBaseCred))) $ \baseCredData ->
                let go = pfixHoisted #$ plam $ \self outs ->
                        pelimList
                            ( \txOut rest ->
                                plet (psndBuiltin # (pasConstr # pforgetData txOut)) $ \fields ->
                                    let addrData = phead # fields
                                        credData = phead # (psndBuiltin # (pasConstr # addrData))
                                        valuePairs = pto (pto (pfromData (punsafeCoerce @(PAsData (PValue 'Sorted 'Positive)) (phead # (ptail # fields)))))
                                     in ((credData #== baseCredData) #|| pownCSAbsent ownCSBytes valuePairs)
                                            #&& (self # rest)
                            )
                            (pconstant True)
                            outs
                 in go # pfromData (ptxInfo'outputs txInfo)

mkLocalIndexedTerm :: CurrencySymbol -> TokenName -> Integer -> Term s (PData :--> PScriptContext :--> PBool)
mkLocalIndexedTerm ownCS tn mintedTotal = plam $ \destIdxsData ctx ->
    pmatch (pscriptContextTxInfo ctx) $ \txInfo ->
        plet (pasByteStr # pforgetData (pdata (pconstant @PCurrencySymbol ownCS))) $ \ownCSBytes ->
            plet (pforgetData (pdata (pconstant @PCredential progLogicBaseCred))) $ \baseCredData ->
                let inputs = pfromData $ ptxInfo'inputs txInfo
                    outputs = pfromData $ ptxInfo'outputs txInfo
                    guardOk =
                        ( pfixHoisted #$ plam $ \self ins ->
                            pelimList
                                ( \txIn rest ->
                                    plet (psndBuiltin # (pasConstr # (phead # (ptail # (psndBuiltin # (pasConstr # pforgetData txIn)))))) $ \outFields ->
                                        let valuePairs = pto (pto (pfromData (punsafeCoerce @(PAsData (PValue 'Sorted 'Positive)) (phead # (ptail # outFields)))))
                                         in pownCSAbsent ownCSBytes valuePairs #&& (self # rest)
                                )
                                (pconstant True)
                                ins
                        )
                            # inputs
                    walk = pfixHoisted #$ plam $ \self idxs pos remainingOuts acc ->
                        pelimList
                            ( \idxD idxsRest ->
                                plet (pasInt # idxD) $ \idx ->
                                    plet (idx - pos) $ \skip ->
                                        pif (skip #< 0) perror $
                                            plet (pdropFast # skip # remainingOuts) $ \remAtIdx ->
                                                plet (psndBuiltin # (pasConstr # pforgetData (phead # remAtIdx))) $ \fields ->
                                                    let addrData = phead # fields
                                                        credData = phead # (psndBuiltin # (pasConstr # addrData))
                                                        outValue = pfromData (punsafeCoerce @(PAsData (PValue 'Sorted 'Positive)) (phead # (ptail # fields)))
                                                        qty = passetQtyInValueBench # outValue # pconstant ownCS # pconstant tn
                                                     in pif
                                                            (credData #== baseCredData)
                                                            (self # idxsRest # (idx + 1) # (ptail # remAtIdx) # (acc + qty))
                                                            perror
                            )
                            (acc #>= pconstant mintedTotal)
                            idxs
                 in guardOk #&& (walk # (pasList # destIdxsData) # 0 # outputs # 0)

localCustodyCtx :: Int -> Int -> Bool -> Int -> Bool -> ScriptContext
localCustodyCtx totalOuts destCount destsLate inputCount inputsCarryTokens =
    buildScriptContext (inputsPart <> outputsPart)
  where
    inputsPart =
        foldMap
            ( \i ->
                withInput
                    ( withAddress (pubKeyAddress (pubKeyHashAt (i + 300)))
                        <> withValue (mkAdaValue 5_000_000 <> (if inputsCarryTokens then assetClassValue (assetClass localUnrelatedCS localTn) 3 else mempty))
                    )
            )
            [0 .. inputCount - 1]
    destOut =
        withOutput
            ( withTxOutAddress (Address progLogicBaseCred Nothing)
                <> withTxOutValue (mkAdaValue 2_000_000 <> assetClassValue (assetClass localOwnCS localTn) 100)
            )
    otherOut i =
        withOutput
            ( withTxOutAddress (pubKeyAddress (pubKeyHashAt (i + 400)))
                <> withTxOutValue (mkAdaValue 2_000_000 <> assetClassValue (assetClass localUnrelatedCS localTn) 1)
            )
    outputsPart
        | destsLate = foldMap otherOut [0 .. totalOuts - destCount - 1] <> foldMap (const destOut) [1 .. destCount]
        | otherwise = foldMap (const destOut) [1 .. destCount] <> foldMap otherOut [0 .. totalOuts - destCount - 1]

mkFullScanCase :: String -> ScriptContext -> BenchCase
mkFullScanCase name ctx =
    mkCase name (mkFullScanTerm localOwnCS) [PlutusTx.toData ctx]

mkLocalIndexedCase :: String -> ScriptContext -> [Integer] -> Integer -> BenchCase
mkLocalIndexedCase name ctx destIdxs mintedTotal =
    mkCase
        name
        (mkLocalIndexedTerm localOwnCS localTn mintedTotal)
        [ PlutusTx.toData destIdxs
        , PlutusTx.toData ctx
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
