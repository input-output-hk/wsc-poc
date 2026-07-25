{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ProgrammableTokens.Test.ScriptContext.Builder (
    UnitTestArgs (..),
    InputBuilder (..),
    TxOutBuilder (..),
    ScriptContextBuilder (..),
    ScriptContextBuilderState (..),
    buildScriptContext,
    withRedeemer,
    withFee,
    withSigner,
    withSigners,
    withMint,
    withMintingScript,
    withSpendingScript,
    withRewardingScript,
    withRewardingScriptWithBuilder,
    withRewardingScriptWitness,
    withOutput,
    withInput,
    withScriptInput,
    withReferenceInput,
    withValue,
    withValidRange,
    withOutRef,
    withInlineDatum,
    withReferenceScript,
    withAddress,
    withWithdrawal,
    mkInput,
    addInput,
    addMint,
    mkMintingScriptWithPurpose,
    addChangeOutput,
    signAndAddChangeOutput,
    negateValue,
    mkAdaValue,
    mkTxOut,
    withTxOutReferenceScript,
    withTxOutInlineDatum,
    withTxOutValue,
    withTxOutAddress,
    addOutput,
    addReferenceInput,
    buildBalancedScriptContext,
    buildLedgerShapedScriptContext,
    balanceWithChangeOutput,
    builderPlaceHolderTxOutRef,
    defaultBalancedTxFee,
    minAdaPerTxOut,
    ensureMinAda,
    compareCredentialLedger,
    canonicaliseWdrl,
) where

import Data.Function (on)
import Data.List (insert, insertBy, sortBy, sortOn)
import Data.Ord (comparing)
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Address
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.MintValue
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Eq qualified

instance PlutusTx.Eq.Eq ScriptPurpose where
    (==) a b = PlutusTx.toBuiltinData a == PlutusTx.toBuiltinData b

data UnitTestArgs = UnitTestArgs
    { utaScriptContext :: ScriptContext
    , utaParameters :: [BuiltinData]
    }
    deriving stock (Generic)

mkAdaValue :: Int -> Value
mkAdaValue i = assetClassValue (assetClass adaSymbol adaToken) (fromIntegral i)

-- | Canonicalise a 'Value' so its currency-symbol map and each token-name map are
-- ordered exactly as the ledger presents them on-chain. The builder otherwise
-- assembles values via list-order @foldMap@, which is not guaranteed to be sorted,
-- so applying this to every value that enters a context ensures the benchmarks/tests
-- measure LEDGER-VALID transactions (the on-chain code relies on canonical value
-- maps: strip-ADA-first, first-non-Ada state token, sorted merges).
--
-- The ledger orders MultiAsset maps by PolicyID/AssetName using their bytewise
-- (lexicographic) Ord — which is exactly the CurrencySymbol/TokenName Ord here, so a
-- plain ascending @sortOn fst@ reproduces the on-chain canonical order.
normalizeValue :: Value -> Value
normalizeValue (Value m) =
    Value $
        Map.unsafeFromList $
            sortOn fst
                [ (cs, Map.unsafeFromList $ sortOn fst (Map.toList inner))
                | (cs, inner) <- Map.toList m
                ]

-- | Fee charged by 'buildLedgerShapedScriptContext'.
--
-- [LEDGER-RULE] A Cardano transaction cannot have a zero fee: the minimum fee is
-- @minFeeA * size + minFeeB@, strictly positive for any non-empty transaction.
-- A @txInfoFee = 0@ context is one no ledger would ever construct, so a
-- validator benchmarked on it is measured against a transaction that could not
-- exist. 0.5 ada is inside the range real script transactions pay.
defaultBalancedTxFee :: Integer
defaultBalancedTxFee = 500_000

-- | Lovelace attached to an output that would otherwise carry no ada entry.
--
-- [LEDGER-RULE] Cardano's min-UTxO rule (@coinsPerUTxOByte@) forbids a UTxO with
-- zero lovelace, so a token-only 'TxOut' is not a shape any ledger emits. The
-- real minimum for a small multi-asset output is ~1.2-1.5 ada; 2 ada is a safe
-- over-approximation and matches the ada the surrounding fixtures already use.
minAdaPerTxOut :: Integer
minAdaPerTxOut = 2_000_000

-- | Attach 'minAdaPerTxOut' to a non-empty value carrying no lovelace entry at
-- all. An entirely empty value is left alone: that is the "no change needed"
-- marker, not a real output.
ensureMinAda :: Value -> Value
ensureMinAda v@(Value m)
    | Map.null m = v
    | otherwise = case Map.lookup adaSymbol m of
        Just _ -> v
        Nothing -> mkAdaValue (fromIntegral minAdaPerTxOut) <> v

-- | Order two credentials the way @cardano-ledger@ does.
--
-- [LEDGER-RULE] @cardano-ledger@'s derived @Ord Credential@ puts
-- @ScriptHashObj@ BEFORE @KeyHashObj@ (the constructor order in
-- @Cardano.Ledger.Credential@) — the OPPOSITE of @PlutusLedgerApi@'s
-- @Credential@, whose @PubKeyCredential@ comes first. The ledger builds
-- @txInfoWdrl@ from a @Map RewardAccount Coin@ in that order, so a withdrawal
-- map must be ascending under THIS comparison, not the Plutus derived one.
compareCredentialLedger :: Credential -> Credential -> Ordering
compareCredentialLedger a b =
    comparing constructorTag a b <> comparing credentialHashBytes a b
  where
    -- ScriptHashObj < KeyHashObj
    constructorTag :: Credential -> Int
    constructorTag (ScriptCredential _) = 0
    constructorTag (PubKeyCredential _) = 1

    credentialHashBytes (ScriptCredential (ScriptHash h)) = h
    credentialHashBytes (PubKeyCredential (PubKeyHash h)) = h

-- | Put a withdrawal map into the ledger's own order (see
-- 'compareCredentialLedger'). A permutation only: no credential, amount or
-- balance is touched.
canonicaliseWdrl :: Map.Map Credential Lovelace -> Map.Map Credential Lovelace
canonicaliseWdrl =
    Map.unsafeFromList . sortBy (compareCredentialLedger `on` fst) . Map.toList

addMint :: ScriptContext -> Value -> BuiltinData -> ScriptContext
addMint ctx newMint redeemer =
    let existingMint = Value $ mintValueToMap (txInfoMint (scriptContextTxInfo ctx))
        mergedMint = UnsafeMintValue $ getValue $ normalizeValue (existingMint <> newMint)
        mintCS = head $ Map.keys $ getValue newMint
        existingRedeemers = txInfoRedeemers (scriptContextTxInfo ctx)
        updatedRedeemers = Map.insert (Minting mintCS) (Redeemer redeemer) existingRedeemers
     in ctx{scriptContextTxInfo = (scriptContextTxInfo ctx){txInfoMint = mergedMint, txInfoRedeemers = updatedRedeemers}}

addInput :: TxInInfo -> ScriptContext -> ScriptContext
addInput newInput ctx =
    let existingInputs = txInfoInputs (scriptContextTxInfo ctx)
        sortedInputs = insertBy (comparing txInInfoOutRef) newInput existingInputs
     in ctx{scriptContextTxInfo = (scriptContextTxInfo ctx){txInfoInputs = sortedInputs}}

addReferenceInput :: TxInInfo -> ScriptContext -> ScriptContext
addReferenceInput newInput ctx =
    let existingInputs = txInfoReferenceInputs (scriptContextTxInfo ctx)
        sortedInputs = insertBy (comparing txInInfoOutRef) newInput existingInputs
     in ctx{scriptContextTxInfo = (scriptContextTxInfo ctx){txInfoReferenceInputs = sortedInputs}}

addOutput :: TxOut -> ScriptContext -> ScriptContext
addOutput newOutput ctx =
    let existingOutputs = txInfoOutputs (scriptContextTxInfo ctx)
     in ctx{scriptContextTxInfo = (scriptContextTxInfo ctx){txInfoOutputs = newOutput : existingOutputs}}

newtype InputBuilder = InputBuilder {runInputBuilder :: InputBuilderState -> InputBuilderState}

data InputBuilderState = InputBuilderState
    { ibOutRef :: TxOutRef
    , ibAddress :: Address
    , ibValue :: Value
    , ibDatum :: OutputDatum
    , ibReferenceScript :: Maybe ScriptHash
    }

instance Semigroup InputBuilder where
    InputBuilder a <> InputBuilder b = InputBuilder (a . b)

instance Monoid InputBuilder where
    mempty = InputBuilder id

builderPlaceHolderTxOutRef :: TxOutRef
builderPlaceHolderTxOutRef = TxOutRef "deadbeef" 0

builderPlaceHolderAddress :: Address
builderPlaceHolderAddress = pubKeyHashAddress (PubKeyHash "deadbeef")

defaultInputBuilderState :: InputBuilderState
defaultInputBuilderState =
    InputBuilderState
        { ibOutRef = builderPlaceHolderTxOutRef
        , ibAddress = builderPlaceHolderAddress
        , ibValue = mempty
        , ibDatum = NoOutputDatum
        , ibReferenceScript = Nothing
        }

withOutRef :: TxOutRef -> InputBuilder
withOutRef outRef = InputBuilder $ \inputBuilder -> inputBuilder{ibOutRef = outRef}

withAddress :: Address -> InputBuilder
withAddress address = InputBuilder $ \inputBuilder -> inputBuilder{ibAddress = address}

withValue :: Value -> InputBuilder
withValue value = InputBuilder $ \inputBuilder -> inputBuilder{ibValue = value}

withInlineDatum :: BuiltinData -> InputBuilder
withInlineDatum datum = InputBuilder $ \inputBuilder -> inputBuilder{ibDatum = OutputDatum $ Datum datum}

withReferenceScript :: ScriptHash -> InputBuilder
withReferenceScript scriptHash = InputBuilder $ \inputBuilder -> inputBuilder{ibReferenceScript = Just scriptHash}

mkInput :: InputBuilder -> TxInInfo
mkInput (InputBuilder modify) =
    let builder = modify defaultInputBuilderState
     in TxInInfo
            { txInInfoOutRef = ibOutRef builder
            , txInInfoResolved =
                TxOut
                    { txOutAddress = ibAddress builder
                    , txOutValue = normalizeValue (ibValue builder)
                    , txOutDatum = ibDatum builder
                    , txOutReferenceScript = ibReferenceScript builder
                    }
            }

newtype TxOutBuilder = TxOutBuilder {runTxOutBuilder :: TxOutBuilderState -> TxOutBuilderState}

data TxOutBuilderState = TxOutBuilderState
    { tobAddress :: Address
    , tobValue :: Value
    , tobDatum :: OutputDatum
    , tobReferenceScript :: Maybe ScriptHash
    }

defaultTxOutBuilderState :: TxOutBuilderState
defaultTxOutBuilderState =
    TxOutBuilderState
        { tobAddress = builderPlaceHolderAddress
        , tobValue = mempty
        , tobDatum = NoOutputDatum
        , tobReferenceScript = Nothing
        }

instance Semigroup TxOutBuilder where
    (TxOutBuilder f) <> (TxOutBuilder g) = TxOutBuilder (f . g)

instance Monoid TxOutBuilder where
    mempty = TxOutBuilder id

withTxOutAddress :: Address -> TxOutBuilder
withTxOutAddress addr = TxOutBuilder $ \tob -> tob{tobAddress = addr}

withTxOutValue :: Value -> TxOutBuilder
withTxOutValue val = TxOutBuilder $ \tob -> tob{tobValue = tobValue tob <> val}

withTxOutInlineDatum :: BuiltinData -> TxOutBuilder
withTxOutInlineDatum datum = TxOutBuilder $ \tob -> tob{tobDatum = OutputDatum $ Datum datum}

withTxOutReferenceScript :: ScriptHash -> TxOutBuilder
withTxOutReferenceScript scriptHash = TxOutBuilder $ \tob -> tob{tobReferenceScript = Just scriptHash}

mkTxOut :: TxOutBuilder -> TxOut
mkTxOut (TxOutBuilder modify) =
    let finalState = modify defaultTxOutBuilderState
     in TxOut
            { txOutAddress = tobAddress finalState
            , txOutValue = normalizeValue (tobValue finalState)
            , txOutDatum = tobDatum finalState
            , txOutReferenceScript = tobReferenceScript finalState
            }

mkMintingScriptWithPurpose :: Value -> BuiltinData -> ScriptContext
mkMintingScriptWithPurpose mintValue redeemer =
    ScriptContext
        mintingScriptTxInfo
        (Redeemer redeemer)
        (MintingScript mintCS)
  where
    mintCS :: CurrencySymbol
    mintCS = head $ Map.keys $ getValue mintValue

    mintingScriptTxInfo :: TxInfo
    mintingScriptTxInfo =
        TxInfo
            { txInfoInputs = mempty
            , txInfoReferenceInputs = mempty
            , txInfoOutputs = mempty
            , txInfoFee = 0
            , txInfoMint = UnsafeMintValue $ getValue (normalizeValue mintValue)
            , txInfoTxCerts = mempty
            , txInfoWdrl = Map.empty
            , txInfoValidRange = always
            , txInfoSignatories = mempty
            , txInfoRedeemers = Map.unsafeFromList [(Minting mintCS, Redeemer redeemer)]
            , txInfoData = Map.empty
            , txInfoId = TxId ""
            , txInfoVotes = Map.empty
            , txInfoProposalProcedures = mempty
            , txInfoCurrentTreasuryAmount = Nothing
            , txInfoTreasuryDonation = Nothing
            }

negateValue :: Value -> Value
negateValue (Value val) = Value $ Map.mapWithKey (\_ -> Map.mapWithKey (\_ x -> negate x)) val

-- | Drop zero-quantity asset entries (and any policy left empty) from a value.
-- A balanced change output computed as @mint + inputs - outputs@ can leave a
-- phantom @(policy, name, 0)@ entry because the Value semigroup does not prune
-- zeros. Real ledger values never carry zero entries, and such a phantom entry
-- makes an on-chain @is_empty@ / token-presence check see a token that is not
-- there — so normalise the change value to the canonical zero-free form.
pruneZeroValue :: Value -> Value
pruneZeroValue (Value m) =
    Value $
        Map.unsafeFromList
            [ (cs, inner')
            | (cs, inner) <- Map.toList m
            , let inner' = Map.unsafeFromList [(tn, q) | (tn, q) <- Map.toList inner, q /= 0]
            , not (Map.null inner')
            ]

addChangeOutput :: PubKeyHash -> ScriptContext -> ScriptContext
addChangeOutput signerPkh ctx =
    let totalInputValue = foldMap (txOutValue . txInInfoResolved) (txInfoInputs $ scriptContextTxInfo ctx)
        totalOutputValue = foldMap txOutValue (txInfoOutputs $ scriptContextTxInfo ctx)
        feeValue = mkAdaValue $ fromIntegral $ getLovelace $ txInfoFee $ scriptContextTxInfo ctx
        mintedValue = Value $ mintValueToMap (txInfoMint (scriptContextTxInfo ctx))
        changeValue = pruneZeroValue (mintedValue <> totalInputValue <> negateValue feeValue <> negateValue totalOutputValue)
        changeOutput = TxOut (pubKeyHashAddress signerPkh) changeValue NoOutputDatum Nothing
     in ctx{scriptContextTxInfo = (scriptContextTxInfo ctx){txInfoOutputs = changeOutput : txInfoOutputs (scriptContextTxInfo ctx)}}

balanceWithChangeOutput :: ScriptContext -> ScriptContext
balanceWithChangeOutput ctx =
    let resolvedInputs = map txInInfoResolved (txInfoInputs $ scriptContextTxInfo ctx)
        signerPkh = case filter (isPubKeyAddress . txOutAddress) resolvedInputs of
            (TxOut (Address (PubKeyCredential pkh) _) _ _ _ : _) -> pkh
            _ -> PubKeyHash "deadbeef"
        totalInputValue = foldMap (txOutValue . txInInfoResolved) (txInfoInputs $ scriptContextTxInfo ctx)
        totalOutputValue = foldMap txOutValue (txInfoOutputs $ scriptContextTxInfo ctx)
        feeValue = mkAdaValue $ fromIntegral $ getLovelace $ txInfoFee $ scriptContextTxInfo ctx
        mintedValue = Value $ mintValueToMap (txInfoMint (scriptContextTxInfo ctx))
        changeValue = pruneZeroValue (mintedValue <> totalInputValue <> negateValue feeValue <> negateValue totalOutputValue)
        changeOutput = TxOut (pubKeyHashAddress signerPkh) changeValue NoOutputDatum Nothing
     in ctx{scriptContextTxInfo = (scriptContextTxInfo ctx){txInfoOutputs = txInfoOutputs (scriptContextTxInfo ctx) <> [changeOutput]}}
  where
    isPubKeyAddress :: Address -> Bool
    isPubKeyAddress (Address (PubKeyCredential _) _) = True
    isPubKeyAddress _ = False

addSigner :: PubKeyHash -> ScriptContext -> ScriptContext
addSigner signerPkh ctx =
    ctx{scriptContextTxInfo = (scriptContextTxInfo ctx){txInfoSignatories = signerPkh : txInfoSignatories (scriptContextTxInfo ctx)}}

signAndAddChangeOutput :: PubKeyHash -> ScriptContext -> ScriptContext
signAndAddChangeOutput signerPkh ctx =
    let signedCtx = addChangeOutput signerPkh ctx
     in addSigner signerPkh signedCtx

newtype ScriptContextBuilder = ScriptContextBuilder {runBuilder :: ScriptContextBuilderState -> ScriptContextBuilderState}

data ScriptContextBuilderState = ScriptContextBuilderState
    { scbInputs :: [TxInInfo]
    , scbReferenceInputs :: [TxInInfo]
    , scbOutputs :: [TxOut]
    , scbFee :: Integer
    , scbMint :: Value
    , scbCerts :: [TxCert]
    , scbWdrl :: Map.Map Credential Lovelace
    , scbValidRange :: POSIXTimeRange
    , scbSignatories :: [PubKeyHash]
    , scbRedeemers :: Map.Map ScriptPurpose Redeemer
    , scbTxId :: TxId
    , scbScriptInfo :: ScriptInfo
    , scbRedeemer :: BuiltinData
    }

defaultScriptContextBuilderState :: ScriptContextBuilderState
defaultScriptContextBuilderState =
    ScriptContextBuilderState
        { scbInputs = []
        , scbReferenceInputs = []
        , scbOutputs = []
        , scbFee = 0
        , scbMint = mempty
        , scbCerts = []
        , scbWdrl = Map.empty
        , scbValidRange = always
        , scbRedeemers = Map.empty
        , scbSignatories = []
        , scbTxId = TxId "deadbeef"
        , scbScriptInfo = MintingScript (CurrencySymbol "deadbeef")
        , scbRedeemer = PlutusTx.toBuiltinData ()
        }

instance Semigroup ScriptContextBuilder where
    (ScriptContextBuilder f) <> (ScriptContextBuilder g) = ScriptContextBuilder (g . f)

instance Monoid ScriptContextBuilder where
    mempty = ScriptContextBuilder id

withFee :: Integer -> ScriptContextBuilder
withFee fee = ScriptContextBuilder $ \scb -> scb{scbFee = fee}

withValidRange :: POSIXTimeRange -> ScriptContextBuilder
withValidRange validRange = ScriptContextBuilder $ \scb -> scb{scbValidRange = validRange}

withSigner :: PubKeyHash -> ScriptContextBuilder
withSigner pkh = ScriptContextBuilder $ \scb ->
    scb{scbSignatories = insert pkh (scbSignatories scb)}

withSigners :: [PubKeyHash] -> ScriptContextBuilder
withSigners pks = ScriptContextBuilder $ \scb ->
    scb{scbSignatories = foldr (\p acc -> insert p acc) (scbSignatories scb) pks}

withMint :: Value -> BuiltinData -> ScriptContextBuilder
withMint value redeemer = ScriptContextBuilder $ \scb ->
    let mintCS = head $ Map.keys $ getValue value
        newRedeemers = Map.insert (Minting mintCS) (Redeemer redeemer) (scbRedeemers scb)
     in scb{scbMint = scbMint scb <> value, scbRedeemers = newRedeemers}

withOutput :: TxOutBuilder -> ScriptContextBuilder
withOutput modify = ScriptContextBuilder $ \scb ->
    scb{scbOutputs = mkTxOut modify : scbOutputs scb}

withInput :: InputBuilder -> ScriptContextBuilder
withInput modify = ScriptContextBuilder $ \scb ->
    let newInput = mkInput modify
        newInputAddress = txOutAddress $ txInInfoResolved newInput
     in if isPubKeyAddress newInputAddress
            then
                scb{scbInputs = insertBy (comparing txInInfoOutRef) newInput (scbInputs scb)}
            else
                error "withInput: Input address is not a public key address"
  where
    isPubKeyAddress :: Address -> Bool
    isPubKeyAddress (Address (PubKeyCredential _) _) = True
    isPubKeyAddress _ = False

withScriptInput :: BuiltinData -> InputBuilder -> ScriptContextBuilder
withScriptInput redeemer modify = ScriptContextBuilder $ \scb ->
    let newInput = mkInput modify
        inputOutRef = txInInfoOutRef newInput
        newRedeemers = Map.insert (Spending inputOutRef) (Redeemer redeemer) (scbRedeemers scb)
     in if isScriptAddress (txOutAddress $ txInInfoResolved newInput)
            then scb{scbInputs = insertBy (comparing txInInfoOutRef) newInput (scbInputs scb), scbRedeemers = newRedeemers}
            else error "withScriptInput: Input address is not a script address"
  where
    isScriptAddress :: Address -> Bool
    isScriptAddress (Address (ScriptCredential _) _) = True
    isScriptAddress _ = False

withReferenceInput :: InputBuilder -> ScriptContextBuilder
withReferenceInput modify = ScriptContextBuilder $ \scb ->
    let newRefInput = mkInput modify
     in scb{scbReferenceInputs = insertBy (comparing txInInfoOutRef) newRefInput (scbReferenceInputs scb)}

withMintingScript :: Value -> BuiltinData -> ScriptContextBuilder
withMintingScript mintValue redeemer =
    withMint mintValue redeemer
        <> ScriptContextBuilder
            ( \scb ->
                let mintCS = head $ Map.keys $ getValue mintValue
                 in scb{scbScriptInfo = MintingScript mintCS}
            )

withSpendingScript :: BuiltinData -> InputBuilder -> ScriptContextBuilder
withSpendingScript redeemer modify = ScriptContextBuilder $ \scb ->
    let scriptInput = mkInput modify
        outRef = txInInfoOutRef scriptInput
        newRedeemers = Map.insert (Spending outRef) (Redeemer redeemer) (scbRedeemers scb)
        datum =
            case txOutDatum $ txInInfoResolved scriptInput of
                NoOutputDatum -> Nothing
                OutputDatum (Datum dat) -> Just (Datum dat)
                _ -> Nothing
     in scb{scbScriptInfo = SpendingScript outRef datum, scbInputs = insertBy (comparing txInInfoOutRef) scriptInput (scbInputs scb), scbRedeemers = newRedeemers, scbRedeemer = redeemer}

withRewardingScript :: BuiltinData -> Credential -> Integer -> ScriptContextBuilder
withRewardingScript redeemer cred adaAmount =
    ScriptContextBuilder $ \scb ->
        let newWdrl = Map.insert cred (fromIntegral adaAmount) (scbWdrl scb)
            newRedeemers = Map.insert (Rewarding cred) (Redeemer redeemer) (scbRedeemers scb)
         in scb
                { scbWdrl = newWdrl
                , scbRedeemers = newRedeemers
                , scbRedeemer = redeemer
                , scbScriptInfo = RewardingScript cred
                }

withRewardingScriptWithBuilder :: (ScriptContextBuilderState -> BuiltinData) -> Credential -> Integer -> ScriptContextBuilder
withRewardingScriptWithBuilder mkRedeemer cred adaAmount =
    ScriptContextBuilder $ \scb ->
        let redeemer = mkRedeemer scb
            newWdrl = Map.insert cred (fromIntegral adaAmount) (scbWdrl scb)
            newRedeemers = Map.insert (Rewarding cred) (Redeemer redeemer) (scbRedeemers scb)
         in scb
                { scbWdrl = newWdrl
                , scbRedeemers = newRedeemers
                , scbRedeemer = redeemer
                , scbScriptInfo = RewardingScript cred
                }

withRewardingScriptWitness :: BuiltinData -> Credential -> Integer -> ScriptContextBuilder
withRewardingScriptWitness redeemer cred adaAmount =
    ScriptContextBuilder $ \scb ->
        let newWdrl = Map.insert cred (fromIntegral adaAmount) (scbWdrl scb)
            newRedeemers = Map.insert (Rewarding cred) (Redeemer redeemer) (scbRedeemers scb)
         in scb
                { scbWdrl = newWdrl
                , scbRedeemers = newRedeemers
                }

withWithdrawal :: Credential -> Integer -> ScriptContextBuilder
withWithdrawal cred adaAmount = ScriptContextBuilder $ \scb ->
    let newWdrl = Map.insert cred (fromIntegral adaAmount) (scbWdrl scb)
     in scb{scbWdrl = newWdrl}

withRedeemer :: BuiltinData -> ScriptContextBuilder
withRedeemer redeemer = ScriptContextBuilder $ \scb -> scb{scbRedeemer = redeemer}

buildScriptContext :: ScriptContextBuilder -> ScriptContext
buildScriptContext modify =
    let finalState = runBuilder modify defaultScriptContextBuilderState
        txInfo =
            TxInfo
                { txInfoInputs = reverse $ scbInputs finalState
                , txInfoReferenceInputs = reverse $ scbReferenceInputs finalState
                , txInfoOutputs = reverse $ scbOutputs finalState
                , txInfoMint = UnsafeMintValue $ getValue (normalizeValue (scbMint finalState))
                , txInfoRedeemers = scbRedeemers finalState
                , txInfoFee = fromIntegral (scbFee finalState)
                , txInfoSignatories = scbSignatories finalState
                , txInfoTxCerts = scbCerts finalState
                , txInfoWdrl = scbWdrl finalState
                , txInfoValidRange = scbValidRange finalState
                , txInfoData = Map.empty
                , txInfoId = scbTxId finalState
                , txInfoVotes = Map.empty
                , txInfoProposalProcedures = []
                , txInfoCurrentTreasuryAmount = Nothing
                , txInfoTreasuryDonation = Nothing
                }
     in ScriptContext txInfo (Redeemer $ scbRedeemer finalState) (scbScriptInfo finalState)

-- | Order redeemer-map keys the way @cardano-ledger@ does.
--
-- [LEDGER-RULE] @txInfoRedeemers@ comes from a
-- @Map (ConwayPlutusPurpose AsIx) …@ whose derived @Ord@ is
-- @ConwaySpending < ConwayMinting < ConwayCertifying < ConwayRewarding < …@, and
-- @transTxRedeemers@ converts it with @Map.toList@ without re-sorting. Note this
-- puts @Spending@ BEFORE @Minting@ — the opposite of the Plutus @ScriptPurpose@
-- constructor order. Do not "fix" that: it is what the chain emits, and every
-- programmable-token mint carries both a spending and a minting redeemer.
--
-- Within one purpose kind the ledger key is @AsIx@ — the item's index in the
-- corresponding canonically-sorted ledger list — so equal kinds are broken by
-- the argument in ITS ledger order: inputs by 'TxOutRef' (the order
-- @txInfoInputs@ is already built in), mint policies by 'CurrencySymbol', and
-- reward accounts by 'compareCredentialLedger'.
comparePurposeLedger :: ScriptPurpose -> ScriptPurpose -> Ordering
comparePurposeLedger a b = comparing toInt a b <> sameKind a b
  where
    toInt :: ScriptPurpose -> Int
    toInt (Spending _) = 0
    toInt (Minting _) = 1
    toInt (Certifying _ _) = 2
    toInt (Rewarding _) = 3
    toInt _ = 10

    sameKind :: ScriptPurpose -> ScriptPurpose -> Ordering
    sameKind (Spending x) (Spending y) = compare x y
    sameKind (Minting x) (Minting y) = compare x y
    sameKind (Certifying i _) (Certifying j _) = compare i j
    sameKind (Rewarding x) (Rewarding y) = compareCredentialLedger x y
    sameKind _ _ = EQ

buildBalancedScriptContext :: ScriptContextBuilder -> ScriptContext
buildBalancedScriptContext modify =
    let finalState = runBuilder modify defaultScriptContextBuilderState
        txInfo =
            TxInfo
                { txInfoInputs = scbInputs finalState
                , txInfoReferenceInputs = scbReferenceInputs finalState
                , txInfoOutputs = scbOutputs finalState
                , txInfoMint = UnsafeMintValue $ getValue (normalizeValue (scbMint finalState))
                , txInfoRedeemers = Map.unsafeFromList $ sortBy (comparePurposeLedger `on` fst) $ Map.toList $ scbRedeemers finalState
                , txInfoFee = fromIntegral (scbFee finalState)
                , txInfoSignatories = scbSignatories finalState
                , txInfoTxCerts = scbCerts finalState
                , txInfoWdrl = scbWdrl finalState
                , txInfoValidRange = scbValidRange finalState
                , txInfoData = Map.empty
                , txInfoId = scbTxId finalState
                , txInfoVotes = Map.empty
                , txInfoProposalProcedures = []
                , txInfoCurrentTreasuryAmount = Nothing
                , txInfoTreasuryDonation = Nothing
                }
     in balanceWithChangeOutput $ ScriptContext txInfo (Redeemer $ scbRedeemer finalState) (scbScriptInfo finalState)

-- | Build a context that satisfies the ledger invariants a real node would
-- enforce, on top of everything 'buildBalancedScriptContext' already gives
-- (canonical ada-first sorted values, `TxOutRef`-sorted inputs, redeemer map in
-- `cardano-ledger`'s `ConwayPlutusPurpose AsIx` order, value-balancing change).
--
-- This exists as a SEPARATE entry point from 'buildBalancedScriptContext'
-- deliberately. The three fixes below change the emitted context (a positive
-- fee moves lovelace, canonical withdrawal order moves withdrawal INDEXES, and
-- min-UTxO ada adds a value entry), so every redeemer that witnesses a
-- withdrawal index has to be written against them. The unit-test suite builds
-- precise hand-balanced contexts against the legacy behaviour and keeps using
-- 'buildBalancedScriptContext'; the BENCHMARK catalogue and the golden
-- ScriptContexts extracted from it use this one, because those are the artifacts
-- whose fidelity to a real transaction is the whole point.
--
-- The three ledger invariants (each independently enforced by Cardano, each
-- previously violated — see `WSC/LR-CTX-AUDIT.md` in CardanoLedgerApiBlaster,
-- which found all 13 golden contexts failing a ledger-context predicate):
--
-- 1. __positive fee__ ('defaultBalancedTxFee'), taken out of the change output so
--    value conservation still holds;
-- 2. __withdrawal map in the ledger's own `Credential` order__
--    ('canonicaliseWdrl'), which also fixes the `Rewarding` entries of
--    `txInfoRedeemers` via 'comparePurposeLedger';
-- 3. __min-UTxO ada on every output__ ('ensureMinAda'), because a token-only
--    output cannot exist on chain.
--
-- A sub-min-UTxO ada leftover is folded into the fee and no change output is
-- emitted — exactly what a real coin selector does, since a change output below
-- min-UTxO would be rejected. A NEGATIVE leftover, or a token-carrying change
-- output with too little ada, is an 'error': the scenario is underfunded and the
-- context it would produce is one no ledger could construct.
buildLedgerShapedScriptContext :: ScriptContextBuilder -> ScriptContext
buildLedgerShapedScriptContext modify =
    let finalState = runBuilder modify defaultScriptContextBuilderState
        fee :: Integer
        fee = case scbFee finalState of
            0 -> defaultBalancedTxFee
            f -> f
        txInfo =
            TxInfo
                { txInfoInputs = scbInputs finalState
                , txInfoReferenceInputs = scbReferenceInputs finalState
                , -- (3) min-UTxO ada on every output
                  txInfoOutputs =
                    map
                        (\o -> o{txOutValue = normalizeValue (ensureMinAda (txOutValue o))})
                        (scbOutputs finalState)
                , txInfoMint = UnsafeMintValue $ getValue (normalizeValue (scbMint finalState))
                , txInfoRedeemers = Map.unsafeFromList $ sortBy (comparePurposeLedger `on` fst) $ Map.toList $ scbRedeemers finalState
                , -- (1) a real fee
                  txInfoFee = fromIntegral fee
                , txInfoSignatories = scbSignatories finalState
                , txInfoTxCerts = scbCerts finalState
                , -- (2) the ledger's withdrawal order
                  txInfoWdrl = canonicaliseWdrl (scbWdrl finalState)
                , txInfoValidRange = scbValidRange finalState
                , txInfoData = Map.empty
                , txInfoId = scbTxId finalState
                , txInfoVotes = Map.empty
                , txInfoProposalProcedures = []
                , txInfoCurrentTreasuryAmount = Nothing
                , txInfoTreasuryDonation = Nothing
                }
     in balanceLedgerShaped $ ScriptContext txInfo (Redeemer $ scbRedeemer finalState) (scbScriptInfo finalState)

-- | The change/fee reconciliation for 'buildLedgerShapedScriptContext'. See that
-- function's haddock for the four cases and why the last two are errors.
balanceLedgerShaped :: ScriptContext -> ScriptContext
balanceLedgerShaped ctx
    | Map.null changeMap = ctx -- exactly balanced: no change output at all
    | changeAda < 0 =
        error $
            "buildLedgerShapedScriptContext: scenario is underfunded — leftover is "
                <> show changeAda
                <> " lovelace. Add ada to an input (or lower the fee) so inputs cover outputs + fee."
                <> diagnostics
    | changeHasTokens && changeAda < minAdaPerTxOut =
        error $
            "buildLedgerShapedScriptContext: change output carries tokens but only "
                <> show changeAda
                <> " lovelace, below the min-UTxO floor of "
                <> show minAdaPerTxOut
                <> ". Add ada to an input so the change output is a valid UTxO."
                <> diagnostics
    -- Dust: a real coin selector pays it as extra fee rather than create a
    -- change output the ledger would reject.
    | changeAda < minAdaPerTxOut =
        ctx{scriptContextTxInfo = txInfo{txInfoFee = txInfoFee txInfo + fromIntegral changeAda}}
    | otherwise =
        ctx{scriptContextTxInfo = txInfo{txInfoOutputs = txInfoOutputs txInfo <> [changeOutput]}}
  where
    txInfo = scriptContextTxInfo ctx
    resolvedInputs = map txInInfoResolved (txInfoInputs txInfo)
    signerPkh = case filter (isPubKeyAddress . txOutAddress) resolvedInputs of
        (TxOut (Address (PubKeyCredential pkh) _) _ _ _ : _) -> pkh
        _ -> PubKeyHash "deadbeef"
    totalInputValue = foldMap txOutValue resolvedInputs
    totalOutputValue = foldMap txOutValue (txInfoOutputs txInfo)
    feeValue = mkAdaValue $ fromIntegral $ getLovelace $ txInfoFee txInfo
    mintedValue = Value $ mintValueToMap (txInfoMint txInfo)
    changeValue = pruneZeroValue (mintedValue <> totalInputValue <> negateValue feeValue <> negateValue totalOutputValue)
    Value changeMap = changeValue
    changeAda = valueOf changeValue adaSymbol adaToken
    changeHasTokens = any ((/= adaSymbol) . fst) (Map.toList changeMap)
    changeOutput = TxOut (pubKeyHashAddress signerPkh) changeValue NoOutputDatum Nothing

    isPubKeyAddress :: Address -> Bool
    isPubKeyAddress (Address (PubKeyCredential _) _) = True
    isPubKeyAddress _ = False

    diagnostics =
        "\n  inputs="
            <> show (length (txInfoInputs txInfo))
            <> " (lovelace "
            <> show (valueOf totalInputValue adaSymbol adaToken)
            <> "), outputs="
            <> show (length (txInfoOutputs txInfo))
            <> " (lovelace "
            <> show (valueOf totalOutputValue adaSymbol adaToken)
            <> "), fee="
            <> show (getLovelace (txInfoFee txInfo))
            <> "\n  first input outRef: "
            <> show (fmap txInInfoOutRef (take 1 (txInfoInputs txInfo)))
            <> "\n  first output address: "
            <> show (fmap txOutAddress (take 1 (txInfoOutputs txInfo)))
