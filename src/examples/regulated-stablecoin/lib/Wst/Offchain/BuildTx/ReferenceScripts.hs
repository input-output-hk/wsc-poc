module Wst.Offchain.BuildTx.ReferenceScripts (
    spendPlutusRefBaseWithRedeemerFn,
    spendPlutusRefWithInlineDatumWithRedeemerFn,
    spendPlutusRefWithInlineDatum,
) where

import Cardano.Api qualified as C
import Control.Lens (over)
import Convex.BuildTx (MonadBuildTx, TxBuilder (..), addTxBuilder, buildRefScriptWitness, setScriptsValid)
import Convex.CardanoApi.Lenses qualified as L
import PlutusLedgerApi.V1 qualified as Plutus

{- | Like `Convex.BuildTx.spendPlutusRefBaseWithInRefWithRedeemerFn`, but does not
add the reference input automatically. Callers are responsible for adding the
reference input exactly where they want it in the transaction builder.
-}
spendPlutusRefBaseWithRedeemerFn ::
    forall redeemer era lang m.
    ( MonadBuildTx era m
    , Plutus.ToData redeemer
    , C.IsAlonzoBasedEra era
    , C.HasScriptLanguageInEra lang era
    , C.IsPlutusScriptLanguage lang
    ) =>
    C.TxIn ->
    C.TxIn ->
    C.PlutusScriptVersion lang ->
    C.ScriptDatum C.WitCtxTxIn ->
    (C.TxBodyContent C.BuildTx era -> redeemer) ->
    m ()
spendPlutusRefBaseWithRedeemerFn txIn refTxIn scrVer dat redFn =
    let wit txBody =
            C.BuildTxWith $
                C.ScriptWitness C.ScriptWitnessForSpending $
                    buildRefScriptWitness refTxIn scrVer dat (redFn txBody)
     in setScriptsValid >> addTxBuilder (TxBuilder $ \body -> over L.txIns ((txIn, wit body) :))

spendPlutusRefWithInlineDatumWithRedeemerFn ::
    forall redeemer era lang m.
    ( MonadBuildTx era m
    , Plutus.ToData redeemer
    , C.IsBabbageBasedEra era
    , C.HasScriptLanguageInEra lang era
    , C.IsPlutusScriptLanguage lang
    ) =>
    C.TxIn ->
    C.TxIn ->
    C.PlutusScriptVersion lang ->
    (C.TxBodyContent C.BuildTx era -> redeemer) ->
    m ()
spendPlutusRefWithInlineDatumWithRedeemerFn txIn refTxIn scrVer =
    spendPlutusRefBaseWithRedeemerFn txIn refTxIn scrVer C.InlineScriptDatum

spendPlutusRefWithInlineDatum ::
    forall redeemer era lang m.
    ( MonadBuildTx era m
    , Plutus.ToData redeemer
    , C.IsBabbageBasedEra era
    , C.HasScriptLanguageInEra lang era
    , C.IsPlutusScriptLanguage lang
    ) =>
    C.TxIn ->
    C.TxIn ->
    C.PlutusScriptVersion lang ->
    redeemer ->
    m ()
spendPlutusRefWithInlineDatum txIn refTxIn scrVer red =
    spendPlutusRefWithInlineDatumWithRedeemerFn txIn refTxIn scrVer (const red)
