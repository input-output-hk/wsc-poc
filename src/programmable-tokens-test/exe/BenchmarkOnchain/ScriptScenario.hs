module BenchmarkOnchain.ScriptScenario (
    ScriptScenarioBackend (..),
    ScriptScenarioEnv (..),
    scenarioEvalSpecsFromCtx,
) where

import BenchmarkOnchain.ScriptHelpers (mintingPurposeCtx, rewardingPurposeCtx, scriptCredentialHash, spendingPurposeCtx)
import BenchmarkOnchain.ScriptRunner (EvalSpec)
import Data.Maybe (mapMaybe)
import PlutusLedgerApi.V3
import PlutusTx.AssocMap qualified as Map

data ScriptScenarioEnv = ScriptScenarioEnv
    { sseDirectoryPolicyCS :: CurrencySymbol
    , sseDirectorySpendHash :: ScriptHash
    , sseExternalAlwaysSucceedsHash :: ScriptHash
    , sseExternalAlwaysSucceedsHash2 :: ScriptHash
    , sseGlobalCred :: Credential
    , sseInitRef :: TxOutRef
    , sseIssuanceInitRef :: TxOutRef
    , sseIssuancePolicyCS :: CurrencySymbol
    , sseIssuerCred :: Credential
    , sseMintingLogicHash :: ScriptHash
    , sseMintingPolicyCS :: CurrencySymbol
    , sseProgLogicBaseCred :: Credential
    , sseProgLogicBaseHash :: ScriptHash
    , sseProtocolParamsCS :: CurrencySymbol
    , sseProtocolParamsInitRef :: TxOutRef
    , sseSeizeCred :: Credential
    , sseTransferLogicHash :: ScriptHash
    , sseTxD29BaseScriptCred :: Credential
    , sseTxD29GlobalStakeCred :: Credential
    , sseTxD29ProtocolParamsCS :: CurrencySymbol
    , sseTxD29TransferLogicStakeCred :: Credential
    }

data ScriptScenarioBackend = ScriptScenarioBackend
    { ssbAlwaysSucceedsRewardSpec :: String -> Credential -> ScriptContext -> EvalSpec
    , ssbAlwaysSucceedsSpendSpec :: String -> TxOutRef -> ScriptContext -> EvalSpec
    , ssbBaseSpendSpec :: ScriptScenarioEnv -> Credential -> TxOutRef -> ScriptContext -> EvalSpec
    , ssbDirectoryMintSpec :: ScriptScenarioEnv -> CurrencySymbol -> ScriptContext -> EvalSpec
    , ssbDirectorySpendSpec :: ScriptScenarioEnv -> CurrencySymbol -> TxOutRef -> ScriptContext -> EvalSpec
    , ssbGlobalRewardSpec :: ScriptScenarioEnv -> CurrencySymbol -> Credential -> ScriptContext -> EvalSpec
    , ssbSeizeRewardSpec :: ScriptScenarioEnv -> CurrencySymbol -> Credential -> ScriptContext -> EvalSpec
    , ssbIssuanceMintSpec :: ScriptScenarioEnv -> CurrencySymbol -> ScriptContext -> EvalSpec
    , ssbProgrammableMintSpec :: ScriptScenarioEnv -> CurrencySymbol -> ScriptContext -> EvalSpec
    , ssbProtocolParamsMintSpec :: ScriptScenarioEnv -> CurrencySymbol -> ScriptContext -> EvalSpec
    }

scenarioEvalSpecsFromCtx :: ScriptScenarioBackend -> ScriptScenarioEnv -> ScriptContext -> [EvalSpec]
scenarioEvalSpecsFromCtx backend env ctx@(ScriptContext txInfo _ _) =
    mapMaybe inputEvalSpec (txInfoInputs txInfo)
        <> mapMaybe withdrawalEvalSpec (Map.keys (txInfoWdrl txInfo))
        <> mapMaybe mintEvalSpec (Map.keys (mintValueToMap (txInfoMint txInfo)))
  where
    inputEvalSpec :: TxInInfo -> Maybe EvalSpec
    inputEvalSpec input =
        case txOutAddress (txInInfoResolved input) of
            Address (ScriptCredential sh) _
                | sh == sseProgLogicBaseHash env ->
                    let outRef = txInInfoOutRef input
                     in Just (ssbBaseSpendSpec backend env (sseGlobalCred env) outRef (spendingPurposeCtx ctx input))
                | sh == scriptCredentialHash (sseTxD29BaseScriptCred env) ->
                    let outRef = txInInfoOutRef input
                     in Just (ssbBaseSpendSpec backend env (sseTxD29GlobalStakeCred env) outRef (spendingPurposeCtx ctx input))
                | sh == sseDirectorySpendHash env ->
                    let outRef = txInInfoOutRef input
                     in Just (ssbDirectorySpendSpec backend env (sseProtocolParamsCS env) outRef (spendingPurposeCtx ctx input))
                | sh == sseExternalAlwaysSucceedsHash env ->
                    let outRef = txInInfoOutRef input
                     in Just (ssbAlwaysSucceedsSpendSpec backend "alwaysSucceeds" outRef (spendingPurposeCtx ctx input))
                | sh == sseExternalAlwaysSucceedsHash2 env ->
                    let outRef = txInInfoOutRef input
                     in Just (ssbAlwaysSucceedsSpendSpec backend "alwaysSucceeds2" outRef (spendingPurposeCtx ctx input))
            _ -> Nothing

    withdrawalEvalSpec :: Credential -> Maybe EvalSpec
    withdrawalEvalSpec cred
        | cred == sseGlobalCred env =
            Just (ssbGlobalRewardSpec backend env (sseProtocolParamsCS env) cred (rewardingPurposeCtx ctx cred))
        | cred == sseTxD29GlobalStakeCred env =
            Just (ssbGlobalRewardSpec backend env (sseTxD29ProtocolParamsCS env) cred (rewardingPurposeCtx ctx cred))
        | cred == sseSeizeCred env =
            Just (ssbSeizeRewardSpec backend env (sseProtocolParamsCS env) cred (rewardingPurposeCtx ctx cred))
        | cred == sseTxD29TransferLogicStakeCred env =
            Just (ssbAlwaysSucceedsRewardSpec backend "transferLogicScript" cred (rewardingPurposeCtx ctx cred))
        | otherwise =
            (`fmap` auxiliaryAlwaysSucceedsScriptName cred) $ \scriptName ->
                ssbAlwaysSucceedsRewardSpec backend scriptName cred (rewardingPurposeCtx ctx cred)

    mintEvalSpec :: CurrencySymbol -> Maybe EvalSpec
    mintEvalSpec cs
        | cs == sseDirectoryPolicyCS env =
            Just (ssbDirectoryMintSpec backend env cs (mintingPurposeCtx ctx cs))
        | cs == sseMintingPolicyCS env =
            Just (ssbProgrammableMintSpec backend env cs (mintingPurposeCtx ctx cs))
        | cs == sseProtocolParamsCS env =
            Just (ssbProtocolParamsMintSpec backend env cs (mintingPurposeCtx ctx cs))
        | cs == sseIssuancePolicyCS env =
            Just (ssbIssuanceMintSpec backend env cs (mintingPurposeCtx ctx cs))
        | otherwise = Nothing

    auxiliaryAlwaysSucceedsScriptName :: Credential -> Maybe String
    auxiliaryAlwaysSucceedsScriptName cred
        | cred == ScriptCredential (sseTransferLogicHash env) = Just "transferLogicScript"
        | cred == sseIssuerCred env = Just "issuerScript"
        | cred == ScriptCredential (sseMintingLogicHash env) = Just "mintingLogicScript"
        | cred == ScriptCredential (sseExternalAlwaysSucceedsHash env) = Just "alwaysSucceeds"
        | cred == ScriptCredential (sseExternalAlwaysSucceedsHash2 env) = Just "alwaysSucceeds2"
        | otherwise = Nothing
