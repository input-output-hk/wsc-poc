{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use second" #-}
module Wst.Offchain.BuildTx.ProgrammableLogic (
    seizeProgrammableToken,
)
where

import Cardano.Api qualified as C
import Control.Lens ((^.))
import Control.Lens qualified as L
import Control.Monad (forM_)
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx (
    MonadBuildTx,
    addOutput,
    addReference,
    addWithdrawalWithTxBody,
    buildScriptWitness,
    findIndexReference,
    findIndexSpending,
    spendPlutusInlineDatum,
 )
import Convex.CardanoApi.Lenses as L
import Convex.Class (MonadBlockchain (queryNetworkId))
import Convex.PlutusLedger.V1 (transPolicyId)
import Convex.Utils qualified as Utils
import Data.Foldable (find)
import Data.List (findIndex, partition)
import Data.Maybe (fromMaybe)
import GHC.Exts (IsList (..))
import PlutusLedgerApi.V3 (CurrencySymbol (..))
import ProgrammableTokens.OffChain.Env qualified as Env
import SmartTokens.Contracts.ProgrammableLogicBase (ProgrammableLogicGlobalRedeemer (..))
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..))
import SmartTokens.Types.ProtocolParams
import Wst.Offchain.Query (UTxODat (..))

{- Seize a programmable token from a user address to an issuer address. The
   outputs address will be that of the issuer retrieved from @issuerTxOut@.
   Throws if the payment credentials of the issuer output does not match the
   programmable logic payment credential.

   IMPORTANT: It is the caller's responsibility to
   ensure that the specific issuer logic stake script witness is included in the
   final transaction.

  * @UTxODat era ProgrammableLogicGlobalParams@: The reference input
    containing the global programmable-logic parameters. Used to anchor the
    global stake script during the seize.
  * @[UTxODat era a]@: The input UTxOs to be seized. Each entry is a
    programmable-token UTxO that will be spent and redirected.
  * @C.PolicyId@: The policy ID of the programmable token being seized. This is
    used to locate the corresponding directory node and to filter the value
    being removed from each seized UTxO.
  * @[UTxODat era DirectorySetNode]@: The directory entries that map programmable
    policies to the relevant transfer / issuer logic scripts. The function searches this list to find the node
    for the supplied policy ID so it can include the correct reference input.

-}
seizeProgrammableToken ::
    forall a env era m.
    ( MonadReader env m
    , Env.HasDirectoryEnv env
    , C.IsBabbageBasedEra era
    , MonadBlockchain era m
    , C.HasScriptLanguageInEra C.PlutusScriptV3 era
    , MonadBuildTx era m
    ) =>
    UTxODat era ProgrammableLogicGlobalParams ->
    [UTxODat era a] ->
    C.PolicyId ->
    [UTxODat era DirectorySetNode] ->
    m ()
seizeProgrammableToken UTxODat{uIn = paramsTxIn} seizingUTxOs seizingTokenPolicyId directoryList = Utils.inBabbage @era $ do
    nid <- queryNetworkId
    globalStakeScript <- asks (Env.dsProgrammableLogicGlobalScript . Env.directoryEnv)
    baseSpendingScript <- asks (Env.dsProgrammableLogicBaseScript . Env.directoryEnv)

    let globalStakeCred = C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 globalStakeScript
        programmableLogicBaseCredential = C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 baseSpendingScript

    -- Finds the directory node entry that references the programmable token symbol
    dirNodeRef <-
        maybe (error "Cannot seize non-programmable token. Entry does not exist in directoryList") (pure . uIn) $
            find (isNodeWithProgrammableSymbol (transPolicyId seizingTokenPolicyId)) directoryList

    -- destStakeCred <- either (error . ("Could not unTrans credential: " <>) . show) pure $ unTransStakeCredential $ transCredential seizeDestinationCred

    forM_ seizingUTxOs $ \UTxODat{uIn = seizingTxIn, uOut = seizingTxOut} -> do
        spendPlutusInlineDatum seizingTxIn baseSpendingScript ()
        let (seizedAddr, remainingValue, seizedDatum, referenceScript) = case seizingTxOut of
                (C.TxOut a v dat refScript) ->
                    let (_seized, other) =
                            partition
                                ( \case
                                    (C.AdaAssetId, _q) -> False
                                    (C.AssetId a_ _, _q) -> a_ == seizingTokenPolicyId
                                )
                                $ toList
                                $ C.txOutValueToValue v
                     in (a, fromList other, dat, refScript)
            remainingTxOutValue = C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toLedgerValue @era C.maryBasedEra remainingValue
            seizedOutput = C.TxOut seizedAddr remainingTxOutValue seizedDatum referenceScript
        addOutput (C.fromCtxUTxOTxOut seizedOutput)

    let
        -- Finds the index of the directory node reference in the transaction ref
        -- inputs
        directoryNodeReferenceIndex txBody =
            fromIntegral @Int @Integer $ findIndexReference dirNodeRef txBody

        -- Finds the index of the issuer input in the transaction body
        seizingInputIndex txBody =
            map (\UTxODat{uIn = seizingTxIn} -> fromIntegral @Int @Integer $ findIndexSpending seizingTxIn txBody) seizingUTxOs

        -- Finds the index of the first output to the programmable logic base credential
        firstSeizeContinuationOutputIndex txBody =
            fromIntegral @Int @Integer $
                fromMaybe (error "firstSeizeContinuationOutputIndex: No output to programmable logic base credential found") $
                    findIndex
                        ( maybe False ((== programmableLogicBaseCredential) . C.fromShelleyPaymentCredential)
                            . L.preview (L._TxOut . L._1 . L._AddressInEra . L._Address . L._2)
                        )
                        (txBody ^. L.txOuts)

        -- The seizing redeemer for the global script
        programmableLogicGlobalRedeemer txBody =
            SeizeAct
                { plgrDirectoryNodeIdx = directoryNodeReferenceIndex txBody
                , plgrInputIdxs = seizingInputIndex txBody
                , plgrOutputsStartIdx = firstSeizeContinuationOutputIndex txBody
                , plgrLengthInputIdxs = fromIntegral @Int @Integer $ length seizingUTxOs
                }

        programmableGlobalWitness txBody = buildScriptWitness globalStakeScript C.NoScriptDatumForStake (programmableLogicGlobalRedeemer txBody)

    addReference paramsTxIn -- Protocol Params TxIn
    addReference dirNodeRef -- Directory Node TxIn
    addWithdrawalWithTxBody -- Add the global script witness to the transaction
        (C.makeStakeAddress nid globalStakeCred)
        (C.Quantity 0)
        $ C.ScriptWitness C.ScriptWitnessForStakeAddr . programmableGlobalWitness

-- TODO: check that the issuerTxOut is at a programmable logic payment credential
_checkIssuerAddressIsProgLogicCred :: forall era ctx m. (MonadBuildTx era m) => C.PaymentCredential -> C.TxOut ctx era -> m ()
_checkIssuerAddressIsProgLogicCred _progLogicCred (C.TxOut (C.AddressInEra _ (C.ShelleyAddress _ _pcred _stakeRef)) _ _ C.ReferenceScriptNone) =
    pure ()
_checkIssuerAddressIsProgLogicCred _ _ = error "Issuer address is not a programmable logic credential"

isNodeWithProgrammableSymbol :: forall era. CurrencySymbol -> UTxODat era DirectorySetNode -> Bool
isNodeWithProgrammableSymbol programmableTokenSymbol (uDatum -> dat) = key dat == programmableTokenSymbol
