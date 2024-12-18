{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use second" #-}
module Wst.Offchain.BuildTx.ProgrammableLogic
  ( issueProgrammableToken,
    transferProgrammableToken,
    seizePragrammableToken,
  )
where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Convex.BuildTx (MonadBuildTx, addReference,
                       addWithdrawZeroPlutusV2InTransaction, addWithdrawal,
                       addWithdrawalWithTxBody, buildScriptWitness,
                       findIndexReference, mintPlutus, spendPlutusInlineDatum)
import Convex.Class (MonadBlockchain (queryNetworkId))
import Convex.PlutusLedger.V1 (transPolicyId, unTransCredential,
                               unTransPolicyId)
import Convex.Scripts (fromHashableScriptData)
import Convex.Utils qualified as Utils
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (find, maximumBy)
import Data.Function (on)
import Data.Maybe (fromJust)
import PlutusLedgerApi.V3 (Credential (..), CurrencySymbol (..))
import SmartTokens.Contracts.Issuance (SmartTokenMintingAction (MintPToken, RegisterPToken))
import SmartTokens.Types.ProtocolParams
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..))
import Wst.Offchain.BuildTx.DirectorySet (insertDirectoryNode)
import Wst.Offchain.BuildTx.ProtocolParams (getProtocolParamsGlobalInline)
import Wst.Offchain.Scripts (programmableLogicBaseScript,
                             programmableLogicGlobalScript,
                             programmableLogicMintingScript)

-- Takes care of both registrations and token mints
issueProgrammableToken :: forall era m. (C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => C.TxIn -> (C.PolicyId, C.TxOut C.CtxTx era) -> (C.AssetName, C.Quantity) -> (C.StakeCredential, C.StakeCredential, C.StakeCredential) -> [(C.TxIn, C.TxOut C.CtxTx era)]-> m CurrencySymbol
issueProgrammableToken directoryInitialTxIn (paramsPolicyId, paramsTxOut) (an, q) (mintingCred, transferLogic, issuerLogic) directoryList = Utils.inBabbage @era $ do
  netId <- queryNetworkId

  ProgrammableLogicGlobalParams{directoryNodeCS, progLogicCred} <- maybe (error "could not parse protocol params") pure $ getProtocolParamsGlobalInline (C.inAnyCardanoEra (C.cardanoEra @era) paramsTxOut)

  progLogicScriptCredential <- either (const $ error "could not parse protocol params") pure $ unTransCredential progLogicCred
  directoryNodeSymbol <- either (const $ error "could not parse protocol params") pure $ unTransPolicyId directoryNodeCS

  let mintingScript = programmableLogicMintingScript progLogicScriptCredential mintingCred directoryNodeSymbol
      policyId = transPolicyId $ C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV3 mintingScript

      (dirNodeRef, dirNodeOut) =
        maximumBy (compare `on` (fmap key . getDirectoryNodeInline . C.inAnyCardanoEra (C.cardanoEra @era) . snd)) $
          filter (maybe False ((<= policyId) . key) . getDirectoryNodeInline . C.inAnyCardanoEra (C.cardanoEra @era) . snd) directoryList

  dirNodeData <- maybe (error "could not parse directory node data") pure $ getDirectoryNodeInline $ C.inAnyCardanoEra (C.cardanoEra @era) dirNodeOut

  if key dirNodeData == policyId
    then
      mintPlutus mintingScript MintPToken an q
    else
      mintPlutus mintingScript RegisterPToken an q
      >> insertDirectoryNode paramsPolicyId directoryInitialTxIn (dirNodeRef, dirNodeOut) (policyId, transferLogic, issuerLogic)

  pure policyId

transferProgrammableToken :: (MonadBuildTx C.ConwayEra m) => C.NetworkId -> (C.TxIn, C.PolicyId) -> C.TxIn -> CurrencySymbol -> [(C.TxIn, C.InAnyCardanoEra (C.TxOut C.CtxTx))] -> m ()
transferProgrammableToken nid paramsTxIn tokenTxIn programmableTokenSymbol [] = error "directory list empty"
transferProgrammableToken nid (paramsTxIn, paramsPolId) tokenTxIn programmableTokenSymbol directoryList = do

  let globalStakeScript = programmableLogicGlobalScript  paramsPolId
      globalStakeCred = C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 globalStakeScript
      baseSpendingScript = programmableLogicBaseScript globalStakeCred

  addReference paramsTxIn
  spendPlutusInlineDatum tokenTxIn baseSpendingScript ()

  -- Finds the directory node with the highest key that is less than or equal
  -- to the programmable token symbol
  let (dirNodeRef, dirNodeOut) =
        maximumBy (compare `on` (fmap key . getDirectoryNodeInline . snd)) $
          filter (maybe False ((<= programmableTokenSymbol) . key) . getDirectoryNodeInline . snd) directoryList

  addReference dirNodeRef
  let
      -- Finds the index of the directory node reference in the transaction ref
      -- inputs
      directoryNodeReferenceIndex txBody =
        fromIntegral @Int @Integer $ findIndexReference dirNodeRef txBody

      -- The redeemer for the global script based on whether a dirctory node
      -- exists with the programmable token symbol
      programmableLogicGlobalRedeemer txBody =
        if fmap key (getDirectoryNodeInline dirNodeOut) == Just programmableTokenSymbol
          then directoryNodeReferenceIndex txBody -- TODO: wrap in PTokenExists
          else directoryNodeReferenceIndex txBody -- TODO: wrap in PTokenNotExists

      programmableGlobalWitness txBody = buildScriptWitness globalStakeScript C.NoScriptDatumForStake (programmableLogicGlobalRedeemer txBody)

  addWithdrawalWithTxBody
    (C.makeStakeAddress nid globalStakeCred)
    (C.Quantity 0)
    $ C.ScriptWitness C.ScriptWitnessForStakeAddr . programmableGlobalWitness

seizePragrammableToken :: (MonadBuildTx C.ConwayEra m) => m ()
seizePragrammableToken = pure ()

isNodeWithProgrammableSymbol :: CurrencySymbol -> (C.TxIn, C.InAnyCardanoEra (C.TxOut C.CtxTx)) -> Bool
isNodeWithProgrammableSymbol programmableTokenSymbol (_, dn) =
  case getDirectoryNodeInline dn of
    Just d -> key d == programmableTokenSymbol
    _ -> False

getDirectoryNodeInline :: C.InAnyCardanoEra (C.TxOut C.CtxTx) -> Maybe DirectorySetNode
getDirectoryNodeInline (C.InAnyCardanoEra C.ConwayEra (C.TxOut _ _ d _)) =
  case d of
    C.TxOutDatumInline C.BabbageEraOnwardsConway (fromHashableScriptData -> Just d) -> Just d
    _ -> Nothing
getDirectoryNodeInline _ = Nothing
