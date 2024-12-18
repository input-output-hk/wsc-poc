{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
module Wst.Offchain.BuildTx.ProgrammableLogic
  ( transferProgrammableToken,
    seizePragrammableToken,
  )
where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Convex.BuildTx (MonadBuildTx, addReference, addWithdrawalWithTxBody,
                       buildScriptWitness, findIndexReference, mintPlutus,
                       spendPlutusInlineDatum)
import Convex.PlutusLedger.V1 (transPolicyId)
import Convex.Scripts (fromHashableScriptData)
import Data.Foldable (maximumBy)
import Data.Function (on)
import PlutusLedgerApi.V3 (Credential (..), CurrencySymbol (..))
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..))
import Wst.Offchain.Scripts (programmableLogicBaseScript,
                             programmableLogicGlobalScript,
                             programmableLogicMintingScript)

-- Takes care of both registrations and token mints
issueProgrammableToken :: (MonadBuildTx C.ConwayEra m) => C.NetworkId -> C.TxIn -> (C.AssetName, C.Quantity) -> Credential -> [(C.TxIn, C.InAnyCardanoEra (C.TxOut C.CtxTx))]-> m CurrencySymbol
issueProgrammableToken nid paramsTxIn (an, q) mintingCred directoyrList = do

  let paymentCred = undefined
      mintingCred = undefined
      nodeCS = undefined

  let mintingScript = programmableLogicMintingScript paymentCred mintingCred nodeCS

  addReference paramsTxIn
  let policyId = transPolicyId $ C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV3 mintingScript
  -- addStakeScriptWitness mintingCred (programmableLogicMintingScript mintingCred) () -- TODO: minting logic redeemer

  -- TODO: register token redeemer should go here
  mintPlutus mintingScript () an q

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
