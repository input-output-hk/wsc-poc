{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
module Wst.Offchain.ProgrammableLogic
  ( transferProgrammableToken,
    seizePragrammableToken,
  )
where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens (over)
import Convex.BuildTx (MonadBuildTx, addBtx, addReference, addStakeScriptWitness, addWithdrawalWithTxBody, buildScriptWitness, findIndexReference, mintPlutus, spendPlutusInlineDatum, spendPlutusRefWithoutInRefInlineDatum, spendPublicKeyOutput)
import Convex.CardanoApi.Lenses qualified as L
import Convex.PlutusLedger (transPolicyId, unTransAssetName)
import Convex.Scripts (fromHashableScriptData, toHashableScriptData)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.List (find)
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import PlutusLedgerApi.V3 (Credential (..), CurrencySymbol (..))
import PlutusLedgerApi.V3 qualified as P
import PlutusTx qualified
import Wst.Offchain.DirectorySet (DirectorySetNode (..))

-- TODO: to be imported
programmableLogicMintingScript :: Credential -> C.PlutusScript C.PlutusScriptV3
programmableLogicMintingScript mintingCred = undefined

-- TODO: To be imported after merge with onchain
programmableLogicBaseScript :: C.PlutusScript C.PlutusScriptV3 -- Parameterized by the stake cred of the global script
programmableLogicBaseScript = undefined

-- TODO: To be imported after merge with onchain
programmableLogicGlobalScript :: C.PlutusScript C.PlutusScriptV3 -- Parameterized by the CS holding protocol params datum
programmableLogicGlobalScript = undefined

programmableLogicGlobalStakeCredential :: C.StakeCredential
programmableLogicGlobalStakeCredential = C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 programmableLogicGlobalScript

programmableLogicGlobalStakeAddress :: C.NetworkId -> C.StakeAddress
programmableLogicGlobalStakeAddress nid = C.makeStakeAddress nid programmableLogicGlobalStakeCredential

-- data ProgrammableLogicGlobalRedeemer (s :: S)
--   = PTransferAct
--       ( Term s ( PDataRecord '[ "proofs" ':= PBuiltinList (PAsData PTokenProof) ] ) )
--   | PSeizeAct
--       ( Term
--           s
--           ( PDataRecord
--               '[ "seizeInputIdx" ':= PInteger
--                , "seizeOutputIdx" ':= PInteger
--                , "directoryNodeIdx" ':= PInteger
--                ]
--           )
--       )

issueProgrammableToken :: (MonadBuildTx C.ConwayEra m) => C.NetworkId -> C.TxIn -> (C.AssetName, C.Quantity) -> Credential-> m CurrencySymbol
issueProgrammableToken nid paramsTxIn (an, q) mintingCred = do

  addReference paramsTxIn
  let policyId = transPolicyId $ C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV3 $ programmableLogicMintingScript mintingCred
  addStakeScriptWitness mintingCred (programmableLogicMintingScript mintingCred) () -- TODO: minting logic redeemer

  -- TODO: register token redeemer should go here
  mintPlutus (programmableLogicMintingScript mintingCred) () an q

  pure policyId

transferProgrammableToken :: (MonadBuildTx C.ConwayEra m) => C.NetworkId -> C.TxIn -> C.TxIn -> CurrencySymbol -> [(C.TxIn, C.InAnyCardanoEra (C.TxOut C.CtxTx))] -> m ()
transferProgrammableToken nid paramsTxIn tokenTxIn programmableTokenSymbol [] = error "directory list empty"
transferProgrammableToken nid paramsTxIn tokenTxIn programmableTokenSymbol directoryList = do
  addReference paramsTxIn
  spendPlutusInlineDatum tokenTxIn programmableLogicBaseScript ()

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

      programmableGlobalWitness txBody = buildScriptWitness programmableLogicGlobalScript C.NoScriptDatumForStake (programmableLogicGlobalRedeemer txBody)

  addWithdrawalWithTxBody
    (programmableLogicGlobalStakeAddress nid)
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
