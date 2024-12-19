{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}

module Wst.Offchain.BuildTx.TransferLogic (
  transferStablecoins,
  issueStablecoins,
  ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Convex.BuildTx (MonadBuildTx, addBtx, addReference, addScriptWithdrawal,
                       addStakeWitness, addWithdrawalWithTxBody,
                       buildScriptWitness, findIndexReference,
                       findIndexSpending, mintPlutus, payToAddress,
                       spendPlutusInlineDatum)
import Convex.CardanoApi.Lenses as L
import Convex.Class (MonadBlockchain (queryNetworkId))
import Convex.PlutusLedger.V1 (transPolicyId, unTransCredential,
                               unTransPolicyId)
import Convex.Scripts (fromHashableScriptData)
import Convex.Utils qualified as Utils
import Data.Either (fromRight)
import Data.Foldable (find, maximumBy)
import Data.Function (on)
import Data.Maybe (fromJust)
import PlutusLedgerApi.V3 (CurrencySymbol (..))
import PlutusTx qualified
import SmartTokens.Contracts.ExampleTransferLogic (BlacklistProof (..))
import SmartTokens.Contracts.Issuance (SmartTokenMintingAction (MintPToken, RegisterPToken))
import SmartTokens.Contracts.ProgrammableLogicBase (ProgrammableLogicGlobalRedeemer (..),
                                                    TokenProof (..))
import SmartTokens.Types.ProtocolParams
import SmartTokens.Types.PTokenDirectory (BlacklistNode (..),
                                          DirectorySetNode (..))
import Wst.Offchain.BuildTx.DirectorySet (insertDirectoryNode)
import Wst.Offchain.BuildTx.ProgrammableLogic (issueProgrammableToken)
import Wst.Offchain.BuildTx.ProtocolParams (getProtocolParamsGlobalInline)
import Wst.Offchain.Scripts (freezeAndSezieTransferScript,
                             permissionedTransferScript,
                             programmableLogicBaseScript,
                             programmableLogicGlobalScript,
                             programmableLogicMintingScript)

issueStablecoins :: forall era m. (C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => C.PaymentCredential -> C.Value -> m ()
issueStablecoins issuerLogicCred amount = Utils.inBabbage @era $ do
  symbol <- issueProgrammableToken undefined undefined undefined undefined undefined
  addIssueStablecoinsWitness undefined

  -- TODO: create the value to be minted and the special address to send it to
  let value = undefined
      addr = undefined --
  payToAddress value addr

addIssueStablecoinsWitness :: forall era m. (C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => C.Hash C.PaymentKey -> m ()
addIssueStablecoinsWitness issuerPubKeyHash = Utils.inBabbage @era $ do
  let mintingScript = permissionedTransferScript issuerPubKeyHash
      sh = C.hashScript $ C.PlutusScript C.PlutusScriptV3 mintingScript
  addScriptWithdrawal sh 0 $ buildScriptWitness mintingScript C.NoScriptDatumForStake ()


transferStablecoins :: forall era m. (C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => C.PaymentCredential -> C.PolicyId -> [(C.TxIn, C.TxOut C.CtxTx era)] -> [(C.TxIn, C.TxOut C.CtxTx era)] -> C.Value -> C.PaymentCredential -> m ()
transferStablecoins transferLogicCred blacklistPolicyId blacklistOutputs userOutputs amount destinationCred = pure ()

addTransferWitness :: forall era m. (C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => C.PolicyId -> [(C.TxIn, C.TxOut C.CtxTx era)] -> C.PaymentCredential -> m ()
addTransferWitness blacklistPolicyId blacklistNodes clientCred = Utils.inBabbage @era $ do
  nid <- queryNetworkId
  let transferScript = freezeAndSezieTransferScript blacklistPolicyId
      transferStakeCred = C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 transferScript

      (blnNodeRef, blnNodeOut) =
        maximumBy (compare `on` (fmap blnKey . getDatumInline @BlacklistNode . C.inAnyCardanoEra (C.cardanoEra @era) . snd)) $
          filter (maybe False ((<= clientCred) . fromRight (error "could not unTrans credential") . unTransCredential . blnKey) . getDatumInline @BlacklistNode  . C.inAnyCardanoEra (C.cardanoEra @era) . snd) blacklistNodes

      -- Finds the index of the blacklist node reference in the transaction ref
      -- inputs
      blacklistNodeReferenceIndex txBody =
        fromIntegral @Int @Integer $ findIndexReference blnNodeRef txBody

      -- The redeemer for the transfer script based on whether a blacklist node
      -- exists with the client credential
      transferRedeemer txBody =
        if fmap
            (fromRight (error "could not unTrans credential") . unTransCredential . blnKey)
            (getDatumInline @BlacklistNode $ C.inAnyCardanoEra (C.cardanoEra @era) blnNodeOut)
           == Just clientCred
          then error "Credential is blacklisted" -- TODO: handle this and other error cases properly
          else NonmembershipProof $ blacklistNodeReferenceIndex txBody

      -- TODO: extend this to handle multiple proofs (i.e. transfers) per tx, onchain allows it
      transferWitness txBody = buildScriptWitness transferScript C.NoScriptDatumForStake [transferRedeemer txBody]

  addReference blnNodeRef -- Add the blacklist node reference to the transaction
  addWithdrawalWithTxBody -- Add the global script witness to the transaction
    (C.makeStakeAddress nid transferStakeCred)
    (C.Quantity 0)
    $ C.ScriptWitness C.ScriptWitnessForStakeAddr . transferWitness

addSeizeWitness :: forall era m. (C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => C.Hash C.PaymentKey -> m ()
addSeizeWitness issuerPubKeyHash = Utils.inBabbage @era $ do
  let seizeScript = permissionedTransferScript issuerPubKeyHash
      sh = C.hashScript $ C.PlutusScript C.PlutusScriptV3 seizeScript
  addScriptWithdrawal sh 0 $ buildScriptWitness seizeScript C.NoScriptDatumForStake ()


-- TODO: move to separate utils module
getDatumInline :: forall a. (PlutusTx.FromData a) => C.InAnyCardanoEra (C.TxOut C.CtxTx) -> Maybe a
getDatumInline (C.InAnyCardanoEra C.ConwayEra (C.TxOut _ _ dat _)) =
  case dat of
    C.TxOutDatumInline C.BabbageEraOnwardsConway (fromHashableScriptData -> Just d) -> Just d
    _ -> Nothing
getDatumInline _ = Nothing
