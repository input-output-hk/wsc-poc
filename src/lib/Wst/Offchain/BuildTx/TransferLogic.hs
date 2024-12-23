{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Wst.Offchain.BuildTx.TransferLogic
  ( transferSmartTokens,
    issueSmartTokens,
    seizeSmartTokens,
    initBlacklist,
    insertBlacklistNode,
  )
where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens (over)
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx (MonadBuildTx, addBtx, addReference, addRequiredSignature,
                       addScriptWithdrawal, addWithdrawalWithTxBody,
                       buildScriptWitness, findIndexReference, mintPlutus,
                       payToAddress, prependTxOut, spendPlutusInlineDatum)
import Convex.CardanoApi.Lenses as L
import Convex.Class (MonadBlockchain (queryNetworkId))
import Convex.PlutusLedger.V1 (transCredential, transPolicyId,
                               unTransStakeCredential)
import Convex.Scripts qualified as C
import Convex.Utils qualified as Utils
import Convex.Utxos (UtxoSet (UtxoSet))
import Convex.Wallet (selectMixedInputsCovering)
import Convex.Wallet.Operator (Operator (..), verificationKey)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Monoid (Last (..))
import Debug.Trace (trace)
import GHC.Exts (IsList (..))
import PlutusLedgerApi.Data.V3 (Credential (..), PubKeyHash (PubKeyHash),
                                ScriptHash (..))
import PlutusLedgerApi.V3 qualified as PlutusTx
import SmartTokens.CodeLens (_printTerm)
import SmartTokens.Contracts.ExampleTransferLogic (BlacklistProof (..))
import SmartTokens.Types.ProtocolParams
import SmartTokens.Types.PTokenDirectory (BlacklistNode (..),
                                          DirectorySetNode (..))
import Wst.Offchain.BuildTx.ProgrammableLogic (IssueNewTokenArgs (..),
                                               issueProgrammableToken,
                                               seizeProgrammableToken,
                                               transferProgrammableToken)
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query (UTxODat (..))
import Wst.Offchain.Scripts (scriptPolicyIdV3)

intaFromEnv :: forall env m. (MonadReader env m, Env.HasTransferLogicEnv env)=> m IssueNewTokenArgs
intaFromEnv = do
  Env.TransferLogicEnv{Env.tleIssuerScript, Env.tleMintingScript, Env.tleTransferScript} <- asks Env.transferLogicEnv
  pure $ IssueNewTokenArgs
    { intaTransferLogic= tleTransferScript
    , intaMintingLogic= tleMintingScript
    , intaIssuerLogic= tleIssuerScript
    }


blacklistInitialNode :: BlacklistNode
blacklistInitialNode = BlacklistNode {blnNext=PubKeyCredential "ffffffffffffffffffffffffffffffffffffffffffffffffffffffff", blnKey= PubKeyCredential ""}

initBlacklist :: forall era env m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasTransferLogicEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => m ()
initBlacklist = Utils.inBabbage @era $ do
  nid <- queryNetworkId

  -- create blacklist head node data
  let blacklistInitialNodeDatum = C.TxOutDatumInline C.babbageBasedEra $ C.toHashableScriptData blacklistInitialNode

  -- mint blacklist policy token
  mintingScript <- asks (Env.tleBlacklistMintingScript . Env.transferLogicEnv)
  let assetName = C.AssetName ""
      quantity = 1

  mintPlutus mintingScript () assetName quantity

  -- send blacklist output to blacklist spending script
  spendingScript <- asks (Env.tleBlacklistSpendingScript . Env.transferLogicEnv)
  let policyId = scriptPolicyIdV3 mintingScript
      spendingHash = C.hashScript $ C.PlutusScript C.PlutusScriptV3 spendingScript
      addr = C.makeShelleyAddressInEra C.shelleyBasedEra nid (C.PaymentCredentialByScript spendingHash) C.NoStakeAddress
      val = C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toLedgerValue @era C.maryBasedEra $ fromList [(C.AssetId policyId assetName, quantity)]
      txout = C.TxOut addr val blacklistInitialNodeDatum C.ReferenceScriptNone

  prependTxOut txout

  -- add operator signature
  opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
  addRequiredSignature opPkh

insertBlacklistNode :: forall era env m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasTransferLogicEnv env, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => C.PaymentCredential -> [UTxODat era BlacklistNode]-> m ()
insertBlacklistNode cred blacklistNodes = Utils.inBabbage @era $ do
  -- mint new blacklist token
  mintingScript <- asks (Env.tleBlacklistMintingScript . Env.transferLogicEnv)
  let newAssetName = C.AssetName $  case transCredential cred of
                        PubKeyCredential (PubKeyHash s) -> PlutusTx.fromBuiltin s
                        ScriptCredential (ScriptHash s) -> PlutusTx.fromBuiltin s
      quantity = 1
  mintPlutus mintingScript () newAssetName quantity

  let
      -- find the node to insert on
      UTxODat {uIn = prevNodeRef,uOut = (C.TxOut prevAddr prevVal _ _),  uDatum = prevNode} =
        maximumBy (compare `on` (blnKey . uDatum)) $
          filter ((<= transCredential cred) . blnKey . uDatum) blacklistNodes

      -- create new blacklist node data
      newNode = BlacklistNode {blnNext=blnNext prevNode, blnKey= transCredential cred}
      newNodeDatum = C.TxOutDatumInline C.babbageBasedEra $ C.toHashableScriptData newNode
      newNodeVal = C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toLedgerValue @era C.maryBasedEra $ fromList [(C.AssetId (scriptPolicyIdV3 mintingScript) newAssetName, quantity)]
      newNodeOutput = C.TxOut prevAddr newNodeVal newNodeDatum C.ReferenceScriptNone

      -- update the previous node to point to the new node
      newPrevNode = prevNode {blnNext=transCredential cred}
      newPrevNodeDatum = C.TxOutDatumInline C.babbageBasedEra $ C.toHashableScriptData newPrevNode
      newPrevNodeOutput = C.TxOut prevAddr prevVal newPrevNodeDatum C.ReferenceScriptNone

  -- spend previous node
  spendingScript <- asks (Env.tleBlacklistSpendingScript . Env.transferLogicEnv)
  spendPlutusInlineDatum prevNodeRef spendingScript ()
  -- set previous node output
  prependTxOut newPrevNodeOutput
  -- set new node output
  prependTxOut newNodeOutput

  -- add operator signature
  opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
  addRequiredSignature opPkh

-- TODO
_removeBlacklistNode = undefined

issueSmartTokens :: forall era env m. (MonadReader env m, Env.HasTransferLogicEnv env, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m, Env.HasOperatorEnv era env) => UTxODat era ProgrammableLogicGlobalParams -> (C.AssetName, C.Quantity) -> [UTxODat era DirectorySetNode] -> C.PaymentCredential -> m ()
issueSmartTokens paramsTxOut (an, q) directoryList destinationCred = Utils.inBabbage @era $ do
  nid <- queryNetworkId

  directoryEnv <- asks Env.directoryEnv
  let progLogicBaseCred = Env.programmableLogicBaseCredential directoryEnv
  inta <- intaFromEnv
  issuedPolicyId <- issueProgrammableToken paramsTxOut (an, q) inta directoryList
  -- TODO: check if there is a better way to achieve: C.PaymentCredential -> C.StakeCredential
  stakeCred <- either (error . ("Could not unTrans credential: " <>) . show) pure $ unTransStakeCredential $ transCredential destinationCred
  let value = fromList [(C.AssetId issuedPolicyId an, q)]
      addr = C.makeShelleyAddressInEra C.shelleyBasedEra nid progLogicBaseCred (C.StakeAddressByValue stakeCred)

  addIssueWitness
  payToAddress addr value

transferSmartTokens :: forall env era a m. (MonadReader env m, Env.HasTransferLogicEnv env, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => C.PaymentCredential -> [UTxODat era BlacklistNode] -> [UTxODat era DirectorySetNode] -> [UTxODat era a] -> (C.AssetId, C.Quantity) -> C.PaymentCredential -> m ()
transferSmartTokens userCred blacklistNodes directoryList spendingUserOutputs (assetId, q) destinationCred = Utils.inBabbage @era $ do
  nid <- queryNetworkId
  progLogicBaseCred <- asks (Env.programmableLogicBaseCredential . Env.directoryEnv)

  -- Find sufficient inputs to cover the transfer
  let userOutputsMap = fromList $ map (\UTxODat {uIn, uOut, uDatum} -> (uIn, (C.inAnyCardanoEra (C.cardanoEra @era) uOut, uDatum))) spendingUserOutputs
  (totalVal, txins) <- maybe (error "insufficient funds for transfer") pure $ selectMixedInputsCovering (UtxoSet userOutputsMap) [(assetId, q)]

  -- Spend the outputs via programmableLogicBaseScript
  let programmablePolicyId = case assetId of
        C.AssetId policyId _ -> policyId
        C.AdaAssetId -> error "Ada is not programmable"

  transferProgrammableToken txins (transPolicyId programmablePolicyId) directoryList -- Invoking the programmableBase and global scripts
  addTransferWitness blacklistNodes userCred -- Proof of non-membership of the blacklist

  -- Send outputs to destinationCred
  destStakeCred <- either (error . ("Could not unTrans credential: " <>) . show) pure $ unTransStakeCredential $ transCredential destinationCred
  let destinationVal :: C.Value = fromList [(assetId, q)]
      destinationAddress = C.makeShelleyAddressInEra C.shelleyBasedEra nid progLogicBaseCred (C.StakeAddressByValue destStakeCred)
  payToAddress destinationAddress destinationVal

  -- Return change to the spendingUserOutputs address
  let returnVal =
        C.TxOutValueShelleyBased C.shelleyBasedEra $
          C.toLedgerValue @era C.maryBasedEra $
            fromList [(assetId, C.selectAsset totalVal assetId - q)]
      returnAddr = undefined
      returnOutput = C.TxOut returnAddr returnVal C.TxOutDatumNone C.ReferenceScriptNone
  prependTxOut returnOutput -- Add the seized output to the transaction

seizeSmartTokens :: forall env era a m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasTransferLogicEnv env, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => UTxODat era a -> UTxODat era a -> [UTxODat era DirectorySetNode] -> C.PaymentCredential -> m ()
seizeSmartTokens seizingTxo issuerTxo directoryList destinationCred = Utils.inBabbage @era $ do
  -- Add issuer and programmableLogic witnesses
  let Last maybeProgAsset = case uOut seizingTxo of
        (C.TxOut _a v _d _r) ->
          foldMap
            ( \case
                (C.AssetId pid an, q) -> Last (Just (pid, an, q))
                (C.AdaAssetId, _q) -> Last Nothing
            )
            (toList $ C.txOutValueToValue v)

  (progTokenPolId, an, q) <- maybe (error "No programmable token found in seizing transaction") pure maybeProgAsset

  seizeProgrammableToken seizingTxo issuerTxo progTokenPolId directoryList
  addSeizeWitness

  -- Send seized funds to destinationCred
  let -- NOTE: Assumes only a single programmable token per UTxO is allowed
      seizedVal = fromList [(C.AssetId progTokenPolId an, q)]
      issuerAddr = case uOut issuerTxo of
        (C.TxOut a _v _d _r) -> a

  payToAddress issuerAddr seizedVal

addIssueWitness :: forall era env m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasTransferLogicEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => m ()
addIssueWitness = Utils.inBabbage @era $ do
  opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
  mintingScript <- asks (Env.tleMintingScript . Env.transferLogicEnv)
  let sh = C.hashScript $ C.PlutusScript C.PlutusScriptV3 mintingScript
  addRequiredSignature opPkh
  addScriptWithdrawal sh 0 $ buildScriptWitness mintingScript C.NoScriptDatumForStake ()

addTransferWitness :: forall env era m. (MonadReader env m, Env.HasTransferLogicEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => [UTxODat era BlacklistNode] -> C.PaymentCredential -> m ()
addTransferWitness blacklistNodes clientCred = Utils.inBabbage @era $ do
  nid <- queryNetworkId
  transferScript <- asks (Env.tleTransferScript . Env.transferLogicEnv)
  let transferStakeCred = C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 transferScript

      UTxODat {uIn = blnNodeRef, uDatum = blnNodeDatum} =
        maximumBy (compare `on` (blnKey . uDatum)) $
          filter ((<= transCredential clientCred) . blnKey . uDatum) blacklistNodes

      -- Finds the index of the blacklist node reference in the transaction ref
      -- inputs
      blacklistNodeReferenceIndex txBody =
        fromIntegral @Int @Integer $ findIndexReference blnNodeRef txBody

      -- The redeemer for the transfer script based on whether a blacklist node
      -- exists with the client credential
      transferRedeemer txBody =
        if blnKey blnNodeDatum == transCredential clientCred
          then error "Credential is blacklisted" -- TODO: handle this and other error cases properly
          else NonmembershipProof $ blacklistNodeReferenceIndex txBody

      -- TODO: extend this to handle multiple proofs (i.e. transfers) per tx, onchain allows it
      transferWitness txBody = buildScriptWitness transferScript C.NoScriptDatumForStake [transferRedeemer txBody]

  addReference blnNodeRef -- Add the blacklist node reference to the transaction
  addWithdrawalWithTxBody -- Add the global script witness to the transaction
    (C.makeStakeAddress nid transferStakeCred)
    (C.Quantity 0)
    $ C.ScriptWitness C.ScriptWitnessForStakeAddr . transferWitness

addSeizeWitness :: forall env era m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasTransferLogicEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => m ()
addSeizeWitness = Utils.inBabbage @era $ do
  opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
  seizeScript <- asks (Env.tleIssuerScript . Env.transferLogicEnv)
  let sh = C.hashScript $ C.PlutusScript C.PlutusScriptV3 seizeScript
  addRequiredSignature opPkh
  addScriptWithdrawal sh 0 $ buildScriptWitness seizeScript C.NoScriptDatumForStake ()
