{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Wst.Offchain.BuildTx.TransferLogic
  ( transferSmartTokens,
    FindProofResult(..),
    issueSmartTokens,
    SeizeReason(..),
    seizeSmartTokens,
    initBlacklist,
    BlacklistReason(..),
    insertBlacklistNode,
    removeBlacklistNode,
    paySmartTokensToDestination,
    registerTransferScripts,
    blacklistInitialNode
  )
where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens (at, over, set, (&), (?~), (^.))
import Control.Monad (when)
import Control.Monad.Error.Lens (throwing_)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx (MonadBuildTx (addTxBuilder), TxBuilder (TxBuilder),
                       addBtx, addRequiredSignature, addScriptWithdrawal,
                       addWithdrawalWithTxBody, buildScriptWitness,
                       findIndexReference, mintPlutus, payToAddress,
                       prependTxOut, spendPlutusInlineDatum)
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (MonadBlockchain (queryNetworkId))
import Convex.PlutusLedger.V1 (transCredential, transPolicyId,
                               transStakeCredential, unTransStakeCredential)
import Convex.Scripts qualified as C
import Convex.Utils qualified as Utils
import Convex.Utxos (UtxoSet (UtxoSet))
import Convex.Wallet (selectMixedInputsCovering)
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.List (find, nub, sort)
import Data.Monoid (Last (..))
import Data.OpenApi (NamedSchema (..), OpenApiType (OpenApiString),
                     ToSchema (..))
import Data.OpenApi.Lens qualified as L
import Data.String (IsString)
import Data.Text (Text)
import GHC.Exts (IsList (..))
import PlutusLedgerApi.V3 (Credential (..), PubKeyHash (PubKeyHash),
                           ScriptHash (..))
import PlutusLedgerApi.V3 qualified as PlutusTx
import SmartTokens.Contracts.ExampleTransferLogic (BlacklistProof (..))
import SmartTokens.Contracts.IssuanceCborHex (IssuanceCborHex (..))
import SmartTokens.Types.ProtocolParams
import SmartTokens.Types.PTokenDirectory (BlacklistNode (..),
                                          DirectorySetNode (..))
import Wst.AppError (AsProgrammableTokensError (..))
import Wst.Offchain.BuildTx.ProgrammableLogic (issueProgrammableToken,
                                               seizeProgrammableToken,
                                               transferProgrammableToken)
import Wst.Offchain.BuildTx.Utils (addConwayStakeCredentialCertificate)
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query (UTxODat (..))
import Wst.Offchain.Scripts (scriptPolicyIdV3)

{-
>>> _printTerm $ unsafeEvalTerm NoTracing (pconstant blacklistInitialNode)
"program\n  1.0.0\n  (List [B #, B #ffffffffffffffffffffffffffffffffffffffffffffffffffffffff])"
-}
blacklistInitialNode :: BlacklistNode
blacklistInitialNode =
  BlacklistNode
    { blnNext= "ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
    , blnKey= ""}

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

{-| Reason for adding an address to the blacklist
-}
newtype BlacklistReason = BlacklistReason Text
  deriving newtype (Eq, Show, IsString, ToJSON, FromJSON, Semigroup, Monoid)

instance ToSchema BlacklistReason where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "BlacklistReason")
    $ mempty
        & L.type_ ?~ OpenApiString
        & L.description ?~ "Reason for adding an address to the blacklist"

{-| Add an entry for the blacklist reason to the transaction metadata
-}
addBlacklistReason :: (C.IsShelleyBasedEra era, MonadBuildTx era m) => BlacklistReason -> m ()
addBlacklistReason (BlacklistReason reason) =
  addBtx (set (L.txMetadata . L._TxMetadata . at 1) (Just (C.TxMetaMap [(C.TxMetaText "reason", C.metaTextChunks reason)])))

insertBlacklistNode :: forall era env err m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasTransferLogicEnv env, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m, MonadError err m, AsProgrammableTokensError err) => BlacklistReason -> C.PaymentCredential -> [UTxODat era BlacklistNode]-> m ()
insertBlacklistNode reason cred blacklistNodes = Utils.inBabbage @era $ do
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
          filter ((<= unwrapCredential (transCredential cred)) . blnKey . uDatum) blacklistNodes

      -- create new blacklist node data
      newNode = BlacklistNode {blnNext=blnNext prevNode, blnKey= unwrapCredential (transCredential cred)}
      newNodeDatum = C.TxOutDatumInline C.babbageBasedEra $ C.toHashableScriptData newNode
      newNodeVal = C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toLedgerValue @era C.maryBasedEra $ fromList [(C.AssetId (scriptPolicyIdV3 mintingScript) newAssetName, quantity)]
      newNodeOutput = C.TxOut prevAddr newNodeVal newNodeDatum C.ReferenceScriptNone

      -- update the previous node to point to the new node
      newPrevNode = prevNode {blnNext=unwrapCredential (transCredential cred)}
      newPrevNodeDatum = C.TxOutDatumInline C.babbageBasedEra $ C.toHashableScriptData newPrevNode
      newPrevNodeOutput = C.TxOut prevAddr prevVal newPrevNodeDatum C.ReferenceScriptNone

  when (blnKey prevNode == blnKey newNode)
    $ throwing_ _DuplicateBlacklistNode

  -- spend previous node
  spendingScript <- asks (Env.tleBlacklistSpendingScript . Env.transferLogicEnv)
  spendPlutusInlineDatum prevNodeRef spendingScript ()
  -- set previous node output
  prependTxOut newPrevNodeOutput
  -- set new node output
  prependTxOut newNodeOutput

  addBlacklistReason reason

  -- add operator signature
  opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
  addRequiredSignature opPkh

removeBlacklistNode :: forall era env err m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasTransferLogicEnv env, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m, MonadError err m, AsProgrammableTokensError err) => C.PaymentCredential -> [UTxODat era BlacklistNode]-> m ()
removeBlacklistNode cred blacklistNodes = Utils.inBabbage @era $ do
  opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
  blacklistSpendingScript <- asks (Env.tleBlacklistSpendingScript . Env.transferLogicEnv)
  blacklistMintingScript <- asks (Env.tleBlacklistMintingScript . Env.transferLogicEnv)
  blacklistPolicyId <- asks (Env.blacklistNodePolicyId . Env.transferLogicEnv)

  -- find node to remove
  UTxODat{uIn = delNodeRef, uOut = (C.TxOut _delAddr delOutVal _ _),  uDatum = delNodeDatum}
    <- maybe (throwing_ _BlacklistNodeNotFound) pure $ find ((== unwrapCredential (transCredential cred)) . blnKey . uDatum) blacklistNodes


  let expectedAssetName = C.AssetName $  case transCredential cred of
        PubKeyCredential (PubKeyHash s) -> PlutusTx.fromBuiltin s
        ScriptCredential (ScriptHash s) -> PlutusTx.fromBuiltin s

      v = C.selectAsset (C.txOutValueToValue delOutVal) (C.AssetId blacklistPolicyId expectedAssetName)

  when (v /= 1) $ error "Unexpected blacklist node token quantity. Head node should not be deleted"

      -- find the node to update to point to the node after the node to remove
  let UTxODat {uIn = prevNodeRef,uOut = (C.TxOut prevAddr prevVal _ _),  uDatum = prevNode} =
        maximumBy (compare `on` (blnKey . uDatum)) $
          filter ((< unwrapCredential (transCredential cred)) . blnKey . uDatum) blacklistNodes

      -- update the previous node to point to the node after the node to remove
      updatedPrevNode = prevNode {blnNext=blnNext delNodeDatum}
      updatedPrevNodeDatum = C.TxOutDatumInline C.babbageBasedEra $ C.toHashableScriptData updatedPrevNode
      updatedPrevNodeOutput = C.TxOut prevAddr prevVal updatedPrevNodeDatum C.ReferenceScriptNone


  -- spend the node to remove
  spendPlutusInlineDatum delNodeRef blacklistSpendingScript ()
  -- set previous node output
  spendPlutusInlineDatum prevNodeRef blacklistSpendingScript ()
  -- set previous node output
  prependTxOut updatedPrevNodeOutput
  -- burn the removed node blacklist token
  mintPlutus blacklistMintingScript () expectedAssetName (-1)
  addRequiredSignature opPkh


{-| Add a smart token output that locks the given value,
addressed to the payment credential
-}
paySmartTokensToDestination :: forall era env m. (MonadBuildTx era m, MonadReader env m, Env.HasDirectoryEnv env, MonadBlockchain era m, C.IsBabbageBasedEra era) => (C.AssetName, C.Quantity) -> C.PolicyId -> C.PaymentCredential -> m ()
paySmartTokensToDestination (an, q) issuedPolicyId destinationCred = Utils.inBabbage @era $ do
  let value = fromList [(C.AssetId issuedPolicyId an, q)]
  addr <- Env.programmableTokenReceivingAddress destinationCred
  payToAddress addr value

issueSmartTokens :: forall era env m. (MonadReader env m, Env.HasTransferLogicEnv env, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m, Env.HasOperatorEnv era env) => UTxODat era ProgrammableLogicGlobalParams -> UTxODat era IssuanceCborHex -> (C.AssetName, C.Quantity) -> [UTxODat era DirectorySetNode] -> C.PaymentCredential -> m C.AssetId
issueSmartTokens paramsTxOut issuanceCborHexTxOut (an, q) directoryList destinationCred = Utils.inBabbage @era $ do
  issuedPolicyId <- issueProgrammableToken paramsTxOut issuanceCborHexTxOut (an, q) directoryList
  addIssueWitness
  --  payToAddress addr value
  paySmartTokensToDestination (an, q) issuedPolicyId destinationCred
  pure $ C.AssetId issuedPolicyId an

transferSmartTokens :: forall env era err a m. (MonadReader env m, Env.HasTransferLogicEnv env, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m, Env.HasOperatorEnv era env, MonadError err m) => UTxODat era ProgrammableLogicGlobalParams -> [UTxODat era BlacklistNode] -> [UTxODat era DirectorySetNode] -> [UTxODat era a] -> (C.AssetId, C.Quantity) -> C.PaymentCredential -> m (FindProofResult era)
transferSmartTokens paramsTxIn blacklistNodes directoryList spendingUserOutputs (assetId, q) destinationCred = Utils.inBabbage @era $ do
  nid <- queryNetworkId
  userCred <- Env.operatorPaymentCredential
  progLogicBaseCred <- asks (Env.programmableLogicBaseCredential . Env.directoryEnv)

  -- Find sufficient inputs to cover the transfer
  let userOutputsMap = fromList $ map (\UTxODat {uIn, uOut, uDatum} -> (uIn, (C.inAnyCardanoEra (C.cardanoEra @era) uOut, uDatum))) spendingUserOutputs
  (totalVal, txins) <- maybe (error "insufficient funds for transfer") pure $ selectMixedInputsCovering (UtxoSet userOutputsMap) [(assetId, q)]

  -- Spend the outputs via programmableLogicBaseScript
  let programmablePolicyId = case assetId of
        C.AssetId policyId _ -> policyId
        C.AdaAssetId -> error "Ada is not programmable"

  transferProgrammableToken paramsTxIn txins (transPolicyId programmablePolicyId) directoryList -- Invoking the programmableBase and global scripts
  result <- addTransferWitness blacklistNodes -- Proof of non-membership of the blacklist

  -- Send outputs to destinationCred
  destStakeCred <- either (error . ("Could not unTrans credential: " <>) . show) pure $ unTransStakeCredential $ transCredential destinationCred
  let destinationVal :: C.Value = fromList [(assetId, q)]
      destinationAddress = C.makeShelleyAddressInEra C.shelleyBasedEra nid progLogicBaseCred (C.StakeAddressByValue destStakeCred)
  payToAddress destinationAddress destinationVal

  -- Return change to the spendingUserOutputs address
  srcStakeCred <- either (error . ("Could not unTrans credential: " <>) . show) pure $ unTransStakeCredential $ transCredential userCred
  let returnVal =
        C.TxOutValueShelleyBased C.shelleyBasedEra $
          C.toLedgerValue @era C.maryBasedEra $
            fromList [(assetId, C.selectAsset totalVal assetId - q)]
      returnAddr = C.makeShelleyAddressInEra C.shelleyBasedEra nid progLogicBaseCred (C.StakeAddressByValue srcStakeCred)
      returnOutput = C.TxOut returnAddr returnVal C.TxOutDatumNone C.ReferenceScriptNone
  prependTxOut returnOutput -- Add the seized output to the transaction
  pure result

{-| Reason for adding an address to the blacklist
-}
newtype SeizeReason = SeizeReason Text
  deriving newtype (Eq, Show, IsString, ToJSON, FromJSON, Semigroup, Monoid)

instance ToSchema SeizeReason where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "SeizeReason")
    $ mempty
        & L.type_ ?~ OpenApiString
        & L.description ?~ "Reason for seizing funds"

{-| Add an entry for the blacklist reason to the transaction metadata
-}
addSeizeReason :: (C.IsShelleyBasedEra era, MonadBuildTx era m) => SeizeReason -> m ()
addSeizeReason (SeizeReason reason) =
  addBtx (set (L.txMetadata . L._TxMetadata . at 1) (Just (C.TxMetaMap [(C.TxMetaText "reason", C.metaTextChunks reason)])))

seizeSmartTokens :: forall env era a m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasTransferLogicEnv env, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => SeizeReason -> UTxODat era ProgrammableLogicGlobalParams -> UTxODat era a -> C.PaymentCredential -> [UTxODat era DirectorySetNode] -> m ()
seizeSmartTokens reason paramsTxIn seizingTxo destinationCred directoryList = Utils.inBabbage @era $ do
  nid <- queryNetworkId

  let -- NOTE: Assumes only a single programmable token per UTxO is allowed
      Last maybeProgAsset = case uOut seizingTxo of
        (C.TxOut _a v _d _r) ->
          foldMap
            ( \case
                (C.AssetId pid an, q) -> Last (Just (pid, an, q))
                (C.AdaAssetId, _q) -> Last Nothing
            )
            (toList $ C.txOutValueToValue v)

  (progTokenPolId, an, q) <- maybe (error "No programmable token found in seizing transaction") pure maybeProgAsset
  seizeProgrammableToken paramsTxIn seizingTxo progTokenPolId directoryList
  addSeizeWitness

  progLogicBaseCred <- asks (Env.programmableLogicBaseCredential . Env.directoryEnv)
  destStakeCred <- either (error . ("Could not unTrans credential: " <>) . show) pure $ unTransStakeCredential $ transCredential destinationCred
  let
      destinationAddress = C.makeShelleyAddressInEra C.shelleyBasedEra nid progLogicBaseCred (C.StakeAddressByValue destStakeCred)

      -- NOTE: Assumes only a single programmable token per UTxO is allowed
      seizedVal = fromList [(C.AssetId progTokenPolId an, q)]

  addSeizeReason reason
  -- Send seized funds to destinationCred
  payToAddress destinationAddress seizedVal

addIssueWitness :: forall era env m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasTransferLogicEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => m ()
addIssueWitness = Utils.inBabbage @era $ do
  opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
  mintingScript <- asks (Env.tleMintingScript . Env.transferLogicEnv)
  let sh = C.hashScript $ C.PlutusScript C.PlutusScriptV3 mintingScript
  addRequiredSignature opPkh
  addScriptWithdrawal sh 0 $ buildScriptWitness mintingScript C.NoScriptDatumForStake ()

{-| Extracts the credentials that can be used for a transfer from the transaction body
-}
transferWitnesses :: C.TxBodyContent v era -> [Credential]
transferWitnesses txBody =
  let wdrls = case txBody ^. L.txWithdrawals of
                -- Maybe `sort` here is redundant if txWithdrawals are already sorted
                C.TxWithdrawals _ wdrls' -> sort $ map (\(stkAddr,_,_) -> transStakeCredential $ C.stakeAddressCredential stkAddr) wdrls'
                _ -> []

      signatories = case txBody ^. L.txExtraKeyWits of
                      C.TxExtraKeyWitnesses _ pkhs -> map (transCredential . C.PaymentCredentialByKey) pkhs
                      _ -> []

  in wdrls <> signatories

data FindProofResult era =
  NoBlacklistNodes -- TODO: Use NonEmpty list to avoid this
  | CredentialBlacklisted (UTxODat era BlacklistNode) -- ^ A node containing exactly the credential was found. (Negative result, transfer not OK)
  | CredentialNotBlacklisted (UTxODat era BlacklistNode) -- ^ A node was found that spans the credential but does not match it exactly. (Positive result, transfer OK)

{-| Find the blacklist node that covers the credential but does not match it exactly
-}
tryFindProof :: [UTxODat era BlacklistNode] -> Credential -> UTxODat era BlacklistNode
tryFindProof blacklistNodes cred =
  case findProof blacklistNodes cred of
    CredentialNotBlacklisted r -> r
    CredentialBlacklisted r -> r
    _ -> error $ "tryFindProof failed for " <> show cred

{-| Find the blacklist node that covers the credential.
-}
findProof :: [UTxODat era BlacklistNode] -> Credential -> FindProofResult era
findProof [] _cred = NoBlacklistNodes
findProof blacklistNodes cred =
  let node@UTxODat {uDatum = blnNodeDatum}
        = maximumBy (compare `on` (blnKey . uDatum)) $
            filter ((<= unwrapCredential cred) . blnKey . uDatum) blacklistNodes
  in if blnKey blnNodeDatum == unwrapCredential cred
      then CredentialBlacklisted node
      else CredentialNotBlacklisted node

{-| Add a proof that the user is allowed to transfer programmable tokens.
Uses the user from 'HasOperatorEnv env'. Fails if the user is blacklisted.
-}
addTransferWitness :: forall env era err m. (MonadError err m, MonadReader env m, Env.HasOperatorEnv era env, Env.HasTransferLogicEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => [UTxODat era BlacklistNode] -> m (FindProofResult era)
addTransferWitness blacklistNodes = Utils.inBabbage @era $ do
  opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv) -- In this case 'operator' is the user
  nid <- queryNetworkId
  transferScript <- asks (Env.tleTransferScript . Env.transferLogicEnv)

  let
      transferStakeCred = C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 transferScript

      -- Finds the index of the blacklist node in the reference scripts
      findWitnessReferenceIndex txBody cred =
        let UTxODat {uIn} = tryFindProof blacklistNodes cred
        in fromIntegral @Int @Integer $ findIndexReference uIn txBody

      -- Maps the credential to the index of the blacklist node in the reference scripts
      witnessReferences txBody = map (uIn . tryFindProof blacklistNodes) $ transferWitnesses txBody

      -- Maps the credential to the index of the blacklist node in the reference scripts and wraps in redeemer
      transferRedeemer txBody = map (NonmembershipProof . findWitnessReferenceIndex txBody) $ transferWitnesses txBody

      -- Builds the script witness for the transfer
      transferStakeWitness txBody = buildScriptWitness transferScript C.NoScriptDatumForStake (transferRedeemer txBody)

  -- Check that none of the witnesses are on the blacklist
  -- This means we're traversing the list of blacklist nodes an additional time.
  -- But here is the only place where we can use MonadError. So we have to do it
  -- here to allow the client code to handle the error properly.
  let proofResult = findProof blacklistNodes (transCredential $ C.PaymentCredentialByKey opPkh)

  addRequiredSignature opPkh
  addReferencesWithTxBody witnessReferences
  addWithdrawalWithTxBody -- Add the global script witness to the transaction
    (C.makeStakeAddress nid transferStakeCred)
    (C.Quantity 0)
    $ C.ScriptWitness C.ScriptWitnessForStakeAddr . transferStakeWitness
  pure proofResult

addReferencesWithTxBody :: (MonadBuildTx era m, C.IsBabbageBasedEra era) => (C.TxBodyContent C.BuildTx era -> [C.TxIn]) -> m ()
addReferencesWithTxBody f =
  addTxBuilder (TxBuilder $ \body -> over (L.txInsReference . L._TxInsReferenceIso) (nub . (f body <>)))

addSeizeWitness :: forall env era m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasTransferLogicEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => m ()
addSeizeWitness = Utils.inBabbage @era $ do
  opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
  seizeScript <- asks (Env.tleIssuerScript . Env.transferLogicEnv)
  let sh = C.hashScript $ C.PlutusScript C.PlutusScriptV3 seizeScript
  addRequiredSignature opPkh
  addScriptWithdrawal sh 0 $ buildScriptWitness seizeScript C.NoScriptDatumForStake ()

unwrapCredential :: Credential -> PlutusTx.BuiltinByteString
unwrapCredential = \case
  PubKeyCredential (PubKeyHash s) -> s
  ScriptCredential (ScriptHash s) -> s

registerTransferScripts :: forall env era m. (MonadReader env m, Env.HasTransferLogicEnv env, C.IsBabbageBasedEra era, MonadBuildTx era m) => m ()
registerTransferScripts = case C.babbageBasedEra @era of
  C.BabbageEraOnwardsBabbage -> error "babbage era registration not implemented"
  C.BabbageEraOnwardsConway  -> Utils.inConway @era $ do
    transferMintingScript <- asks (Env.tleMintingScript . Env.transferLogicEnv)
    transferSpendingScript <- asks (Env.tleTransferScript . Env.transferLogicEnv)
    transferSeizeSpendingScript <- asks (Env.tleIssuerScript . Env.transferLogicEnv)

    let
        hshMinting = C.hashScript $ C.PlutusScript C.plutusScriptVersion transferMintingScript
        credMinting = C.StakeCredentialByScript hshMinting

        hshSpending = C.hashScript $ C.PlutusScript C.plutusScriptVersion transferSpendingScript
        credSpending = C.StakeCredentialByScript hshSpending

        hshSeizeSpending = C.hashScript $ C.PlutusScript C.plutusScriptVersion transferSeizeSpendingScript
        credSeizeSpending = C.StakeCredentialByScript hshSeizeSpending

    addConwayStakeCredentialCertificate credMinting
    addConwayStakeCredentialCertificate credSpending
    addConwayStakeCredentialCertificate credSeizeSpending
