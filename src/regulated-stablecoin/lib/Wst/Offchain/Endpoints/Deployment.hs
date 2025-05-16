{-# LANGUAGE NamedFieldPuns #-}
{-| Deploy the directory and global params
-}
module Wst.Offchain.Endpoints.Deployment(
  deployTx,
  deployFullTx,
  deployBlacklistTx,
  insertNodeTx,
  issueProgrammableTokenTx,
  issueSmartTokensTx,
  transferSmartTokensTx,
  insertBlacklistNodeTx,
  removeBlacklistNodeTx,
  seizeCredentialAssetsTx,
) where

import Cardano.Api (Quantity)
import Cardano.Api.Shelley qualified as C
import Control.Monad (when)
import Control.Monad.Error.Lens (throwing_)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain, MonadUtxoQuery)
import Convex.CoinSelection (AsBalancingError, AsCoinSelectionError)
import Convex.CoinSelection qualified
import Data.Foldable (maximumBy)
import Data.Function (on)
import GHC.IsList (IsList (..))
import SmartTokens.Core.Scripts (ScriptTarget (..))
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..))
import Wst.AppError (AsProgrammableTokensError (..))
import Wst.Offchain.BuildTx.DirectorySet (InsertNodeArgs (inaNewKey))
import Wst.Offchain.BuildTx.DirectorySet qualified as BuildTx
import Wst.Offchain.BuildTx.Failing (BlacklistedTransferPolicy,
                                     balanceTxEnvFailing)
import Wst.Offchain.BuildTx.ProgrammableLogic qualified as BuildTx
import Wst.Offchain.BuildTx.ProtocolParams qualified as BuildTx
import Wst.Offchain.BuildTx.TransferLogic (BlacklistReason)
import Wst.Offchain.BuildTx.TransferLogic qualified as BuildTx
import Wst.Offchain.Env (DirectoryScriptRoot (..))
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query (UTxODat (..))
import Wst.Offchain.Query qualified as Query

{-| Build a transaction that deploys the directory and global params. Returns the
transaction and the 'TxIn' that was selected for the one-shot NFTs.
-}
deployTx :: (MonadReader env m, Env.HasOperatorEnv era env, MonadBlockchain era m, MonadError err m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, AsProgrammableTokensError err, AsCoinSelectionError err, AsBalancingError err era) => ScriptTarget -> m (C.Tx era, DirectoryScriptRoot)
deployTx target = do
  (txi, _) <- Env.selectOperatorOutput
  opEnv <- asks Env.operatorEnv
  let root = DirectoryScriptRoot txi target
  (tx, _) <- Env.withEnv $ Env.withOperator opEnv $ Env.withDirectoryFor root
              $ Env.balanceTxEnv_
              $ BuildTx.mintProtocolParams
                >> BuildTx.initDirectorySet
                >> BuildTx.registerProgrammableGlobalScript
  pure (Convex.CoinSelection.signBalancedTxBody [] tx, root)

{-| Build a transaction that deploys the directory and global params as well as
the relevant stablecoin transfer logic scripts and registrations. Returns the
transaction and the 'TxIn' that was selected for the one-shot NFTs.
-}
deployFullTx :: (MonadReader env m, Env.HasOperatorEnv era env, MonadBlockchain era m, MonadError err m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, AsProgrammableTokensError err, AsCoinSelectionError err, AsBalancingError err era) => ScriptTarget -> m (C.Tx era, DirectoryScriptRoot)
deployFullTx target = do
  (txi, _) <- Env.selectOperatorOutput
  opEnv <- asks Env.operatorEnv
  let root = DirectoryScriptRoot txi target
  (tx, _) <- Env.withEnv $ Env.withOperator opEnv $ Env.withDirectoryFor root $ Env.withTransferFromOperator
              $ Env.balanceTxEnv_
              $ BuildTx.mintProtocolParams
                >> BuildTx.initDirectorySet
                >> BuildTx.initBlacklist
                >> BuildTx.registerProgrammableGlobalScript
                >> BuildTx.registerTransferScripts

  pure (Convex.CoinSelection.signBalancedTxBody [] tx, root)

{-| Build a transaction that inserts a node into the directory
-}
insertNodeTx :: forall era env err m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasDirectoryEnv env, MonadBlockchain era m, MonadError err m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadUtxoQuery m, AsProgrammableTokensError err, AsBalancingError err era, AsCoinSelectionError err) => InsertNodeArgs -> m (C.Tx era)
insertNodeTx args = do
  -- 1. Find the head node
  directoryList <- Query.registryNodes @era
  -- FIXME: Error handling. And how can we actually identify the head node if the query returns more than one?
  let headNode@UTxODat{uDatum = dirNodeDat} =
        maximumBy (compare `on` (key . uDatum)) $
          filter ((<= inaNewKey args) . key . uDatum) directoryList
  when (key dirNodeDat == inaNewKey args) $ error "Node already exists"

  -- 2. Find the global parameter node
  paramsNode <- Query.globalParamsNode @era
  (tx, _) <- Env.balanceTxEnv_ (BuildTx.insertDirectoryNode paramsNode headNode args)
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)

{-| Build a transaction that issues a progammable token
-}
issueProgrammableTokenTx :: forall era env err m.
  ( MonadReader env m
  , Env.HasOperatorEnv era env
  , Env.HasDirectoryEnv env
  , Env.HasTransferLogicEnv env
  , MonadBlockchain era m
  , MonadError err m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  , AsProgrammableTokensError err
  , AsCoinSelectionError err
  , AsBalancingError err era
  )
  => C.AssetName -- ^ Name of the asset
  -> Quantity -- ^ Amount of tokens to be minted
  -> m (C.Tx era)
issueProgrammableTokenTx assetName quantity = do
  directory <- Query.registryNodes @era
  paramsNode <- Query.globalParamsNode @era
  Env.TransferLogicEnv{Env.tleMintingScript} <- asks Env.transferLogicEnv
  (tx, _) <- Env.balanceTxEnv_ $ do
    polId <- BuildTx.issueProgrammableToken paramsNode (assetName, quantity) directory
    Env.operatorPaymentCredential
      >>= BuildTx.paySmartTokensToDestination (assetName, quantity) polId
    let hsh = C.hashScript (C.PlutusScript C.plutusScriptVersion tleMintingScript)
    BuildTx.addScriptWithdrawal hsh 0 $ BuildTx.buildScriptWitness tleMintingScript C.NoScriptDatumForStake ()
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)

deployBlacklistTx :: (MonadReader env m, Env.HasOperatorEnv era env, MonadBlockchain era m, MonadError err m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, Env.HasDirectoryEnv env, AsCoinSelectionError err, AsBalancingError err era) => m (C.Tx era)
deployBlacklistTx = do
  opEnv <- asks Env.operatorEnv
  dirEnv <- asks Env.directoryEnv
  (tx, _) <- Env.withEnv $ Env.withOperator opEnv $ Env.withDirectory dirEnv $ Env.withTransferFromOperator
              $ Env.balanceTxEnv_ BuildTx.initBlacklist
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)

insertBlacklistNodeTx :: forall era env err m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasTransferLogicEnv env, MonadBlockchain era m, MonadError err m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadUtxoQuery m, AsCoinSelectionError err, AsBalancingError err era, AsProgrammableTokensError err) => BlacklistReason -> C.PaymentCredential -> m (C.Tx era)
insertBlacklistNodeTx reason cred = do
  blacklist <- Query.blacklistNodes @era
  (tx, _)  <- Env.balanceTxEnv_ (BuildTx.insertBlacklistNode reason cred blacklist)
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)

removeBlacklistNodeTx :: forall era env err m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasTransferLogicEnv env, MonadBlockchain era m, MonadError err m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadUtxoQuery m, AsCoinSelectionError err, AsBalancingError err era, AsProgrammableTokensError err) => C.PaymentCredential -> m (C.Tx era)
removeBlacklistNodeTx cred = do
  blacklist <- Query.blacklistNodes @era
  (tx, _)  <- Env.balanceTxEnv_ (BuildTx.removeBlacklistNode cred blacklist)
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)


{-| Build a transaction that issues a progammable token
-}
issueSmartTokensTx :: forall era env err m.
  ( MonadReader env m
  , Env.HasOperatorEnv era env
  , Env.HasDirectoryEnv env
  , Env.HasTransferLogicEnv env
  , MonadBlockchain era m
  , MonadError err m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  , AsProgrammableTokensError err
  , AsCoinSelectionError err
  , AsBalancingError err era
  )
  => C.AssetName -- ^ Name of the asset
  -> Quantity -- ^ Amount of tokens to be minted
  -> C.PaymentCredential -- ^ Destination credential
  -> m (C.Tx era, C.AssetId)
issueSmartTokensTx assetName quantity destinationCred = do
  directory <- Query.registryNodes @era
  paramsNode <- Query.globalParamsNode @era
  ((tx, _), aid) <- Env.balanceTxEnv $ do
    BuildTx.issueSmartTokens paramsNode (assetName, quantity) directory destinationCred
  pure (Convex.CoinSelection.signBalancedTxBody [] tx, aid)

{-| Build a transaction that issues a progammable token
-}
transferSmartTokensTx :: forall era env err m.
  ( MonadReader env m
  , Env.HasOperatorEnv era env
  , Env.HasDirectoryEnv env
  , Env.HasTransferLogicEnv env
  , MonadBlockchain era m
  , MonadError err m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  , AsProgrammableTokensError err
  , AsCoinSelectionError err
  , AsBalancingError err era
  )
  => BlacklistedTransferPolicy
  -> C.AssetId -- ^ AssetId to transfer
  -> Quantity -- ^ Amount of tokens to be minted
  -> C.PaymentCredential -- ^ Destination credential
  -> m (C.Tx era)
transferSmartTokensTx policy assetId quantity destCred = do
  directory <- Query.registryNodes @era
  blacklist <- Query.blacklistNodes @era
  userOutputsAtProgrammable <- Env.operatorPaymentCredential >>= Query.userProgrammableOutputs
  paramsTxIn <- Query.globalParamsNode @era
  (tx, _) <- balanceTxEnvFailing policy $ do
    BuildTx.transferSmartTokens paramsTxIn blacklist directory userOutputsAtProgrammable (assetId, quantity) destCred
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)


seizeCredentialAssetsTx :: forall era env err m.
  ( MonadReader env m
  , Env.HasOperatorEnv era env
  , Env.HasTransferLogicEnv env
  , Env.HasDirectoryEnv env
  , MonadBlockchain era m
  , MonadError err m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  , AsProgrammableTokensError err
  , AsBalancingError err era
  , AsCoinSelectionError err
  )
  => BuildTx.SeizeReason
  -> C.PaymentCredential -- ^ Source/User credential
  -> m (C.Tx era)
seizeCredentialAssetsTx reason sanctionedCred = do
  opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
  directory <- Query.registryNodes @era
  let getTxOutValue (C.TxOut _a v _d _r) = v
      -- simple fold to choose the UTxO with the most total assets
      nonAda v = foldl (\acc -> \case
                    (C.AdaAssetId,  _) -> acc
                    (_aid,  q) -> acc + q
                  ) 0 $ toList v

      getNonAdaTokens = nonAda . C.txOutValueToValue . getTxOutValue . uOut
  seizeTxo <- maximumBy (compare `on` getNonAdaTokens) <$> Query.userProgrammableOutputs sanctionedCred
  when (getNonAdaTokens seizeTxo == 0) $
    throwing_ _NoTokensToSeize
  paramsTxIn <- Query.globalParamsNode @era
  (tx, _) <- Env.balanceTxEnv_ $ do
    BuildTx.seizeSmartTokens reason paramsTxIn seizeTxo (C.PaymentCredentialByKey opPkh) directory
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)
