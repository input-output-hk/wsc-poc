{-# LANGUAGE NamedFieldPuns #-}
{-| Deploy the directory and global params
-}
module Wst.Offchain.Endpoints.Deployment(
  deployTx,
  deployBlacklistTx,
  insertNodeTx,
  issueProgrammableTokenTx,
  issueSmartTokensTx,
  transferSmartTokensTx,
  insertBlacklistNodeTx,
  blacklistCredentialTx,
  seizeCredentialAssetsTx,
) where

import Cardano.Api (Quantity)
import Cardano.Api.Shelley qualified as C
import Control.Monad (when)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain, MonadUtxoQuery)
import Convex.CoinSelection qualified
import Data.Foldable (maximumBy)
import Data.Function (on)
import SmartTokens.Core.Scripts (ScriptTarget (..))
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..))
import Wst.AppError (AppError)
import Wst.Offchain.BuildTx.DirectorySet (InsertNodeArgs (inaNewKey))
import Wst.Offchain.BuildTx.DirectorySet qualified as BuildTx
import Wst.Offchain.BuildTx.ProgrammableLogic qualified as BuildTx
import Wst.Offchain.BuildTx.ProtocolParams qualified as BuildTx
import Wst.Offchain.BuildTx.TransferLogic qualified as BuildTx
import Wst.Offchain.Env (DirectoryScriptRoot (..))
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query (UTxODat (..))
import Wst.Offchain.Query qualified as Query

{-| Build a transaction that deploys the directory and global params. Returns the
transaction and the 'TxIn' that was selected for the one-shot NFTs.
-}
deployTx :: (MonadReader env m, Env.HasOperatorEnv era env, MonadBlockchain era m, MonadError (AppError era) m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era) => ScriptTarget -> m (C.Tx era, DirectoryScriptRoot)
deployTx target = do
  (txi, _) <- Env.selectOperatorOutput
  opEnv <- asks Env.operatorEnv
  let root = DirectoryScriptRoot txi target
  (tx, _) <- Env.withEnv $ Env.withOperator opEnv $ Env.withDirectoryFor root
              $ Env.balanceTxEnv_
              $ BuildTx.mintProtocolParams >> BuildTx.initDirectorySet
  pure (Convex.CoinSelection.signBalancedTxBody [] tx, root)

{-| Build a transaction that inserts a node into the directory
-}
insertNodeTx :: forall era env m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasDirectoryEnv env, MonadBlockchain era m, MonadError (AppError era) m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadUtxoQuery m) => InsertNodeArgs -> m (C.Tx era)
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
issueProgrammableTokenTx :: forall era env m.
  ( MonadReader env m
  , Env.HasOperatorEnv era env
  , Env.HasDirectoryEnv env
  , Env.HasTransferLogicEnv env
  , MonadBlockchain era m
  , MonadError (AppError era) m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
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

deployBlacklistTx :: (MonadReader env m, Env.HasOperatorEnv era env, MonadBlockchain era m, MonadError (AppError era) m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, Env.HasDirectoryEnv env) => m (C.Tx era)
deployBlacklistTx = do
  opEnv <- asks Env.operatorEnv
  dirEnv <- asks Env.directoryEnv
  (tx, _) <- Env.withEnv $ Env.withOperator opEnv $ Env.withDirectory dirEnv $ Env.withTransferFromOperator (error "issueProgramableTokenTx: target")
              $ Env.balanceTxEnv_ BuildTx.initBlacklist
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)

insertBlacklistNodeTx :: forall era env m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasTransferLogicEnv env, MonadBlockchain era m, MonadError (AppError era) m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadUtxoQuery m) => C.PaymentCredential -> m (C.Tx era)
insertBlacklistNodeTx cred = do
  blacklist <- Query.blacklistNodes @era
  (tx, _)  <- Env.balanceTxEnv_ (BuildTx.insertBlacklistNode cred blacklist)
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)

{-| Build a transaction that issues a progammable token
-}
issueSmartTokensTx :: forall era env m.
  ( MonadReader env m
  , Env.HasOperatorEnv era env
  , Env.HasDirectoryEnv env
  , Env.HasTransferLogicEnv env
  , MonadBlockchain era m
  , MonadError (AppError era) m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
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
transferSmartTokensTx :: forall era env m.
  ( MonadReader env m
  , Env.HasOperatorEnv era env
  , Env.HasDirectoryEnv env
  , Env.HasTransferLogicEnv env
  , MonadBlockchain era m
  , MonadError (AppError era) m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  )
  => C.AssetId -- ^ AssetId to transfer
  -> Quantity -- ^ Amount of tokens to be minted
  -> C.PaymentCredential -- ^ Destination credential
  -> m (C.Tx era)
transferSmartTokensTx assetId quantity destCred = do
  directory <- Query.registryNodes @era
  blacklist <- Query.blacklistNodes @era
  userOutputsAtProgrammable <- Env.operatorPaymentCredential >>= Query.userProgrammableOutputs
  paramsTxIn <- Query.globalParamsNode @era
  (tx, _) <- Env.balanceTxEnv_ $ do
    BuildTx.transferSmartTokens paramsTxIn blacklist directory userOutputsAtProgrammable (assetId, quantity) destCred
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)

blacklistCredentialTx :: forall era env m.
  ( MonadReader env m
  , Env.HasOperatorEnv era env
  , Env.HasTransferLogicEnv env
  , MonadBlockchain era m
  , MonadError (AppError era) m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  )
  => C.PaymentCredential -- ^ Source/User credential
  -> m (C.Tx era)
blacklistCredentialTx sanctionedCred = do
  blacklist <- Query.blacklistNodes @era
  (tx, _) <- Env.balanceTxEnv_ $ do
    BuildTx.insertBlacklistNode sanctionedCred blacklist
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)

seizeCredentialAssetsTx :: forall era env m.
  ( MonadReader env m
  , Env.HasOperatorEnv era env
  , Env.HasTransferLogicEnv env
  , Env.HasDirectoryEnv env
  , MonadBlockchain era m
  , MonadError (AppError era) m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  )
  => C.PaymentCredential -- ^ Source/User credential
  -> m (C.Tx era)
seizeCredentialAssetsTx sanctionedCred = do
  opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
  directory <- Query.registryNodes @era
  seizeTxo <- head <$> Query.userProgrammableOutputs sanctionedCred
  paramsTxIn <- Query.globalParamsNode @era
  (tx, _) <- Env.balanceTxEnv_ $ do
    BuildTx.seizeSmartTokens paramsTxIn seizeTxo (C.PaymentCredentialByKey opPkh) directory
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)
