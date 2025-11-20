{-# LANGUAGE NamedFieldPuns #-}
{-| Deploy the directory and global params
-}
module Wst.Offchain.Endpoints.Deployment(
  deployFullTx,
  deployBlacklistTx,
  issueSmartTokensTx,
  transferSmartTokensTx,
  insertBlacklistNodeTx,
  removeBlacklistNodeTx,
  seizeCredentialAssetsTx,
) where

import Cardano.Api (Quantity)
import Cardano.Api qualified as C
import Control.Monad (when)
import Control.Monad.Error.Lens (throwing_)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader, asks)
import Convex.Class (MonadBlockchain, MonadUtxoQuery)
import Convex.CoinSelection (AsBalancingError, AsCoinSelectionError)
import Convex.CoinSelection qualified
import Data.Foldable (maximumBy)
import Data.Function (on)
import GHC.IsList (IsList (..))
import ProgrammableTokens.OffChain.BuildTx qualified as BuildTx
import ProgrammableTokens.OffChain.Env.Operator qualified as Env
import ProgrammableTokens.OffChain.Error (AsProgrammableTokensError (..))
import ProgrammableTokens.OffChain.Query qualified as Query
import SmartTokens.Core.Scripts (ScriptTarget (..))
import Wst.AppError (AsRegulatedStablecoinError (..))
import Wst.Offchain.BuildTx.Failing (BlacklistedTransferPolicy,
                                     balanceTxEnvFailing)
import Wst.Offchain.BuildTx.TransferLogic (BlacklistReason)
import Wst.Offchain.BuildTx.TransferLogic qualified as BuildTx
import Wst.Offchain.Env (DirectoryScriptRoot (..))
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query (UTxODat (..))
import Wst.Offchain.Query qualified as Query

{-| Build a transaction that deploys the directory and global params as well as
the relevant stablecoin transfer logic scripts and registrations. Returns the
transaction and the 'TxIn' that was selected for the one-shot NFTs.
-}
deployFullTx :: forall env era err m. (MonadReader env m, C.IsConwayBasedEra era, Env.HasOperatorEnv era env, MonadBlockchain era m, MonadError err m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, AsProgrammableTokensError err, AsCoinSelectionError err, AsBalancingError err era) => ScriptTarget -> m (C.Tx era, DirectoryScriptRoot)
deployFullTx target = do
  ((txi, _), (issuanceCborHexTxIn, _)) <- Env.selectTwoOperatorOutputs @env @_ @era
  opEnv <- asks (Env.operatorEnv @era)
  let root = DirectoryScriptRoot txi issuanceCborHexTxIn target
  (tx, _) <- Env.withEnv $ Env.withOperator opEnv $ Env.withDirectoryFor root $ Env.withTransferFromOperator @era
              $ Env.balanceDeployTxEnv_
              $ BuildTx.mintProtocolParams
                >> BuildTx.initDirectorySet
                >> BuildTx.initBlacklist
                >> BuildTx.registerProgrammableGlobalScript
                >> BuildTx.registerTransferScripts

  pure (Convex.CoinSelection.signBalancedTxBody [] tx, root)

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
  directory <- Query.registryNodeForReferenceOrInsertion @era
  paramsNode <- Query.globalParamsNode @era
  cborHexTxIn <- Query.issuanceCborHexUTxO @era
  ((tx, _), aid) <- Env.balanceTxEnv $ do
    BuildTx.issueSmartTokens paramsNode cborHexTxIn (assetName, quantity) directory destinationCred
  pure (Convex.CoinSelection.signBalancedTxBody [] tx, aid)

deployBlacklistTx :: forall env era err m. (MonadReader env m, Env.HasOperatorEnv era env, MonadBlockchain era m, MonadError err m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, Env.HasDirectoryEnv env, AsCoinSelectionError err, AsBalancingError err era) => m (C.Tx era)
deployBlacklistTx = do
  opEnv <- asks (Env.operatorEnv @era)
  dirEnv <- asks Env.directoryEnv
  (tx, _) <- Env.withEnv $ Env.withOperator opEnv $ Env.withDirectory dirEnv $ Env.withTransferFromOperator @era
              $ Env.balanceTxEnv_ BuildTx.initBlacklist
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)

insertBlacklistNodeTx :: forall era env err m. (MonadReader env m, Env.HasOperatorEnv era env, MonadBlockchain era m, MonadError err m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadUtxoQuery m, AsCoinSelectionError err, AsBalancingError err era, AsRegulatedStablecoinError err, Env.HasBlacklistEnv env) => BlacklistReason -> C.PaymentCredential -> m (C.Tx era)
insertBlacklistNodeTx reason cred = do
  blacklist <- Query.blacklistNodes @era
  (tx, _)  <- Env.balanceTxEnv_ (BuildTx.insertBlacklistNode reason cred blacklist)
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)

removeBlacklistNodeTx :: forall era env err m. (MonadReader env m, Env.HasOperatorEnv era env, MonadBlockchain era m, MonadError err m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadUtxoQuery m, AsCoinSelectionError err, AsBalancingError err era, AsRegulatedStablecoinError err, Env.HasBlacklistEnv env) => C.PaymentCredential -> m (C.Tx era)
removeBlacklistNodeTx cred = do
  blacklist <- Query.blacklistNodes @era
  (tx, _)  <- Env.balanceTxEnv_ (BuildTx.removeBlacklistNode cred blacklist)
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)


{-| Build a transaction that issues a progammable token
-}
transferSmartTokensTx :: forall era env err m.
  ( MonadReader env m
  , Env.HasOperatorEnv era env
  , Env.HasDirectoryEnv env
  , Env.HasTransferLogicEnv env
  , Env.HasBlacklistEnv env
  , MonadBlockchain era m
  , MonadError err m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  , AsProgrammableTokensError err
  , AsCoinSelectionError err
  , AsBalancingError err era
  , AsRegulatedStablecoinError err
  )
  => BlacklistedTransferPolicy
  -> C.AssetId -- ^ AssetId to transfer
  -> Quantity -- ^ Amount of tokens to be minted
  -> C.PaymentCredential -- ^ Destination credential
  -> m (C.Tx era)
transferSmartTokensTx policy assetId quantity destCred = do
  directory <- Query.registryNodeForReference @era
  blacklist <- Query.blacklistNodes @era
  userOutputsAtProgrammable <- Env.operatorPaymentCredential @_ @era >>= Query.userProgrammableOutputs
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
  , AsRegulatedStablecoinError err
  )
  => BuildTx.SeizeReason
  -> C.PaymentCredential -- ^ Source/User credential
  -> m (C.Tx era)
seizeCredentialAssetsTx reason sanctionedCred = do
  opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv @era)
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
