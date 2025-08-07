{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module ProgrammableTokens.OffChain.Endpoints
  (
    -- * Policy registration
    registerCip143PolicyTx,
    registerCip143PolicyTransferScripts,

    -- * CIP deployment
    deployCip143RegistryTx,
    deployIssuanceCborHex,
    frackUtxosTx
  )
where

import Cardano.Api qualified as C
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader, asks)
import Convex.Class (MonadBlockchain, MonadUtxoQuery)
import Convex.CoinSelection (AsBalancingError, AsCoinSelectionError)
import Convex.CoinSelection qualified
import ProgrammableTokens.OffChain.BuildTx qualified as BuildTx
import ProgrammableTokens.OffChain.Env (DirectoryScriptRoot (..))
import ProgrammableTokens.OffChain.Env qualified as Env
import ProgrammableTokens.OffChain.Error (AsProgrammableTokensError)
import ProgrammableTokens.OffChain.Query qualified as Query
import SmartTokens.Core.Scripts (ScriptTarget (..))

{-| Build a transaction that fractionalizes the operators UTxOs.
-}
frackUtxosTx :: (MonadReader env m, Env.HasOperatorEnv era env, MonadBlockchain era m, MonadError err m, C.IsBabbageBasedEra era, AsBalancingError err era, AsCoinSelectionError err) => m (C.Tx era)
frackUtxosTx = do
  (tx, _) <- Env.balanceTxEnv_ $ BuildTx.frackUTxOs
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)

{-| Build a transaction that deploys the cbox hex outputs
-}
deployIssuanceCborHex :: forall era env err m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasDirectoryEnv env, MonadBlockchain era m, MonadError err m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, AsCoinSelectionError err, AsBalancingError err era) => m (C.Tx era)
deployIssuanceCborHex = do
  (tx, _) <- Env.balanceTxEnv_ BuildTx.mintIssuanceCborHexNFT
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)

-- | Build a transaction that deploys the directory and global params. Returns the
-- transaction and the 'TxIn' that was selected for the one-shot NFTs.
deployCip143RegistryTx :: (MonadReader env m, Env.HasOperatorEnv era env, MonadBlockchain era m, MonadError err m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, AsProgrammableTokensError err, AsCoinSelectionError err, AsBalancingError err era) => ScriptTarget -> m (C.Tx era, DirectoryScriptRoot)
deployCip143RegistryTx target = do
  ((txi, _), (issuanceCborHexTxIn_, _)) <- Env.selectTwoOperatorOutputs
  opEnv <- asks Env.operatorEnv
  let root = DirectoryScriptRoot txi issuanceCborHexTxIn_ target
  (tx, _) <-
    Env.withEnv (Env.directoryOperatorEnv (Env.mkDirectoryEnv root) opEnv) $
      Env.balanceDeployTxEnv_ $
        BuildTx.mintProtocolParams
          >> BuildTx.initDirectorySet
          >> BuildTx.registerProgrammableGlobalScript
  pure (Convex.CoinSelection.signBalancedTxBody [] tx, root)

-- | Build a transaction that inserts a node into the directory
registerCip143PolicyTx :: forall era env err m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasDirectoryEnv env, MonadBlockchain era m, MonadError err m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadUtxoQuery m, AsProgrammableTokensError err, AsBalancingError err era, AsCoinSelectionError err, Env.HasTransferLogicEnv env) => m (C.Tx era)
registerCip143PolicyTx = do
  headNode <- Query.registryNodeForReferenceOrInsertion @era
  paramsNode <- Query.globalParamsNode @era
  cborHexTxIn <- Query.issuanceCborHexUTxO @era
  (tx, _) <- Env.balanceTxEnv_ $ do
    let assetName = "TEST"
        quantity  = 1
    policyId <- BuildTx.issueProgrammableToken paramsNode cborHexTxIn (assetName, quantity) headNode
    Env.operatorPaymentCredential
      >>= BuildTx.paySmartTokensToDestination (assetName, quantity) policyId
    BuildTx.invokeMintingStakeScript
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)

registerCip143PolicyTransferScripts :: forall era env err m.
  ( MonadReader env m
  , Env.HasOperatorEnv era env
  , MonadBlockchain era m
  , MonadError err m
  , AsBalancingError err era
  , AsCoinSelectionError err
  , Env.HasTransferLogicEnv env
  , C.IsConwayBasedEra era
  )
  => m (C.Tx era)
registerCip143PolicyTransferScripts = do
  opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv)
  (tx, _) <- Env.balanceTxEnv_ $ do
    BuildTx.registerTransferScripts opPkh
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)
