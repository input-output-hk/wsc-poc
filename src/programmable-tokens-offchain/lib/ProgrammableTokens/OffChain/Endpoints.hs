module ProgrammableTokens.OffChain.Endpoints
  (
    -- * Policy registration
    registerCip143PolicyTx,
    registerCip143PolicyTransferScripts,

    -- * CIP deployment
    deployCip143RegistryTx,
    deployIssuanceCborHex,
    frackUtxosTx,

    -- * Transfer
    transferTokens
  )
where

import Cardano.Api qualified as C
import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain, MonadUtxoQuery)
import Convex.CoinSelection (AsBalancingError, AsCoinSelectionError)
import Convex.CoinSelection qualified
import Convex.PlutusLedger.V1 (transPolicyId)
import PlutusLedgerApi.V3 qualified as PV3
import ProgrammableTokens.OffChain.BuildTx qualified as BuildTx
import ProgrammableTokens.OffChain.Env (DirectoryScriptRoot (..),
                                        programmableTokenPolicyId)
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
deployCip143RegistryTx :: forall era env err m.
  ( MonadReader env m
  , Env.HasOperatorEnv era env
  , MonadBlockchain era m
  , MonadError err m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , AsProgrammableTokensError err
  , AsCoinSelectionError err
  , AsBalancingError err era
  )
  => ScriptTarget
  -> m (C.Tx era, DirectoryScriptRoot)
deployCip143RegistryTx target = do
  ((txi, _), (issuanceCborHexTxIn_, _)) <- Env.selectTwoOperatorOutputs @env @_ @era
  opEnv <- asks (Env.operatorEnv @era)
  let root = DirectoryScriptRoot txi issuanceCborHexTxIn_ target
  (tx, _) <-
    flip Env.runReaderT (Env.addEnv (Env.mkDirectoryEnv root) $ Env.singleton opEnv) $
      Env.balanceDeployTxEnv_ $
        BuildTx.mintProtocolParams
          >> BuildTx.initDirectorySet
          >> BuildTx.registerProgrammableGlobalScript
          >> BuildTx.mintIssuanceCborHexNFT
  pure (Convex.CoinSelection.signBalancedTxBody [] tx, root)

-- | Build a transaction that inserts a node into the directory
registerCip143PolicyTx :: forall era env redeemer err m.
  ( MonadReader env m
  , Env.HasOperatorEnv era env
  , Env.HasDirectoryEnv env
  , MonadBlockchain era m
  , MonadError err m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  , AsProgrammableTokensError err
  , AsBalancingError err era
  , AsCoinSelectionError err
  , Env.HasTransferLogicEnv env
  , PV3.ToData redeemer
  )
  => C.AssetName
  -> C.Quantity
  -> redeemer
  -> m (C.Tx era)
registerCip143PolicyTx assetName quantity redeemer = do
  cborHexTxIn <- Query.issuanceCborHexUTxO @era
  headNode <- Query.registryNodeForReferenceOrInsertion @era
  paramsNode <- Query.globalParamsNode @era
  (tx, _) <- Env.balanceTxEnv_ $ do
    policyId <- BuildTx.issueProgrammableToken paramsNode cborHexTxIn (assetName, quantity) headNode
    Env.operatorPaymentCredential @env @era
      >>= BuildTx.paySmartTokensToDestination (assetName, quantity) policyId
    BuildTx.invokeMintingStakeScript redeemer
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)

-- | Buld a transaction that registers the transfer scripts
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
  (tx, _) <- Env.balanceTxEnv_ $ do
    BuildTx.registerTransferScripts
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)

{-| Build a transaction that transfers the programmable tokens to the target address
-}
transferTokens :: forall era env redeemer err m.
  ( MonadReader env m
  , Env.HasOperatorEnv era env
  , MonadBlockchain era m
  , MonadError err m
  , AsBalancingError err era
  , AsCoinSelectionError err
  , Env.HasTransferLogicEnv env
  , Env.HasDirectoryEnv env
  , MonadUtxoQuery m
  , AsProgrammableTokensError err
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , C.IsConwayBasedEra era
  , PV3.ToData redeemer
  )
  => C.AssetName -> C.Quantity -> C.PaymentCredential -> redeemer -> m (C.Tx era)
transferTokens assetName quantity target redeemer = do
  headNode <- Query.registryNodeForReference @era
  paramsNode <- Query.globalParamsNode @era
  policyId <- programmableTokenPolicyId
  op <- Env.operatorPaymentCredential @env @era
  opPkh <- asks (fst . Env.bteOperator . Env.operatorEnv @era)
  (inputs, change) <- Query.selectProgammableOutputsFor op assetName quantity
  (tx, _) <- Env.balanceTxEnv_ $ do
    BuildTx.transferProgrammableToken paramsNode inputs (transPolicyId policyId) headNode
    BuildTx.paySmartTokensToDestination (assetName, quantity) policyId target
    BuildTx.invokeTransferStakeScript redeemer
    when (change > 0) $ do
      Env.operatorPaymentCredential @env @era >>= BuildTx.paySmartTokensToDestination (assetName, change) policyId
    BuildTx.addRequiredSignature opPkh
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)
