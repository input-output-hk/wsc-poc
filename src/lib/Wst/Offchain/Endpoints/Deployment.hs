{-| Deploy the directory and global params
-}
module Wst.Offchain.Endpoints.Deployment(
  deployTx,
  insertNodeTx,
  issueProgrammableTokenTx
) where

import Cardano.Api (Quantity)
import Cardano.Api.Shelley qualified as C
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain, MonadUtxoQuery)
import Convex.CoinSelection qualified
import Wst.Offchain.BuildTx.DirectorySet (InsertNodeArgs)
import Wst.Offchain.BuildTx.DirectorySet qualified as BuildTx
import Wst.Offchain.BuildTx.ProgrammableLogic qualified as BuildTx
import Wst.Offchain.BuildTx.ProtocolParams qualified as BuildTx
import Wst.Offchain.Env (BuildTxError)
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query qualified as Query

{-| Build a transaction that deploys the directory and global params. Returns the
transaction and the 'TxIn' that was selected for the one-shot NFTs.
-}
deployTx :: (MonadReader env m, Env.HasOperatorEnv era env, MonadBlockchain era m, MonadError (BuildTxError era) m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era) => m (C.Tx era, C.TxIn)
deployTx = do
  (txi, _) <- Env.selectOperatorOutput
  (tx, _) <- Env.withDirectoryFor txi
              $ Env.balanceTxEnv
              $ BuildTx.mintProtocolParams >> BuildTx.initDirectorySet
  pure (Convex.CoinSelection.signBalancedTxBody [] tx, txi)

{-| Build a transaction that inserts a node into the directory
-}
insertNodeTx :: forall era env m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasDirectoryEnv env, MonadBlockchain era m, MonadError (BuildTxError era) m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadUtxoQuery m) => InsertNodeArgs -> m (C.Tx era)
insertNodeTx args = do
  -- 1. Find the head node
  -- FIXME: Error handling. And how can we actually identify the head node if the query returns more than one?
  headNode <- head <$> Query.registryNodes @era

  -- 2. Find the global parameter node
  paramsNode <- head <$> Query.globalParamsNode @era
  (tx, _) <- Env.balanceTxEnv (BuildTx.insertDirectoryNode paramsNode headNode args)
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)

{-| Build a transaction that issues a progammable token
-}
issueProgrammableTokenTx :: forall era env m.
  ( MonadReader env m
  , Env.HasOperatorEnv era env
  , Env.HasDirectoryEnv env
  , MonadBlockchain era m
  , MonadError (BuildTxError era) m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  )
  => BuildTx.IssueNewTokenArgs -- ^ credentials of the token
  -> C.AssetName -- ^ Name of the asset
  -> Quantity -- ^ Amount of tokens to be minted
  -> m (C.Tx era)
issueProgrammableTokenTx issueTokenArgs assetName quantity = do
  directory <- Query.registryNodes @era
  paramsNode <- head <$> Query.globalParamsNode @era
  (tx, _) <- Env.balanceTxEnv $ do
    _ <- BuildTx.issueProgrammableToken paramsNode (assetName, quantity) issueTokenArgs directory

    let hsh = C.hashScript (C.PlutusScript C.plutusScriptVersion $ BuildTx.intaMintingLogic issueTokenArgs)
    BuildTx.addScriptWithdrawal hsh 0 $ BuildTx.buildScriptWitness (BuildTx.intaMintingLogic issueTokenArgs) C.NoScriptDatumForStake ()
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)
