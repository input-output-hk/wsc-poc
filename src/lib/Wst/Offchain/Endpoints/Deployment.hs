{-| Deploy the directory and global params
-}
module Wst.Offchain.Endpoints.Deployment(
  deployTx
) where

import Cardano.Api.Shelley qualified as C
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Convex.Class (MonadBlockchain)
import Convex.CoinSelection qualified
import Wst.Offchain.BuildTx.DirectorySet qualified as BuildTx
import Wst.Offchain.BuildTx.ProtocolParams qualified as BuildTx
import Wst.Offchain.Env (BuildTxError)
import Wst.Offchain.Env qualified as Env

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
