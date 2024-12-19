{-| Deploy the directory and global params
-}
module Wst.Offchain.Endpoints.Deployment(
  deployTx
) where

import Cardano.Api (PlutusScript, PlutusScriptV3)
import Cardano.Api.Shelley qualified as C
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Convex.Class (MonadBlockchain)
import Convex.CoinSelection qualified
import Convex.PlutusLedger.V1 (transCredential, transPolicyId)
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (..))
import Wst.Offchain.BuildTx.DirectorySet (initDirectorySet)
import Wst.Offchain.BuildTx.ProtocolParams (mintProtocolParams)
import Wst.Offchain.Env (BuildTxError, OperatorEnv)
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Scripts (directoryNodeMintingScript,
                             directoryNodeSpendingScript,
                             programmableLogicBaseScript,
                             programmableLogicGlobalScript,
                             protocolParamsMintingScript, scriptPolicyIdV3)

{-| Build a transaction that deploys the directory and global params. Returns the
transaction and the 'TxIn' that was selected for the one-shot NFTs.
-}
deployTx :: (MonadReader (OperatorEnv era) m, MonadBlockchain era m, MonadError (BuildTxError era) m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era) => m (C.Tx era, C.TxIn)
deployTx = do
  (txi, _) <- Env.selectOperatorOutput
  let scripts = Env.directoryEnv txi
  (tx, _) <- Env.balanceTxEnv $ do
          mintProtocolParams (Env.globalParams scripts) txi
          initDirectorySet (Env.protocolParamsPolicyId scripts) txi
  pure (Convex.CoinSelection.signBalancedTxBody [] tx, txi)
