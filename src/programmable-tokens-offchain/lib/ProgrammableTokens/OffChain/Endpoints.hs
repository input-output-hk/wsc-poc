module ProgrammableTokens.OffChain.Endpoints(
  registerCip143PolicyTx
) where

import Cardano.Api qualified as C
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader)
import Convex.Class (MonadBlockchain, MonadUtxoQuery)
import Convex.CoinSelection (AsBalancingError, AsCoinSelectionError)
import Convex.CoinSelection qualified
import ProgrammableTokens.OffChain.BuildTx.Directory qualified as BuildTx
import ProgrammableTokens.OffChain.Env qualified as Env
import ProgrammableTokens.OffChain.Error (AsProgrammableTokensError)
import ProgrammableTokens.OffChain.Query qualified as Query

{-| Build a transaction that inserts a node into the directory
-}
registerCip143PolicyTx :: forall era env err m. (MonadReader env m, Env.HasOperatorEnv era env, Env.HasDirectoryEnv env, MonadBlockchain era m, MonadError err m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadUtxoQuery m, AsProgrammableTokensError err, AsBalancingError err era, AsCoinSelectionError err, Env.HasTransferLogicEnv env) => m (C.Tx era)
registerCip143PolicyTx = do
  headNode <- Query.registryNodeForInsertion @era
  paramsNode <- Query.globalParamsNode @era
  cborHexTxIn <- Query.issuanceCborHexUTxO @era
  (tx, _) <- Env.balanceTxEnv_ (BuildTx.insertDirectoryNode paramsNode cborHexTxIn headNode)
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)
