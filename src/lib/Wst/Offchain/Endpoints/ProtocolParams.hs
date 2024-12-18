{- This module contains the definition of the 'ProtocolParams' endpoints
-}
module Wst.Offchain.Endpoints.ProtocolParams (
  deployParamsTx
) where

import Cardano.Api qualified as C
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Convex.Class (MonadBlockchain)
import Convex.CoinSelection qualified
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)
import Wst.Offchain.BuildTx.ProtocolParams (mintProtocolParams)
import Wst.Offchain.Endpoints.Env (BuildTxEnv, BuildTxError)
import Wst.Offchain.Endpoints.Env qualified as Env

{-| Build a transaction that deploys the given protocol parameters
-}
deployParamsTx :: (MonadReader (BuildTxEnv era) m, MonadBlockchain era m, MonadError (BuildTxError era) m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era) => ProgrammableLogicGlobalParams -> m (C.Tx era)
deployParamsTx params = do
  fmap (Convex.CoinSelection.signBalancedTxBody [] . fst)
    $ Env.selectOperatorOutput
        >>= Env.balanceTxEnv . mintProtocolParams params . fst
