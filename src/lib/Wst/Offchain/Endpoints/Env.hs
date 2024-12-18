{-# LANGUAGE NamedFieldPuns #-}
{-| Transaction building environment
-}
module Wst.Offchain.Endpoints.Env(
  BuildTxEnv(..),
  BuildTxError(..),

  -- * Using the environment
  selectOperatorOutput,
  balanceTxEnv
) where

import Cardano.Api (UTxO)
import Cardano.Api qualified as C
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ask, asks)
import Convex.BuildTx (TxBuilder)
import Convex.Class (MonadBlockchain)
import Convex.CoinSelection qualified as CoinSelection
import Convex.Utils (mapError)
import Convex.Utxos (BalanceChanges)
import Convex.Utxos qualified as Utxos
import Convex.Wallet.Operator (Operator, Verification, operatorReturnOutput)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)

{-| Information needed to build transactions
-}
data BuildTxEnv era =
  BuildTxEnv
    { bteOperator :: Operator Verification
    , bteOperatorUtxos :: UTxO era -- ^ UTxOs owned by the operator, available for spending
    }

data BuildTxError era =
  OperatorNoUTxOs -- ^ The operator does not have any UTxOs
  | BalancingError (CoinSelection.BalanceTxError era)

{-| Select an output owned by the operator
-}
selectOperatorOutput :: (MonadReader (BuildTxEnv era) m, MonadError (BuildTxError era) m) => m (C.TxIn, C.TxOut C.CtxUTxO era)
selectOperatorOutput = asks (listToMaybe . Map.toList . C.unUTxO . bteOperatorUtxos) >>= \case
  Nothing -> throwError OperatorNoUTxOs
  Just k -> pure k

{-| Balance a transaction using the operator's funds and return output
-}
balanceTxEnv :: (MonadBlockchain era m, MonadReader (BuildTxEnv era) m, MonadError (BuildTxError era) m, C.IsBabbageBasedEra era) => TxBuilder era -> m (C.BalancedTxBody era, BalanceChanges)
balanceTxEnv txBuilder = do
  BuildTxEnv{bteOperatorUtxos, bteOperator} <- ask
  output <- operatorReturnOutput bteOperator
  mapError BalancingError (CoinSelection.balanceTx mempty output (Utxos.fromApiUtxo bteOperatorUtxos) txBuilder CoinSelection.TrailingChange)
