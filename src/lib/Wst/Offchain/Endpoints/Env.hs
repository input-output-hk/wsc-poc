{-# LANGUAGE NamedFieldPuns #-}
{-| Transaction building environment
-}
module Wst.Offchain.Endpoints.Env(
  BuildTxEnv(..),
  loadEnv,
  BuildTxError(..),

  -- * Using the environment
  selectOperatorOutput,
  balanceTxEnv
) where

import Cardano.Api (UTxO)
import Cardano.Api qualified as C
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ask, asks)
import Convex.BuildTx (BuildTxT)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain, MonadUtxoQuery (..),
                     queryProtocolParameters, utxosByPaymentCredential)
import Convex.CoinSelection qualified as CoinSelection
import Convex.Utils (mapError)
import Convex.Utxos (BalanceChanges)
import Convex.Utxos qualified as Utxos
import Convex.Wallet.Operator (Operator (..), PaymentExtendedKey (..),
                               Verification, operatorPaymentCredential,
                               operatorReturnOutput)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)

{-| Information needed to build transactions
-}
data BuildTxEnv era =
  BuildTxEnv
    { bteOperator :: Operator Verification
    , bteOperatorUtxos :: UTxO era -- ^ UTxOs owned by the operator, available for spending
    }

{-| Populate the 'BuildTxEnv' with UTxOs locked by the verification key
-}
loadEnv :: (MonadUtxoQuery m, C.IsBabbageBasedEra era) => C.VerificationKey C.PaymentKey -> Maybe (C.VerificationKey C.StakeKey) -> m (BuildTxEnv era)
loadEnv verificationKey oStakeKey = do
  let bteOperator
        = Operator
            { oPaymentKey = PEVerification verificationKey
            , oStakeKey
            }
  bteOperatorUtxos <- Utxos.toApiUtxo <$> utxosByPaymentCredential (operatorPaymentCredential bteOperator)
  pure BuildTxEnv{bteOperator, bteOperatorUtxos}

data BuildTxError era =
  OperatorNoUTxOs -- ^ The operator does not have any UTxOs
  | BalancingError (CoinSelection.BalanceTxError era)
  deriving stock (Show)

{-| Select an output owned by the operator
-}
selectOperatorOutput :: (MonadReader (BuildTxEnv era) m, MonadError (BuildTxError era) m) => m (C.TxIn, C.TxOut C.CtxUTxO era)
selectOperatorOutput = asks (listToMaybe . Map.toList . C.unUTxO . bteOperatorUtxos) >>= \case
  Nothing -> throwError OperatorNoUTxOs
  Just k -> pure k

{-| Balance a transaction using the operator's funds and return output
-}
balanceTxEnv :: forall era a m. (MonadBlockchain era m, MonadReader (BuildTxEnv era) m, MonadError (BuildTxError era) m, C.IsBabbageBasedEra era) => BuildTxT era m a -> m (C.BalancedTxBody era, BalanceChanges)
balanceTxEnv btx = do
  BuildTxEnv{bteOperatorUtxos, bteOperator} <- ask
  params <- queryProtocolParameters
  txBuilder <- BuildTx.execBuildTxT $ btx >> BuildTx.setMinAdaDepositAll params
  output <- operatorReturnOutput bteOperator
  mapError BalancingError (CoinSelection.balanceTx mempty output (Utxos.fromApiUtxo bteOperatorUtxos) txBuilder CoinSelection.TrailingChange)
