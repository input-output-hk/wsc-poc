{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
module ProgrammableTokens.OffChain.Env.Operator(
  HasOperatorEnv(..),
  OperatorEnv(..),

  -- ** Creating operator env values
  operatorPaymentCredential,
  loadOperatorEnv,
  loadOperatorEnvFromAddress,
  selectOperatorOutput,
  selectTwoOperatorOutputs,

  -- ** Transaction balancing
  balanceTxEnv_,
  balanceTxEnv
) where

import Cardano.Api (UTxO)
import Cardano.Api.Shelley qualified as C
import Control.Monad.Error.Lens (throwing_)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx (BuildTxT)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain, MonadUtxoQuery (..),
                     queryProtocolParameters, utxosByPaymentCredential)
import Convex.CoinSelection qualified as CoinSelection
import Convex.Utxos (BalanceChanges)
import Convex.Utxos qualified as Utxos
import Convex.Wallet.Operator (returnOutputFor)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import ProgrammableTokens.OffChain.Error (AsProgrammableTokensError (..))

{-| Environments that have an 'OperatorEnv'
-}
class HasOperatorEnv era e | e -> era where
  operatorEnv :: e -> OperatorEnv era

instance HasOperatorEnv era (OperatorEnv era) where
  operatorEnv = id

{-| Information needed to build transactions
-}
data OperatorEnv era =
  OperatorEnv
    { bteOperator      :: (C.Hash C.PaymentKey, C.StakeAddressReference) -- ^ Payment and stake credential, used for generating return outputs
    , bteOperatorUtxos :: UTxO era -- ^ UTxOs owned by the operator, available for spending
    }

{-| Get the operator's payment credential from the 'env'
-}
operatorPaymentCredential :: (MonadReader env m, HasOperatorEnv era env) => m C.PaymentCredential
operatorPaymentCredential = asks (C.PaymentCredentialByKey . fst . bteOperator . operatorEnv)

{-| Populate the 'OperatorEnv' with UTxOs locked by the payment credential
-}
loadOperatorEnv :: (MonadUtxoQuery m, C.IsBabbageBasedEra era) => C.Hash C.PaymentKey -> C.StakeAddressReference -> m (OperatorEnv era)
loadOperatorEnv paymentCredential stakeCredential = do
  let bteOperator = (paymentCredential, stakeCredential)
  bteOperatorUtxos <- Utxos.toApiUtxo <$> utxosByPaymentCredential (C.PaymentCredentialByKey paymentCredential)
  pure OperatorEnv{bteOperator, bteOperatorUtxos}

loadOperatorEnvFromAddress :: (MonadUtxoQuery m, C.IsBabbageBasedEra era) => C.Address C.ShelleyAddr -> m (OperatorEnv era)
loadOperatorEnvFromAddress = \case
  (C.ShelleyAddress _ntw (C.fromShelleyPaymentCredential -> C.PaymentCredentialByKey pmt) stakeRef) ->
    loadOperatorEnv pmt (C.fromShelleyStakeReference stakeRef)
  _ -> error "Expected public key address" -- FIXME: proper error

{-| Select an output owned by the operator
-}
selectOperatorOutput :: (MonadReader env m, HasOperatorEnv era env, MonadError err m, AsProgrammableTokensError err) => m (C.TxIn, C.TxOut C.CtxUTxO era)
selectOperatorOutput = asks (listToMaybe . Map.toList . C.unUTxO . bteOperatorUtxos . operatorEnv) >>= \case
  Nothing -> throwing_ _OperatorNoUTxOs
  Just k -> pure k

selectTwoOperatorOutputs :: (MonadReader env m, HasOperatorEnv era env, MonadError err m, AsProgrammableTokensError err) => m ((C.TxIn, C.TxOut C.CtxUTxO era), (C.TxIn, C.TxOut C.CtxUTxO era))
selectTwoOperatorOutputs = do
  utxos <- asks (C.unUTxO . bteOperatorUtxos . operatorEnv)
  case Map.toList utxos of
    (k1, v1) : (k2, v2) : _rest -> pure ((k1, v1), (k2, v2))
    _ -> throwing_ _OperatorNoUTxOs

{-| Balance a transaction using the operator's funds and return output
-}
balanceTxEnv_ :: forall era env err a m. (MonadBlockchain era m, MonadReader env m, HasOperatorEnv era env, MonadError err m, C.IsBabbageBasedEra era, CoinSelection.AsCoinSelectionError err, CoinSelection.AsBalancingError err era) => BuildTxT era m a -> m (C.BalancedTxBody era, BalanceChanges)
balanceTxEnv_ btx = do
  OperatorEnv{bteOperatorUtxos, bteOperator} <- asks operatorEnv
  params <- queryProtocolParameters
  txBuilder <- BuildTx.execBuildTxT $ btx >> BuildTx.setMinAdaDepositAll params
  -- TODO: change returnOutputFor to consider the stake address reference
  -- (needs to be done in sc-tools)
  output <- returnOutputFor (C.PaymentCredentialByKey $ fst bteOperator)
  CoinSelection.balanceTx mempty output (Utxos.fromApiUtxo bteOperatorUtxos) txBuilder CoinSelection.TrailingChange

{-| Balance a transaction using the operator's funds and return output
-}
balanceTxEnv :: forall era env err a m. (MonadBlockchain era m, MonadReader env m, HasOperatorEnv era env, MonadError err m, C.IsBabbageBasedEra era, CoinSelection.AsBalancingError err era, CoinSelection.AsCoinSelectionError err) => BuildTxT era m a -> m ((C.BalancedTxBody era, BalanceChanges), a)
balanceTxEnv btx = do
  OperatorEnv{bteOperatorUtxos, bteOperator} <- asks operatorEnv
  params <- queryProtocolParameters
  (r, txBuilder) <- BuildTx.runBuildTxT $ btx <* BuildTx.setMinAdaDepositAll params
  -- TODO: change returnOutputFor to consider the stake address reference
  -- (needs to be done in sc-tools)
  output <- returnOutputFor (C.PaymentCredentialByKey $ fst bteOperator)
  (balBody, balChanges) <- CoinSelection.balanceTx mempty output (Utxos.fromApiUtxo bteOperatorUtxos) txBuilder CoinSelection.TrailingChange
  pure ((balBody, balChanges), r)
