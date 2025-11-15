{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE UndecidableInstances #-}
module ProgrammableTokens.OffChain.Env.Operator(
  HasOperatorEnv(..),
  OperatorEnv(..),

  -- ** Creating operator env values
  operatorPaymentCredential,
  loadOperatorEnv,
  reloadOperatorEnv,
  loadOperatorEnvFromAddress,
  loadConvexOperatorEnv,
  selectOperatorOutput,
  selectTwoOperatorOutputs,
  selectOperatorUTxOs,
  runAs,

  -- ** Transaction balancing
  balanceTxEnv_,
  balanceTxEnv,
) where

import Cardano.Api (UTxO)
import Cardano.Api.Shelley qualified as C
import Control.Monad.Error.Lens (throwing_)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Convex.BuildTx (BuildTxT)
import Convex.BuildTx qualified as BuildTx
import Convex.CardanoApi.Lenses (emptyTxOut)
import Convex.Class (MonadBlockchain, MonadUtxoQuery (..), queryNetworkId,
                     queryProtocolParameters, utxosByPaymentCredential)
import Convex.CoinSelection qualified as CoinSelection
import Convex.Utxos (BalanceChanges)
import Convex.Utxos qualified as Utxos
import Convex.Wallet.Operator qualified as Op
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import ProgrammableTokens.OffChain.Env.Utils qualified as Env
import ProgrammableTokens.OffChain.Error (AsProgrammableTokensError (..))

{-| Environments that have an 'OperatorEnv'
-}
class HasOperatorEnv era e where
  operatorEnv :: e -> OperatorEnv era

instance HasOperatorEnv era (OperatorEnv era) where
  operatorEnv = id

instance (Env.Elem (OperatorEnv era) els) => HasOperatorEnv era (Env.HSet els) where
  operatorEnv = Env.hget @_ @(OperatorEnv era)

{-| Information needed to build transactions
-}
data OperatorEnv era =
  OperatorEnv
    { bteOperator      :: (C.Hash C.PaymentKey, C.StakeAddressReference) -- ^ Payment and stake credential, used for generating return outputs
    , bteOperatorUtxos :: UTxO era -- ^ UTxOs owned by the operator, available for spending
    }

{-| Get the operator's payment credential from the 'env'
-}
operatorPaymentCredential :: forall env era m. (MonadReader env m, HasOperatorEnv era env) => m C.PaymentCredential
operatorPaymentCredential = asks (C.PaymentCredentialByKey . fst . bteOperator . operatorEnv @era)

{-| Populate the 'OperatorEnv' with UTxOs locked by the payment credential
-}
loadOperatorEnv :: (MonadUtxoQuery m, C.IsBabbageBasedEra era) => C.Hash C.PaymentKey -> C.StakeAddressReference -> m (OperatorEnv era)
loadOperatorEnv paymentCredential stakeCredential = do
  let bteOperator = (paymentCredential, stakeCredential)
  bteOperatorUtxos <- Utxos.toApiUtxo <$> utxosByPaymentCredential (C.PaymentCredentialByKey paymentCredential)
  pure OperatorEnv{bteOperator, bteOperatorUtxos}

{-| Refresh an existing 'OperatorEnv' by re-querying the chain for its UTxOs
-}
reloadOperatorEnv :: (MonadUtxoQuery m, C.IsBabbageBasedEra era) => OperatorEnv era -> m (OperatorEnv era)
reloadOperatorEnv OperatorEnv{bteOperator = (paymentCredential, stakeCredential)} =
  loadOperatorEnv paymentCredential stakeCredential

{-| Glue code for creating an 'OperatorEnv' from the convex type
-}
-- TODO: This entire module should be merged with the one from convex!
loadConvexOperatorEnv :: (MonadUtxoQuery m, C.IsBabbageBasedEra era) => Op.Operator k -> m (OperatorEnv era)
loadConvexOperatorEnv op =
  loadOperatorEnv
    (C.verificationKeyHash $ Op.verificationKey $ Op.oPaymentKey op)
    (maybe C.NoStakeAddress (C.StakeAddressByValue . C.StakeCredentialByKey . C.verificationKeyHash) $ Op.oStakeKey op)

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

{-| Select all UTxOs owned by the operator
-}
selectOperatorUTxOs :: (MonadReader env m, HasOperatorEnv era env, MonadError err m, AsProgrammableTokensError err) => m [(C.TxIn, C.TxOut C.CtxUTxO era)]
selectOperatorUTxOs = do
    asks (C.unUTxO . bteOperatorUtxos . operatorEnv) >>= (\case
      [] -> throwing_ _OperatorNoUTxOs
      utxos -> pure utxos) . Map.toList

{- An empty output to address produced by the payment credential and stake credential.
-}
returnOutputForWithStakeReference :: (MonadBlockchain era m, C.IsShelleyBasedEra era) => (C.Hash C.PaymentKey, C.StakeAddressReference) -> m (C.TxOut ctx era)
returnOutputForWithStakeReference (paymentCredential, stakeReference) = do
  addr <-
    C.makeShelleyAddress
      <$> queryNetworkId
      <*> pure (C.PaymentCredentialByKey paymentCredential)
      <*> pure stakeReference
  pure $ emptyTxOut $ C.AddressInEra (C.ShelleyAddressInEra C.shelleyBasedEra) addr

{-| Balance a transaction using the operator's funds and return output
-}
balanceTxEnv_ :: forall era env err a m. (MonadBlockchain era m, MonadReader env m, HasOperatorEnv era env, MonadError err m, C.IsBabbageBasedEra era, CoinSelection.AsCoinSelectionError err, CoinSelection.AsBalancingError err era) => BuildTxT era m a -> m (C.BalancedTxBody era, BalanceChanges)
balanceTxEnv_ btx = do
  OperatorEnv{bteOperatorUtxos, bteOperator} <- asks (operatorEnv @era)
  params <- queryProtocolParameters
  txBuilder <- BuildTx.execBuildTxT $ btx >> BuildTx.setMinAdaDepositAll params
  output <- returnOutputForWithStakeReference bteOperator
  CoinSelection.balanceTx mempty output (Utxos.fromApiUtxo bteOperatorUtxos) txBuilder CoinSelection.TrailingChange

{-| Balance a transaction using the operator's funds and return output
-}
balanceTxEnv :: forall era env err a m. (MonadBlockchain era m, MonadReader env m, HasOperatorEnv era env, MonadError err m, C.IsBabbageBasedEra era, CoinSelection.AsBalancingError err era, CoinSelection.AsCoinSelectionError err) => BuildTxT era m a -> m ((C.BalancedTxBody era, BalanceChanges), a)
balanceTxEnv btx = do
  OperatorEnv{bteOperatorUtxos, bteOperator} <- asks (operatorEnv @era)
  params <- queryProtocolParameters
  (r, txBuilder) <- BuildTx.runBuildTxT $ btx <* BuildTx.setMinAdaDepositAll params
  output <- returnOutputForWithStakeReference bteOperator
  (balBody, balChanges) <- CoinSelection.balanceTx mempty output (Utxos.fromApiUtxo bteOperatorUtxos) txBuilder CoinSelection.TrailingChange
  pure ((balBody, balChanges), r)

{-| Load the operator UTxOs and run the action
-}
runAs ::
  forall k era els m a.
  ( MonadUtxoQuery m
  , C.IsBabbageBasedEra era
  , Env.NotElem (OperatorEnv era) els
  , MonadReader (Env.HSet els) m
  ) => Op.Operator k -> ReaderT (Env.HSet (OperatorEnv era : els)) m a -> m a
runAs op action = do
  opEnv <- loadConvexOperatorEnv op
  Env.withEnv opEnv action
