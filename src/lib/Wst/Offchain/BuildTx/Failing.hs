{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE UndecidableInstances #-}
{-| Tools for deliberately building a transaction
with "scriptValidity" flag set to "invalid".
-}
module Wst.Offchain.BuildTx.Failing(
  IsEra,
  BlacklistedTransferPolicy(..),
  balanceTxEnvFailing
) where

import Cardano.Api.Experimental (IsEra, obtainCommonConstraints, useEra)
import Cardano.Api.Experimental qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Api qualified as L
import Control.Lens (Iso', _3, _Just, at, iso, set, (&), (.~))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.Class (MonadTrans (..))
import Convex.BuildTx (BuildTxT)
import Convex.BuildTx qualified as BuildTx
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (MonadBlockchain (utxoByTxIn), queryProtocolParameters)
import Convex.CoinSelection qualified as CoinSelection
import Convex.PlutusLedger.V1 (transCredential)
import Convex.Scripts (toHashableScriptData)
import Convex.Utils (mapError)
import Convex.Utxos (BalanceChanges)
import Convex.Utxos qualified as Utxos
import Convex.Wallet.Operator (returnOutputFor)
import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import Wst.AppError (AppError (..))
import Wst.Offchain.BuildTx.TransferLogic (FindProofResult (..),
                                           blacklistInitialNode)
import Wst.Offchain.Env (HasOperatorEnv (..), OperatorEnv (..))
import Wst.Offchain.Query (UTxODat (..))

{-| What to do if a transfer cannot proceed because of blacklisting
-}
data BlacklistedTransferPolicy
  = SubmitFailingTx -- ^ Deliberately submit a transaction with "scriptValidity = False". This will result in the collateral input being spent!
  | DontSubmitFailingTx -- ^ Don't submit a transaction
  deriving stock (Eq, Show)

{-| Balance a transaction using the operator's funds and return output
-}
balanceTxEnvFailing :: forall era env m. (IsEra era, MonadBlockchain era m, MonadReader env m, HasOperatorEnv era env, MonadError (AppError era) m, C.IsBabbageBasedEra era) => BlacklistedTransferPolicy -> BuildTxT era m (FindProofResult era) -> m (C.BalancedTxBody era, BalanceChanges)
balanceTxEnvFailing policy btx = do
  OperatorEnv{bteOperatorUtxos, bteOperator} <- asks operatorEnv
  params <- queryProtocolParameters
  (r, txBuilder) <- BuildTx.runBuildTxT $ btx <* BuildTx.setMinAdaDepositAll params
  -- TODO: change returnOutputFor to consider the stake address reference
  -- (needs to be done in sc-tools)
  let credential = C.PaymentCredentialByKey $ fst bteOperator
  output <- returnOutputFor credential
  (balBody, balChanges) <- case r of
    CredentialNotBlacklisted{} ->
      mapError BalancingError (CoinSelection.balanceTx mempty output (Utxos.fromApiUtxo bteOperatorUtxos) txBuilder CoinSelection.TrailingChange)
    CredentialBlacklisted UTxODat{uIn}
      | policy == SubmitFailingTx ->
        fmap (first setScriptsInvalid)
          $ runBacklistResetT uIn
          $ mapError BalancingError (CoinSelection.balanceTx mempty output (Utxos.fromApiUtxo bteOperatorUtxos) txBuilder CoinSelection.TrailingChange)
      | otherwise ->
        throwError (TransferBlacklistedCredential (transCredential credential))
    NoBlacklistNodes -> throwError BlacklistNodeNotFound
  pure (balBody, balChanges)

newtype BlacklistResetT m a = BlacklistResetT (ReaderT C.TxIn m a)
  deriving newtype (Functor, Applicative, Monad, MonadError e, MonadTrans)

instance (C.IsBabbageBasedEra era, MonadBlockchain era m) => MonadBlockchain era (BlacklistResetT m) where
  utxoByTxIn txis = BlacklistResetT $ do
    txi <- ask
    let newDat = C.TxOutDatumInline C.babbageBasedEra (toHashableScriptData blacklistInitialNode)
    fmap (set (_UTxO . at txi . _Just . L._TxOut . _3) newDat) $ utxoByTxIn txis

runBacklistResetT :: C.TxIn -> BlacklistResetT m a -> m a
runBacklistResetT txi (BlacklistResetT action) = runReaderT action txi

_UTxO :: Iso' (C.UTxO era) (Map C.TxIn (C.TxOut C.CtxUTxO era))
_UTxO = iso t f where
  t (C.UTxO k) = k
  f = C.UTxO

setScriptsInvalid ::
  forall era.
  ( IsEra era
  )
  => C.BalancedTxBody era
  -> C.BalancedTxBody era
setScriptsInvalid (C.BalancedTxBody a (C.UnsignedTx b) c d) = obtainCommonConstraints (useEra @era) $
  let b' = C.UnsignedTx (b & L.isValidTxL @(C.LedgerEra era) .~ L.IsValid False)
  in C.BalancedTxBody a b' c d
