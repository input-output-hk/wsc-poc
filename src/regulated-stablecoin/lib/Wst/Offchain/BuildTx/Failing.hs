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

import Cardano.Api.Experimental (IsEra)
import Cardano.Api.Shelley qualified as C
import Control.Lens (set)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx (BuildTxT)
import Convex.BuildTx qualified as BuildTx
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (MonadBlockchain, queryProtocolParameters)
import Convex.CoinSelection qualified as CoinSelection
import Convex.PlutusLedger.V1 (transCredential)
import Convex.Utils (mapError)
import Convex.Utxos (BalanceChanges)
import Convex.Utxos qualified as Utxos
import Convex.Wallet.Operator (returnOutputFor)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Wst.AppError (AppError (..))
import Wst.Offchain.BuildTx.TransferLogic (FindProofResult (..))
import Wst.Offchain.Env (HasOperatorEnv (..), OperatorEnv (..))
import Wst.Offchain.Query (UTxODat (..))

{-| What to do if a transfer cannot proceed because of blacklisting
-}
data BlacklistedTransferPolicy
  = SubmitFailingTx -- ^ Deliberately submit a transaction with "scriptValidity = False". This will result in the collateral input being spent!
  | DontSubmitFailingTx -- ^ Don't submit a transaction
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{-| Balance a transaction using the operator's funds and return output
-}
balanceTxEnvFailing :: forall era env m. (MonadBlockchain era m, MonadReader env m, HasOperatorEnv era env, MonadError (AppError era) m, C.IsBabbageBasedEra era) => BlacklistedTransferPolicy -> BuildTxT era m (FindProofResult era) -> m (C.BalancedTxBody era, BalanceChanges)
balanceTxEnvFailing policy btx = do
  OperatorEnv{bteOperatorUtxos, bteOperator} <- asks operatorEnv
  params <- queryProtocolParameters
  (r, txBuilder) <- BuildTx.runBuildTxT $ btx <* BuildTx.setMinAdaDepositAll params
  -- TODO: change returnOutputFor to consider the stake address reference
  -- (needs to be done in sc-tools)
  let credential = C.PaymentCredentialByKey $ fst bteOperator
  output <- returnOutputFor credential
  (balBody, balChanges) <- case r of
    CredentialNotBlacklisted{} -> do
      mapError BalancingError (CoinSelection.balanceTx mempty output (Utxos.fromApiUtxo bteOperatorUtxos) txBuilder CoinSelection.TrailingChange)
    CredentialBlacklisted UTxODat{}
      | policy == SubmitFailingTx -> do
        -- deliberately set the script validity flag to false
        -- this means we will be losing the collateral!
        let builder' = txBuilder <> BuildTx.liftTxBodyEndo (set L.txScriptValidity (C.TxScriptValidity C.alonzoBasedEra C.ScriptInvalid))
        mapError BalancingError (CoinSelection.balanceTx mempty output (Utxos.fromApiUtxo bteOperatorUtxos) builder' CoinSelection.TrailingChange)
      | otherwise -> do
        throwError (TransferBlacklistedCredential (transCredential credential))
    NoBlacklistNodes -> throwError BlacklistNodeNotFound
  pure (balBody, balChanges)
