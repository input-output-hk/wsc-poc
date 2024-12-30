{-| Error type for endpoints and queries
-}
module Wst.AppError(
  AppError(..)
) where

import Blockfrost.Client.Core (BlockfrostError)
import Convex.CoinSelection qualified as CoinSelection
import PlutusLedgerApi.Data.V3 (Credential)

data AppError era =
  OperatorNoUTxOs -- ^ The operator does not have any UTxOs
  | GlobalParamsNodeNotFound -- ^ The node with the global parameters was not found
  | BalancingError (CoinSelection.BalanceTxError era)
  | BlockfrostErr BlockfrostError
  | TransferBlacklistedCredential Credential -- ^ Attempting to transfer funds from a blacklisted address
  deriving stock (Show)
