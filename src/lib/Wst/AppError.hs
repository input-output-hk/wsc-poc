{-| Error type for endpoints and queries
-}
module Wst.AppError(
  AppError(..)
) where

import Blockfrost.Client.Core (BlockfrostError)
import Convex.CoinSelection qualified as CoinSelection

data AppError era =
  OperatorNoUTxOs -- ^ The operator does not have any UTxOs
  | GlobalParamsNodeNotFound -- ^ The node with the global parameters was not found
  | BalancingError (CoinSelection.BalanceTxError era)
  | BlockfrostErr BlockfrostError
  deriving stock (Show)
