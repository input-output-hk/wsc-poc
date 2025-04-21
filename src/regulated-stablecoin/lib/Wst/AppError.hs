{-| Error type for endpoints and queries
-}
module Wst.AppError(
  AppError(..)
) where

import Blockfrost.Client.Core (BlockfrostError)
import Convex.Class (ValidationError)
import Convex.CoinSelection qualified as CoinSelection
import PlutusLedgerApi.V3 (Credential)

data AppError era =
  OperatorNoUTxOs -- ^ The operator does not have any UTxOs
  | GlobalParamsNodeNotFound -- ^ The node with the global parameters was not found
  | IssuanceCborHexUTxONotFound -- ^ The UTxO with the issuance minting cbor hex was not found
  | BalancingError (CoinSelection.BalanceTxError era)
  | BlockfrostErr BlockfrostError
  | NoTokensToSeize -- ^ No tokens to seize
  | DuplicateBlacklistNode -- ^ Attempting to add a duplicate blacklist node
  | BlacklistNodeNotFound -- ^ Attempting to remove a blacklist node that does not exist
  | TransferBlacklistedCredential Credential -- ^ Attempting to transfer funds from a blacklisted address
  | SubmitError (ValidationError era)
  deriving stock (Show)
