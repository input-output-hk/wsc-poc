{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-| Error type for endpoints and queries
-}
module Wst.AppError(
  -- * Programmable token errors
  ProgrammableTokensError(..),
  AsProgrammableTokensError(..),

  -- * WST App error
  AppError(..),
  AsAppError(..)
) where

import Blockfrost.Client.Core (BlockfrostError)
import Control.Lens (makeClassyPrisms)
import Convex.Class (AsValidationError (..), ValidationError)
import Convex.CoinSelection (AsBalanceTxError (..), AsCoinSelectionError (..))
import Convex.CoinSelection qualified as CoinSelection
import PlutusLedgerApi.V3 (Credential)

data ProgrammableTokensError =
  OperatorNoUTxOs -- ^ The operator does not have any UTxOs
  | GlobalParamsNodeNotFound -- ^ The node with the global parameters was not found
  | IssuanceCborHexUTxONotFound -- ^ The UTxO with the issuance minting cbor hex was not found
  | DirectorySetNodeNotFound -- ^ The UTxO with the directory entry for the policy was not found. (Policy not registered properly?)
  -- TODO: The following errors are specific to the regulated stablecoin
  -- They should be separated out
  | NoTokensToSeize -- ^ No tokens to seize
  | DuplicateBlacklistNode -- ^ Attempting to add a duplicate blacklist node
  | BlacklistNodeNotFound -- ^ Attempting to remove a blacklist node that does not exist
  | TransferBlacklistedCredential Credential -- ^ Attempting to transfer funds from a blacklisted address
  deriving stock (Show)

makeClassyPrisms ''ProgrammableTokensError

data AppError era =
  BalancingError (CoinSelection.BalanceTxError era)
  | SubmitError (ValidationError era)
  | ProgTokensError ProgrammableTokensError
  | BlockfrostErr BlockfrostError
  deriving stock (Show)

makeClassyPrisms ''AppError

instance AsBalanceTxError (AppError era) era where
  _BalanceTxError = _BalancingError

instance AsValidationError (AppError era) era where
  _ValidationError = _SubmitError

instance AsProgrammableTokensError (AppError era) where
  _ProgrammableTokensError = _ProgTokensError

instance AsCoinSelectionError (AppError era) where
  _CoinSelectionError = _BalancingError . _CoinSelectionError

instance CoinSelection.AsBalancingError (AppError era) era where
  __BalancingError = _BalanceTxError . CoinSelection.__BalancingError

-- CoinSelection.AsCoinSelectionError err, CoinSelection.AsBalancingError err era
