{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-| Error type for endpoints and queries
-}
module Wst.AppError(
  -- * Stablecoin errors
  RegulatedStablecoinError(..),
  AsRegulatedStablecoinError(..),

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
import ProgrammableTokens.OffChain.Error (AsProgrammableTokensError (..),
                                          ProgrammableTokensError)

data RegulatedStablecoinError =
  NoTokensToSeize -- ^ No tokens to seize
  | DuplicateBlacklistNode -- ^ Attempting to add a duplicate blacklist node
  | BlacklistNodeNotFound -- ^ Attempting to remove a blacklist node that does not exist
  | TransferBlacklistedCredential Credential -- ^ Attempting to transfer funds from a blacklisted address
  deriving stock (Show)

makeClassyPrisms ''RegulatedStablecoinError

data AppError era =
  BalancingError (CoinSelection.BalanceTxError era)
  | SubmitError (ValidationError era)
  | ProgTokensError ProgrammableTokensError
  | RegStablecoinError RegulatedStablecoinError
  | BlockfrostErr BlockfrostError
  deriving stock (Show)

makeClassyPrisms ''AppError

instance AsBalanceTxError (AppError era) era where
  _BalanceTxError = _BalancingError

instance AsValidationError (AppError era) era where
  _ValidationError = _SubmitError

instance AsProgrammableTokensError (AppError era) where
  _ProgrammableTokensError = _ProgTokensError

instance AsRegulatedStablecoinError (AppError era) where
  _RegulatedStablecoinError = _RegStablecoinError

instance AsCoinSelectionError (AppError era) where
  _CoinSelectionError = _BalancingError . _CoinSelectionError

instance CoinSelection.AsBalancingError (AppError era) era where
  __BalancingError = _BalanceTxError . CoinSelection.__BalancingError

-- CoinSelection.AsCoinSelectionError err, CoinSelection.AsBalancingError err era
