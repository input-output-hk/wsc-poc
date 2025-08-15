{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
module ProgrammableTokens.Test.Error(
  TestError(..)
) where

import Control.Lens (makeClassyPrisms)
import Convex.Class (AsValidationError (..), ValidationError)
import Convex.CoinSelection (AsBalanceTxError (..), AsCoinSelectionError (..))
import Convex.CoinSelection qualified as CoinSelection
import ProgrammableTokens.OffChain.Error (AsProgrammableTokensError (..),
                                          ProgrammableTokensError)

data TestError era =
  BalancingError (CoinSelection.BalanceTxError era)
  | SubmitError (ValidationError era)
  | ProgTokensError ProgrammableTokensError
  deriving stock (Show)

makeClassyPrisms ''TestError

instance AsBalanceTxError (TestError era) era where
  _BalanceTxError = _BalancingError

instance AsValidationError (TestError era) era where
  _ValidationError = _SubmitError

instance AsProgrammableTokensError (TestError era) where
  _ProgrammableTokensError = _ProgTokensError

instance AsCoinSelectionError (TestError era) where
  _CoinSelectionError = _BalancingError . _CoinSelectionError

instance CoinSelection.AsBalancingError (TestError era) era where
  __BalancingError = _BalanceTxError . CoinSelection.__BalancingError

-- CoinSelection.AsCoinSelectionError err, CoinSelection.AsBalancingError err era
