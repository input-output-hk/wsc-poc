{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
module Wst.Aiken.Error(
  AsBlueprintError(..),
  BlueprintError(..),
  LookupScriptFailure(..),
  AsLookupScriptFailure(..),

  AikenError(..)
) where

import Blockfrost.Client.Core (BlockfrostError)
import Control.Lens (makeClassyPrisms)
import Convex.Aiken.Error (AsBlueprintError (..), AsLookupScriptFailure (..),
                           BlueprintError (..), LookupScriptFailure (..))
import Convex.CoinSelection (AsBalancingError (..), AsCoinSelectionError (..),
                             BalancingError, CoinSelectionError)
import ProgrammableTokens.OffChain.Error (AsProgrammableTokensError (..),
                                          ProgrammableTokensError)

data AikenError era =
  ABlueprintError BlueprintError
  | ALookupScriptFailure LookupScriptFailure
  | ABalancingError (BalancingError era)
  | ACoinSelectionError CoinSelectionError
  | AProgrammableTokensError ProgrammableTokensError
  | ABlockfrostError BlockfrostError
  deriving stock (Show)

makeClassyPrisms ''AikenError

instance AsLookupScriptFailure (AikenError era) where
  _LookupScriptFailure = _ALookupScriptFailure

instance AsBlueprintError (AikenError era) where
  _BlueprintError = _ABlueprintError

instance AsBalancingError (AikenError era) era where
  __BalancingError = _ABalancingError

instance AsCoinSelectionError (AikenError era) where
  _CoinSelectionError = _ACoinSelectionError

instance AsProgrammableTokensError (AikenError era) where
  _ProgrammableTokensError = _AProgrammableTokensError
