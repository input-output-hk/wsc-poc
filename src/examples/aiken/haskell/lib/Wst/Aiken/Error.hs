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
import Cardano.Api (AnyPlutusScriptVersion, AnyScriptLanguage)
import Control.Lens (makeClassyPrisms)
import Convex.CoinSelection (AsBalancingError (..), AsCoinSelectionError (..),
                             BalancingError, CoinSelectionError)
import ProgrammableTokens.OffChain.Error (AsProgrammableTokensError (..),
                                          ProgrammableTokensError)
import Wst.Aiken.BlueprintKey (BlueprintKey)

data BlueprintError =

  -- | Failed to convert 'ScriptInAnyLang' to the target script version
  UnexpectedPlutusVersionError
    { expectedVersion :: AnyPlutusScriptVersion
    , actualVersion   :: AnyScriptLanguage
    }
  deriving stock (Show)

makeClassyPrisms ''BlueprintError

data LookupScriptFailure =
  FailedToFindTransferScript BlueprintKey
  | FailedToFindIssuanceScript BlueprintKey
  deriving stock (Eq, Show)

makeClassyPrisms ''LookupScriptFailure

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
