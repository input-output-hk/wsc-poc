{-# LANGUAGE TemplateHaskell #-}
module Wst.Aiken.Error(
  AsBlueprintError(..),
  BlueprintError(..),
  LookupScriptFailure(..),
  AsLookupScriptFailure(..),

  AikenError(..)
) where

import Cardano.Api (AnyPlutusScriptVersion, AnyScriptLanguage)
import Control.Lens (makeClassyPrisms)
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

data AikenError =
  ABlueprintError BlueprintError
  | ALookupScriptFailure LookupScriptFailure
  deriving stock (Show)

makeClassyPrisms ''AikenError

instance AsLookupScriptFailure AikenError where
  _LookupScriptFailure = _ALookupScriptFailure

instance AsBlueprintError AikenError where
  _BlueprintError = _ABlueprintError
