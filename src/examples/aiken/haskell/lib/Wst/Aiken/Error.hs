{-# LANGUAGE TemplateHaskell #-}
module Wst.Aiken.Error(
  AsBlueprintError(..),
  BlueprintError(..)
) where

import Cardano.Api (AnyPlutusScriptVersion, AnyScriptLanguage)
import Control.Lens (makeClassyPrisms)

data BlueprintError =

  -- | Failed to convert 'ScriptInAnyLang' to the target script version
  UnexpectedPlutusVersionError
    { expectedVersion :: AnyPlutusScriptVersion
    , actualVersion   :: AnyScriptLanguage
    }
  deriving stock (Show)

makeClassyPrisms ''BlueprintError
