{-| JSON utility functions
-}
module ProgrammableTokens.JSON.Utils(
  customJsonOptions
) where

import Data.Aeson qualified as JSON

-- | JSON options that drop @n@ characters and then apply @JSON.camel2@ to the rest
customJsonOptions :: Int -> JSON.Options
customJsonOptions i = JSON.defaultOptions{JSON.fieldLabelModifier= JSON.camelTo2 '_' . drop i }
