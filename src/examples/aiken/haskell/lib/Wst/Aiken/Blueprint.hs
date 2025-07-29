{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- TODO: This can probably move to sc-tools

-- | Reading plutus scripts from blueprint JSON files
module Wst.Aiken.Blueprint
  ( BlueprintScriptVersion (..),
    Blueprint (..),
    BlueprintValidator (..),
    Preamble (..),
    loadFromFile,
    deserialise,
    getPlutusV3,
  )
where

import Cardano.Api (AnyPlutusScriptVersion, ScriptInAnyLang)
import Cardano.Api qualified as C
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError)
import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Wst.Aiken.BlueprintKey (BlueprintKey)
import Wst.Aiken.Error (AsBlueprintError (..))

-- | Plutus script version with a blueprint-specific JSON encoding
newtype BlueprintScriptVersion = BlueprintScriptVersion AnyPlutusScriptVersion
  deriving stock (Eq, Show)

instance ToJSON BlueprintScriptVersion where
  toJSON (BlueprintScriptVersion (C.AnyPlutusScriptVersion k)) = case k of
    C.PlutusScriptV1 -> toJSON @String "v1"
    C.PlutusScriptV2 -> toJSON @String "v2"
    C.PlutusScriptV3 -> toJSON @String "v3"

instance FromJSON BlueprintScriptVersion where
  parseJSON = fmap (fmap BlueprintScriptVersion) $ Aeson.withText "BlueprintScriptVersion" $ \x -> case T.unpack x of
    "v1" -> pure (C.AnyPlutusScriptVersion C.PlutusScriptV1)
    "v2" -> pure (C.AnyPlutusScriptVersion C.PlutusScriptV2)
    "v3" -> pure (C.AnyPlutusScriptVersion C.PlutusScriptV3)
    v -> fail $ "Unexpected plutus script version: " <> v

data Blueprint = Blueprint
  { preamble :: Preamble,
    validators :: Map BlueprintKey ScriptInAnyLang
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Blueprint where
  parseJSON = withObject "Blueprint" $ \obj ->
    let mkb p v = Blueprint p <$> deserialise p v
     in ( mkb
            <$> obj .: "preamble"
            <*> obj .: "validators"
        )
          >>= either fail pure

data BlueprintValidator = BlueprintValidator
  { title :: BlueprintKey,
    compiledCode :: Text,
    hash :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Preamble = Preamble
  { description :: Text,
    plutusVersion :: BlueprintScriptVersion
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Preamble where
  parseJSON = withObject "Preamble" $ \obj ->
    Preamble
      <$> obj .: "description"
      <*> obj .: "plutusVersion"

loadFromFile :: FilePath -> IO (Either String Blueprint)
loadFromFile fp = Aeson.eitherDecode . BSL.fromStrict <$> BS.readFile fp

deserialise :: Preamble -> [BlueprintValidator] -> Either String (Map BlueprintKey ScriptInAnyLang)
deserialise Preamble {plutusVersion = BlueprintScriptVersion v} validators =
  Map.fromList <$> traverse (deserialiseScript v) validators

deserialiseScript :: AnyPlutusScriptVersion -> BlueprintValidator -> Either String (BlueprintKey, ScriptInAnyLang)
deserialiseScript (C.AnyPlutusScriptVersion v) BlueprintValidator {title, compiledCode} =
  let lng = C.PlutusScriptLanguage v
   in fmap ((title,) . C.ScriptInAnyLang lng) (deserialisePlutus v compiledCode)

deserialisePlutus :: forall lang. (C.IsScriptLanguage lang) => C.PlutusScriptVersion lang -> Text -> Either String (C.Script lang)
deserialisePlutus _ text = first show $ C.deserialiseFromCBOR (C.proxyToAsType $ Proxy @(C.Script lang)) (TE.encodeUtf8 text)

-- | Extract the Plutus V3 script, fail if the script is not V3
getPlutusV3 :: (MonadError err m, AsBlueprintError err) => ScriptInAnyLang -> m (C.Script C.PlutusScriptV3)
getPlutusV3 (C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV3) script) = pure script
getPlutusV3 (C.ScriptInAnyLang otherLang _) =
  throwing _UnexpectedPlutusVersionError (C.AnyPlutusScriptVersion C.PlutusScriptV3, C.AnyScriptLanguage otherLang)
