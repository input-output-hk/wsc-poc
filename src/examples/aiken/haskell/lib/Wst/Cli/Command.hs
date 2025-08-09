module Wst.Cli.Command(
  Command(..),
  parseCommand,
  PolicyCommand(..),
  TransferPolicySource(..),
  loadTransferPolicy
) where

import Cardano.Api (AssetName, Quantity, ScriptData)
import Cardano.Api.Shelley qualified as C
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Convex.Wallet.Operator (OperatorConfigSigning,
                               parseOperatorConfigSigning)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Base16 qualified as Base16
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Options.Applicative (CommandFields, Mod, Parser, argument, auto, command,
                            eitherReader, fullDesc, help, info, long, metavar,
                            option, optional, progDesc, strArgument, subparser)
import PlutusLedgerApi.V1 qualified as PV1
import ProgrammableTokens.OffChain.Env.TransferLogic (TransferLogicEnv)
import Text.Read (readEither)
import Wst.Aiken.Blueprint qualified as Blueprint
import Wst.Aiken.Error (AsBlueprintError, AsLookupScriptFailure)
import Wst.Aiken.Offchain qualified as OffChain

parseCommand :: Parser Command
parseCommand =
  subparser $
    mconcat
      [ parseDeploy
      , parseGenerate
      , parsePolicyCom
      , parseQuery
      ]

data Command
  = DeployRegistry OperatorConfigSigning FilePath -- ^ Deploy the CIP 143 registry, writing the 'DirectoryScriptRoot' to the file path
  | PolicyCom TransferPolicySource PolicyCommand
  | Query -- ^ Query registry state and user outputs
  | GenerateOperator -- ^ Generate an operator private key
  deriving stock (Eq, Show)

data PolicyCommand
  = Register OperatorConfigSigning
  | Issue OperatorConfigSigning AssetName Quantity PV1.Data
  | Transfer -- ^ Send tokens
  deriving stock (Eq, Show)

parseQuery :: Mod CommandFields Command
parseQuery =
  command "query" $
    info (pure Query) (fullDesc <> progDesc "Query the programmable tokens registry")

parseDeploy :: Mod CommandFields Command
parseDeploy =
  command "deploy" $
    info (DeployRegistry <$> parseOperatorConfigSigning <*> parseDirectoryScriptRootPath) (fullDesc <> progDesc "Deploy the CIP-143 directory and global params")

parseGenerate :: Mod CommandFields Command
parseGenerate =
  command "generate-operator" $
    info (pure GenerateOperator) (fullDesc <> progDesc "Generate an operator private key and print it serialised as bech32")

parsePolicyCom :: Mod CommandFields Command
parsePolicyCom =
  command "policy" $
    info (PolicyCom <$> parseAikenBlueprintPath <*> parsePolicyCommand) (fullDesc <> progDesc "Register a new CIP-143 policy")

parsePolicyCommand :: Parser PolicyCommand
parsePolicyCommand =
  subparser $ mconcat
      [ command "register" $
          info
            (Register <$> parseOperatorConfigSigning)
            (fullDesc <> progDesc "Register a CIP-143 policy using an aiken blueprint")
      , command "issue" $
          info
            (Issue <$> parseOperatorConfigSigning <*> parseAssetName <*> parseQuantity <*> parseScriptDataRedeemer)
            (fullDesc <> progDesc "Issue (mint) a number of programmable assets")
      ]

-- | Registering a new CIP-143 compliant policy
newtype TransferPolicySource =
  AikenBlueprintFile FilePath
  deriving stock (Eq, Show)

parseAikenBlueprintPath :: Parser TransferPolicySource
parseAikenBlueprintPath =
  AikenBlueprintFile <$> strArgument (help "Location of the aiken blueprint JSON file" <> metavar "AIKEN_BLUEPRINT")

parseDirectoryScriptRootPath :: Parser FilePath
parseDirectoryScriptRootPath =
  strArgument (help "Location of the directory script root JSON file" <> metavar "DIRECTORY_SCRIPT_ROOT")

loadTransferPolicy :: (MonadIO m, MonadError err m, AsBlueprintError err, AsLookupScriptFailure err) => TransferPolicySource -> m TransferLogicEnv
loadTransferPolicy = \case
  AikenBlueprintFile fp ->
    Blueprint.loadFromFile_ fp
      >>= flip OffChain.lookupScripts_ OffChain.blueprintKeys
      >>= fmap OffChain.transferLogic . OffChain.extractV3Scripts_

parseAssetName :: Parser AssetName
parseAssetName =
  argument
    (eitherReader (first show . C.deserialiseFromRawBytes (C.proxyToAsType Proxy) . Base16.decodeLenient . TE.encodeUtf8 . Text.pack))
    (help "Asset name (hexadecimal encoding)" <> metavar "ASSET_NAME_HEX")

parseQuantity :: Parser Quantity
parseQuantity =
  C.Quantity <$>
    argument
      auto
      (help "Quantity" <> metavar "QUANTITY")

-- read @Data "I 100"
parseScriptDataRedeemer :: Parser PV1.Data
parseScriptDataRedeemer =
  argument
    (eitherReader (readEither @PV1.Data))
    (help "Redeemer in Plutus Data notation. Example: \"I 100\"" <> metavar "REDEEMER")
