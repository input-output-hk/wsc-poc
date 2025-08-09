module Wst.Cli.Command(
  Command(..),
  parseCommand,
  RegisterCommand(..)
) where

import Convex.Wallet.Operator (OperatorConfigSigning,
                               parseOperatorConfigSigning)
import Options.Applicative (CommandFields, Mod, Parser, command, fullDesc, help,
                            info, metavar, progDesc, strArgument, subparser)

parseCommand :: Parser Command
parseCommand =
  subparser $
    mconcat
      [ parseDeploy
      , parseGenerate
      , parseRegister
      ]

data Command
  = Register OperatorConfigSigning RegisterCommand -- ^ register a CIP 143 policy
  | Deploy OperatorConfigSigning FilePath -- ^ Deploy the CIP 143 registry, writing the 'DirectoryScriptRoot' to the file path
  | Transfer -- ^ Send tokens
  | Query -- ^ Query registry state and user outputs
  | GenerateOperator -- ^ Generate an operator private key
  deriving stock (Eq, Show)

parseDeploy :: Mod CommandFields Command
parseDeploy =
  command "deploy" $
    info (Deploy <$> parseOperatorConfigSigning <*> parseDirectoryScriptRootPath) (fullDesc <> progDesc "Deploy the CIP-143 directory and global params")

parseGenerate :: Mod CommandFields Command
parseGenerate =
  command "generate-operator" $
    info (pure GenerateOperator) (fullDesc <> progDesc "Generate an operator private key and print it serialised as bech32")

parseRegister :: Mod CommandFields Command
parseRegister =
  command "register" $
    info (Register <$> parseOperatorConfigSigning <*> parseRegisterCommand) (fullDesc <> progDesc "Register a new CIP-0143 policy")

-- | Registering a new CIP-143 compliant policy
newtype RegisterCommand =
  RegisterAiken FilePath
  deriving stock (Eq, Show)

parseRegisterCommand :: Parser RegisterCommand
parseRegisterCommand =
  subparser $
    command "aiken" $
      info (RegisterAiken <$> parseAikenBlueprintPath) (fullDesc <> progDesc "Register a CIP-0143 policy using an aiken blueprint")

parseAikenBlueprintPath :: Parser FilePath
parseAikenBlueprintPath =
  strArgument (help "Location of the aiken blueprint JSON file" <> metavar "AIKEN_BLUEPRINT")

parseDirectoryScriptRootPath :: Parser FilePath
parseDirectoryScriptRootPath =
  strArgument (help "Location of the directory script root JSON file" <> metavar "DIRECTORY_SCRIPT_ROOT")
