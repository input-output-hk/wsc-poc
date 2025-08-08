module Wst.Cli.Command(
  Command(..),
  parseCommand
) where

import Convex.Wallet.Operator (OperatorConfigSigning,
                               parseOperatorConfigSigning)
import Options.Applicative (CommandFields, Mod, Parser, command, fullDesc, info,
                            progDesc, subparser)

parseCommand :: Parser Command
parseCommand =
  subparser $
    mconcat
      [ parseDeploy
      , generateOperator
      ]

data Command
  = Register -- ^ register a CIP 143 policy
  | Deploy OperatorConfigSigning -- ^ Deploy the CIP 143 registry
  | Transfer -- ^ Send tokens
  | Query -- ^ Query registry state and user outputs
  | GenerateOperator -- ^ Generate an operator private key
  deriving stock (Eq, Show)

parseDeploy :: Mod CommandFields Command
parseDeploy =
  command "deploy" $
    info (Deploy <$> parseOperatorConfigSigning) (fullDesc <> progDesc "Deploy the CIP-143 directory and global params")

generateOperator :: Mod CommandFields Command
generateOperator =
  command "generate-operator" $
    info (pure GenerateOperator) (fullDesc <> progDesc "Generate an operator private key and print it serialised as bech32")
