{-| The CLI commands and parsers
-}
module Wst.Cli.Command(
  parseCommand,
  Command(..),
  ManageCommand(..)
) where

import Cardano.Api (TxIn (..), TxIx (..))
import Control.Monad (when)
import Data.String (IsString (..))
import Options.Applicative (CommandFields, Mod, Parser, ReadM, argument,
                            command, eitherReader, fullDesc, help, info, long,
                            many, metavar, optional, progDesc, short, str,
                            strOption, subparser, (<|>))
import Text.Read (readMaybe)


parseCommand :: Parser Command
parseCommand =
  subparser $
    mconcat
      [ parseDeploy
      , parseManage
      ]

data Command =
  Deploy
  | Manage TxIn ManageCommand
  deriving Show

-- | Commands that require a deployed system
data ManageCommand =
  Status
  deriving stock Show

parseDeploy :: Mod CommandFields Command
parseDeploy =
  command "deploy" $
    info (pure Deploy) (fullDesc <> progDesc "Deploy the directory and global params")

parseManage :: Mod CommandFields Command
parseManage =
  command "manage" $
    info (Manage <$> parseTxIn <*> parseManageCommand) (fullDesc <> progDesc "Manage a deployed system")

parseManageCommand :: Parser ManageCommand
parseManageCommand = subparser $ mconcat [parseStatus]

parseStatus :: Mod CommandFields ManageCommand
parseStatus =
  command "status" $
    info (pure Status) (fullDesc <> progDesc "Show the status of the programmable tokens")

parseTxIn :: Parser TxIn
parseTxIn =
  argument
    txInReader
    (help "The TxIn that was selected when deploying the system. Format: <tx-id>.<index>" <> metavar "TX_IN")

txInReader :: ReadM TxIn
txInReader = eitherReader $ \str -> do
  (txId, txIx) <- case break ((==) '.') str of
    (txId, _:txIx) -> Right (txId, txIx)
    _ -> Left "Expected <tx-id>.<index>"
  -- 8c728a68fed42fe4893fb84ee2c6276a25e642d9892962af3234f952ea641993
  when (length txId /= 64) $ Left "Expected tx ID with 64 characters"
  ix <- case readMaybe @Word txIx of
          Nothing -> Left "Expected tx index"
          Just n -> Right (TxIx n)
  return $ TxIn (fromString txId) ix
