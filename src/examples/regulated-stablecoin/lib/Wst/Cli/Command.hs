-- | The CLI commands and parsers
module Wst.Cli.Command (
    parseCommand,
    Command (..),
    ManageCommand (..),

    -- * Other parsers
    parseTxIn,
    txInReader,
) where

import Cardano.Api (TxIn (..), TxIx (..), parseTxId)
import Cardano.Api.Parser.Text qualified as Parser
import Control.Monad (when)
import Convex.Wallet.Operator (
    OperatorConfigSigning,
    parseOperatorConfigSigning,
 )
import Data.Text qualified as Text
import Options.Applicative (
    CommandFields,
    Mod,
    Parser,
    ReadM,
    argument,
    auto,
    command,
    eitherReader,
    fullDesc,
    help,
    info,
    long,
    metavar,
    option,
    optional,
    progDesc,
    short,
    showDefault,
    subparser,
    value,
 )
import Options.Applicative.Builder (strOption)
import Text.Read (readMaybe)
import Wst.Server (ServerArgs (..))

parseCommand :: Parser Command
parseCommand =
    subparser $
        mconcat
            [ parseDeploy
            , parseManage
            , parseDeployIssuanceCborHex
            , parseRegisterPolicyStakeScripts
            , parseBlacklistInit
            ]

data Command
    = Deploy OperatorConfigSigning
    | DeployIssuanceCborHex OperatorConfigSigning TxIn TxIn
    | RegisterPolicyStakeScripts OperatorConfigSigning TxIn TxIn
    | BlacklistInit OperatorConfigSigning TxIn TxIn
    | Manage TxIn TxIn ManageCommand
    deriving (Show)

-- | Commands that require a deployed system
data ManageCommand
    = Status
    | StartServer ServerArgs
    deriving stock (Show)

parseDeploy :: Mod CommandFields Command
parseDeploy =
    command "deploy" $
        info (Deploy <$> parseOperatorConfigSigning) (fullDesc <> progDesc "Deploy the directory and global params")

parseBlacklistInit :: Mod CommandFields Command
parseBlacklistInit =
    command "blacklist-init" $
        info (BlacklistInit <$> parseOperatorConfigSigning <*> parseTxIn <*> parseTxIn) (fullDesc <> progDesc "Deploy the blacklist")

parseRegisterPolicyStakeScripts :: Mod CommandFields Command
parseRegisterPolicyStakeScripts =
    command "register-policy-stake-scripts" $
        info (RegisterPolicyStakeScripts <$> parseOperatorConfigSigning <*> parseTxIn <*> parseTxIn) (fullDesc <> progDesc "Register the stake scripts for the programmable token policy")

parseDeployIssuanceCborHex :: Mod CommandFields Command
parseDeployIssuanceCborHex =
    command "issuance-cbor-hex" $
        info (DeployIssuanceCborHex <$> parseOperatorConfigSigning <*> parseTxIn <*> parseTxIn) (fullDesc <> progDesc "Deploy the issuance cbor hex")

parseManage :: Mod CommandFields Command
parseManage =
    command "manage" $
        info (Manage <$> parseTxIn <*> parseTxIn <*> parseManageCommand) (fullDesc <> progDesc "Manage a deployed system")

parseManageCommand :: Parser ManageCommand
parseManageCommand = subparser $ mconcat [parseStatus, parseStartServer]

parseStatus :: Mod CommandFields ManageCommand
parseStatus =
    command "status" $
        info (pure Status) (fullDesc <> progDesc "Show the status of the programmable tokens")

parseStartServer :: Mod CommandFields ManageCommand
parseStartServer =
    command "start" $
        info (StartServer <$> parseServerArgs) (fullDesc <> progDesc "Start the HTTP server")

parseServerArgs :: Parser ServerArgs
parseServerArgs =
    ServerArgs
        <$> option auto (help "The port" <> value 8080 <> long "port" <> short 'p')
        <*> optional (strOption (help "Folder to serve static files from" <> long "static-files"))
        <*> optional (strOption (help "JSON file with demo environment" <> long "demo-environment"))
        <*> strOption
            ( help "SQLite file for persisting policy issuer metadata"
                <> long "policy-issuer-store"
                <> metavar "FILE"
                <> value "policy-issuers.sqlite"
                <> showDefault
            )

parseTxIn :: Parser TxIn
parseTxIn =
    argument
        txInReader
        (help "The TxIn that was selected when deploying the system. Format: <tx-id>#<index>" <> metavar "TX_IN")

txInReader :: ReadM TxIn
txInReader = eitherReader $ \str -> do
    (txId, txIx) <- case break ((==) '#') str of
        (txId, _ : txIx) -> Right (txId, txIx)
        _ -> Left "Expected <tx-id>#<index>"
    when (length txId /= 64) $ Left "Expected tx ID with 64 characters"
    ix <- case readMaybe @Word txIx of
        Nothing -> Left "Expected tx index"
        Just n -> Right (TxIx n)
    txId' <- Parser.runParser parseTxId (Text.pack txId)
    return $ TxIn txId' ix
