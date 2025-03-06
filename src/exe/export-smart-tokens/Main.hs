{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Binary qualified as CBOR
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Data.Aeson (KeyValue ((.=)), eitherDecode, object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (first)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.String (IsString (..))
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Options.Applicative
import Options.Applicative (Parser, argument, customExecParser, disambiguate,
                            eitherReader, help, helper, idm, info, metavar,
                            prefs, showHelpOnEmpty, showHelpOnError)
import Options.Applicative.Builder (ReadM)
import Plutarch.Evaluate (applyArguments, evalScript)
import Plutarch.Internal.Term (Config (..), LogLevel (LogInfo), Script,
                               TracingMode (DoTracing, DoTracingAndBinds),
                               compile)
import Plutarch.Prelude
import Plutarch.Script (serialiseScript)
import PlutusLedgerApi.V2 (Data, ExBudget)
import SmartTokens.Contracts.ExampleTransferLogic (mkFreezeAndSeizeTransfer,
                                                   mkPermissionedTransfer)
import SmartTokens.Contracts.Issuance (mkProgrammableLogicMinting)
import SmartTokens.Contracts.ProgrammableLogicBase (mkProgrammableLogicBase,
                                                    mkProgrammableLogicGlobal)
import SmartTokens.Contracts.ProtocolParams (alwaysFailScript,
                                             mkPermissionedMinting,
                                             mkProtocolParametersMinting)
import SmartTokens.Core.Scripts (ScriptTarget (Production))
import SmartTokens.LinkedList.MintDirectory (mkDirectoryNodeMP)
import SmartTokens.LinkedList.SpendBlacklist (pmkBlacklistSpending)
import SmartTokens.LinkedList.SpendDirectory (pmkDirectorySpending)
import Text.Read (readMaybe)
import Wst.Offchain.Env (BlacklistTransferLogicScriptRoot (..),
                         DirectoryEnv (..),
                         DirectoryScriptRoot (DirectoryScriptRoot),
                         HasDirectoryEnv (directoryEnv), TransferLogicEnv (..),
                         mkDirectoryEnv, programmableTokenMintingScript,
                         transferLogicEnv, withDirectoryFor, withEnv,
                         withTransferFor)
import Wst.Server.Types (SerialiseAddress (..))

encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR = Text.decodeUtf8 . Base16.encode . CBOR.serialize' . serialiseScript

evalT :: Config -> ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT cfg x = evalWithArgsT cfg x []

evalWithArgsT :: Config -> ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT cfg x args = do
  cmp <- compile cfg x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

writePlutusScript :: Config -> String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScript cfg title filepath term = do
  case evalT cfg term of
    Left e -> print e
    Right (script, _, _) -> do
      let
        scriptType = "PlutusScriptV3" :: String
        plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
        content = encodePretty plutusJson
      LBS.writeFile filepath content

writePlutusScriptTraceBind :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptTraceBind = writePlutusScript (Tracing LogInfo DoTracingAndBinds)

writePlutusScriptTrace :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptTrace = writePlutusScript (Tracing LogInfo DoTracing)

writePlutusScriptNoTrace :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptNoTrace = writePlutusScript NoTracing

main :: IO ()
main = runMain

runMain :: IO ()
runMain =
  customExecParser
    (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
    (info (helper <*> parseExportCommand) idm)
    >>= runExportCommand

runExportCommand :: ExportCommand -> IO ()
runExportCommand ExportUnapplied = exportUnapplied
runExportCommand ExportCommand{ecTxIn, ecIssuerAddress=SerialiseAddress issuerAddr} = do
  let opkh = case issuerAddr of
              (C.ShelleyAddress _ntw (C.fromShelleyPaymentCredential -> C.PaymentCredentialByKey pmt) _stakeRef) -> pmt
              _ -> error "Expected public key address" -- FIXME: proper error
      dirRoot = DirectoryScriptRoot ecTxIn Production
      blacklistTransferRoot = BlacklistTransferLogicScriptRoot Production (mkDirectoryEnv dirRoot) opkh

  withEnv $
    withDirectoryFor dirRoot $ do
      withTransferFor blacklistTransferRoot $ do
        transferEnv@TransferLogicEnv
          { tleBlacklistMintingScript
          , tleBlacklistSpendingScript
          , tleMintingScript
          , tleTransferScript
          , tleIssuerScript
          } <- asks transferLogicEnv
        dirEnv@DirectoryEnv
          { dsDirectoryMintingScript
          , dsDirectorySpendingScript
          , dsProtocolParamsMintingScript
          , dsProtocolParamsSpendingScript
          , dsProgrammableLogicBaseScript
          , dsProgrammableLogicGlobalScript
          } <- asks directoryEnv
        let programmableMinting = programmableTokenMintingScript dirEnv transferEnv
        writeAppliedScript "./applied-prod/protocolParametersNFTMinting" "Protocol Parameters NFT" dsProtocolParamsMintingScript
        writeAppliedScript "./applied-prod/protocolParametersSpending" "Protocol Parameters Spending" dsProtocolParamsSpendingScript
        writeAppliedScript "./applied-prod/programmableLogicBaseSpending" "Programmable Logic Base" dsProgrammableLogicBaseScript
        writeAppliedScript "./applied-prod/programmableLogicGlobalStake" "Programmable Logic Global" dsProgrammableLogicGlobalScript
        writeAppliedScript "./applied-prod/directoryNodeMinting" "Directory Node Minting Policy" dsDirectoryMintingScript
        writeAppliedScript "./applied-prod/directoryNodeSpending" "Directory Spending" dsDirectorySpendingScript
        writeAppliedScript "./applied-prod/blacklistSpending" "Blacklist Spending" tleBlacklistSpendingScript
        writeAppliedScript "./applied-prod/blacklistMinting" "Blacklist Minting" tleBlacklistMintingScript
        writeAppliedScript "./applied-prod/transferLogicMinting" "Transfer Logic Minting" tleMintingScript
        writeAppliedScript "./applied-prod/transferLogicSpending" "Transfer Logic Spending" tleTransferScript
        writeAppliedScript "./applied-prod/transferLogicIssuerSpending" "Transfer Logic Issuer Spending" tleIssuerScript
        writeAppliedScript "./applied-prod/programmableTokenMinting" "Programmable Token Minting" programmableMinting

  exportUnapplied

writeAppliedScript :: forall m lang.  (MonadIO m, C.IsPlutusScriptLanguage lang) => FilePath -> C.TextEnvelopeDescr -> C.PlutusScript lang -> m ()
writeAppliedScript path desc script = liftIO $ do
  let lang = C.plutusScriptVersion @lang
      hsh = C.hashScript $ C.PlutusScript lang script
      hshStr = C.serialiseToRawBytesHexText hsh
      path' = path <> "-" <> Text.unpack hshStr <> ".json"
  C.writeFileTextEnvelope (C.File path') (Just desc) script >>= either (error . show) pure


exportUnapplied :: IO ()
exportUnapplied = do
  putStrLn "Writing Plutus Scripts to files"
  writePlutusScriptTraceBind "Programmable Logic Base" "./compiled-binds/programmableLogicBase.json" mkProgrammableLogicBase
  writePlutusScriptTraceBind "Programmable Logic Global" "./compiled-binds/programmableLogicGlobal.json" mkProgrammableLogicGlobal
  writePlutusScriptTraceBind "Issuance" "./compiled-binds/programmableTokenMinting.json" mkProgrammableLogicMinting
  writePlutusScriptTraceBind "Protocol Parameters NFT" "./compiled-binds/protocolParametersNFTMinting.json" mkProtocolParametersMinting
  writePlutusScriptTraceBind "Always Fail" "./compiled-binds/alwaysFail.json" alwaysFailScript
  writePlutusScriptTraceBind "Permissioned Minting" "./compiled-binds/permissionedMinting.json" mkPermissionedMinting
  writePlutusScriptTraceBind "Permissioned Transfer" "./compiled-binds/permissionedTransfer.json" mkPermissionedTransfer
  writePlutusScriptTraceBind "Freeze and Seize Transfer" "./compiled-binds/freezeAndSeizeTransfer.json" mkFreezeAndSeizeTransfer
  writePlutusScriptTraceBind "Directory Node Minting Policy" "./compiled-binds/directoryNodeMintingPolicy.json" mkDirectoryNodeMP
  writePlutusScriptTraceBind "Directory Spending" "./compiled-binds/directorySpending.json" pmkDirectorySpending
  writePlutusScriptTraceBind "Blacklist Spending" "./compiled-binds/blacklistSpending.json" pmkBlacklistSpending

  writePlutusScriptTrace "Programmable Logic Base" "./compiled-tracing/programmableLogicBase.json" mkProgrammableLogicBase
  writePlutusScriptTrace "Programmable Logic Global" "./compiled-tracing/programmableLogicGlobal.json" mkProgrammableLogicGlobal
  writePlutusScriptTrace "Issuance" "./compiled-tracing/programmableTokenMinting.json" mkProgrammableLogicMinting
  writePlutusScriptTrace "Protocol Parameters NFT" "./compiled-tracing/protocolParametersNFTMinting.json" mkProtocolParametersMinting
  writePlutusScriptTrace "Always Fail" "./compiled-tracing/alwaysFail.json" alwaysFailScript
  writePlutusScriptTrace "Permissioned Minting" "./compiled-tracing/permissionedMinting.json" mkPermissionedMinting
  writePlutusScriptTrace "Permissioned Transfer" "./compiled-tracing/permissionedTransfer.json" mkPermissionedTransfer
  writePlutusScriptTrace "Freeze and Seize Transfer" "./compiled-tracing/freezeAndSeizeTransfer.json" mkFreezeAndSeizeTransfer
  writePlutusScriptTrace "Directory Node Minting Policy" "./compiled-tracing/directoryNodeMintingPolicy.json" mkDirectoryNodeMP
  writePlutusScriptTrace "Directory Spending" "./compiled-tracing/directorySpending.json" pmkDirectorySpending
  writePlutusScriptTrace "Blacklist Spending" "./compiled-tracing/blacklistSpending.json" pmkBlacklistSpending

  writePlutusScriptNoTrace "Programmable Logic Base" "./compiled-prod/programmableLogicBase.json" mkProgrammableLogicBase
  writePlutusScriptNoTrace "Programmable Logic Global" "./compiled-prod/programmableLogicGlobal.json" mkProgrammableLogicGlobal
  writePlutusScriptNoTrace "Issuance" "./compiled-prod/programmableTokenMinting.json" mkProgrammableLogicMinting
  writePlutusScriptNoTrace "Protocol Parameters NFT" "./compiled-prod/protocolParametersNFTMinting.json" mkProtocolParametersMinting
  writePlutusScriptNoTrace "Always Fail" "./compiled-prod/alwaysFail.json" alwaysFailScript
  writePlutusScriptNoTrace "Permissioned Minting" "./compiled-prod/permissionedMinting.json" mkPermissionedMinting
  writePlutusScriptNoTrace "Permissioned Transfer" "./compiled-prod/permissionedTransfer.json" mkPermissionedTransfer
  writePlutusScriptNoTrace "Freeze and Seize Transfer" "./compiled-prod/freezeAndSeizeTransfer.json" mkFreezeAndSeizeTransfer
  writePlutusScriptNoTrace "Directory Node Minting Policy" "./compiled-prod/directoryNodeMintingPolicy.json" mkDirectoryNodeMP
  writePlutusScriptNoTrace "Directory Spending" "./compiled-prod/directorySpending.json" pmkDirectorySpending
  writePlutusScriptNoTrace "Blacklist Spending" "./compiled-prod/blacklistSpending.json" pmkBlacklistSpending

data ExportCommand =
  ExportCommand
    { ecTxIn :: C.TxIn
    , ecIssuerAddress :: SerialiseAddress (C.Address C.ShelleyAddr)
    }
  | ExportUnapplied

parseExportCommand :: Parser ExportCommand
parseExportCommand = parseUnapplied <|> parseExportCommandArgs

parseExportCommandArgs :: Parser ExportCommand
parseExportCommandArgs = ExportCommand <$> parseTxIn <*> parseAddress

parseUnapplied :: Parser ExportCommand
parseUnapplied = flag' ExportUnapplied
  ( long "unapplied"
  <> help "Export unapplied scripts"
  )

parseAddress :: Parser (SerialiseAddress (C.Address C.ShelleyAddr))
parseAddress = argument (eitherReader (eitherDecode . LBS8.pack)) (help "The address to use for the issuer" <> metavar "ISSUER_ADDRESS")

parseTxIn :: Parser C.TxIn
parseTxIn =
  argument
    txInReader
    (help "The TxIn that was selected when deploying the system. Format: <tx-id>.<index>" <> metavar "TX_IN")

txInReader :: ReadM C.TxIn
txInReader = eitherReader $ \str -> do
  (txId, txIx) <- case break (== '.') str of
    (txId, _:txIx) -> Right (txId, txIx)
    _ -> Left "Expected <tx-id>.<index>"
  when (length txId /= 64) $ Left "Expected tx ID with 64 characters"
  ix <- case readMaybe @Word txIx of
          Nothing -> Left "Expected tx index"
          Just n -> Right (C.TxIx n)
  return $ C.TxIn (fromString txId) ix
