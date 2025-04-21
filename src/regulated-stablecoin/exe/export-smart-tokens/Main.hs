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
import Data.Foldable (traverse_)
import Data.String (IsString (..))
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as TIO
import Options.Applicative (Parser, argument, customExecParser, disambiguate,
                            eitherReader, help, helper, idm, info, metavar,
                            optional, prefs, showHelpOnEmpty, showHelpOnError,
                            strArgument)
import Options.Applicative.Builder (ReadM)
import Plutarch.Evaluate (applyArguments, evalScript)
import Plutarch.Internal.Term (Config (..), LogLevel (LogInfo), Script,
                               TracingMode (DoTracing, DoTracingAndBinds),
                               compile)
import Plutarch.Prelude
import Plutarch.Script (serialiseScript)
import PlutusLedgerApi.Common (toData)
import PlutusLedgerApi.V2 (Data, ExBudget)
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx.Builtins.HasOpaque
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
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Read (readMaybe)
import Wst.Offchain.Env (BlacklistTransferLogicScriptRoot (..),
                         DirectoryEnv (..),
                         DirectoryScriptRoot (DirectoryScriptRoot),
                         HasDirectoryEnv (directoryEnv), TransferLogicEnv (..),
                         globalParams, mkDirectoryEnv,
                         programmableTokenMintingScript, transferLogicEnv,
                         withDirectoryFor, withEnv, withTransferFor)
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

_writePlutusScriptWithArgs :: String -> FilePath -> [Data] -> Script -> IO ()
_writePlutusScriptWithArgs title filepath args compiledScript = do
  let appliedScript = applyArguments compiledScript args
      scriptType = "PlutusScriptV3" :: String
      plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR appliedScript]
      content = encodePretty plutusJson
  LBS.writeFile filepath content

writePlutusScriptTraceBind :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptTraceBind = writePlutusScript (Tracing LogInfo DoTracingAndBinds)

writePlutusScriptTrace :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptTrace = writePlutusScript (Tracing LogInfo DoTracing)

writePlutusScriptNoTrace :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptNoTrace = writePlutusScript NoTracing

issuerPrefixPostfixBytes :: V3.Credential -> (Text, Text)
issuerPrefixPostfixBytes progLogicCred =
  let
      dummyHex = "deadbeefcafebabe"
      placeholderMintingLogic = V3.ScriptHash $ stringToBuiltinByteStringHex "deadbeefcafebabe"
      issuerScriptBase =
        case compile NoTracing (mkProgrammableLogicMinting # pconstant progLogicCred) of
          Right compiledScript -> compiledScript
          Left err -> error $ "Failed to compile issuer script: " <> show err
      dummyIssuerInstanceCborHex = encodeSerialiseCBOR $ applyArguments issuerScriptBase [toData placeholderMintingLogic]
   in breakCborHex dummyHex dummyIssuerInstanceCborHex

breakCborHex :: Text -> Text -> (Text, Text)
breakCborHex toSplitOn cborHex =
  case Text.breakOn toSplitOn cborHex of
    (before, after)
      | not (Text.null after) -> (before, Text.drop (Text.length toSplitOn) after)
      | otherwise          -> (cborHex, "")

main :: IO ()
main = do
  let progLogicBase = V3.ScriptCredential (V3.ScriptHash "deadbeef")
      mintingLogicA = V3.ScriptHash $ stringToBuiltinByteStringHex "deadbeefdeadbeef"
      mintingLogicB = V3.ScriptHash $ stringToBuiltinByteStringHex "deadbeefcafebabe"
      (prefixIssuerCborHex, postfixIssuerCborHex) = issuerPrefixPostfixBytes progLogicBase
  let baseCompiled = case compile NoTracing (mkProgrammableLogicMinting # pconstant progLogicBase) of
        Right compiledScript -> compiledScript
        Left err -> error $ "Failed to compile base issuance script: " <> show err

  TIO.writeFile ("generated" </> "unapplied" </> "test" </> "prefixIssuerCborHex.txt") prefixIssuerCborHex
  TIO.writeFile ("generated" </> "unapplied" </> "test" </> "postfixIssuerCborHex.txt") postfixIssuerCborHex
  _writePlutusScriptWithArgs "Issuance" ("generated" </> "unapplied" </> "test" </> "issuance1.json") [toData mintingLogicA] baseCompiled
  _writePlutusScriptWithArgs "Issuance" ("generated" </> "unapplied" </> "test" </> "issuance2.json") [toData mintingLogicB] baseCompiled

  runMain

runMain :: IO ()
runMain =
  customExecParser
    (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
    (info (helper <*> parseExportCommand) idm)
    >>= runExportCommand

writeAppliedScripts :: FilePath -> AppliedScriptArgs -> IO ()
writeAppliedScripts baseFolder AppliedScriptArgs{asaTxIn, asaIssuerCborHexTxIn, asaIssuerAddress=SerialiseAddress issuerAddr} = do
  let opkh = case issuerAddr of
              (C.ShelleyAddress _ntw (C.fromShelleyPaymentCredential -> C.PaymentCredentialByKey pmt) _stakeRef) -> pmt
              _ -> error "Expected public key address" -- FIXME: proper error
      dirRoot = DirectoryScriptRoot asaTxIn asaIssuerCborHexTxIn Production
      blacklistTransferRoot = BlacklistTransferLogicScriptRoot Production (mkDirectoryEnv dirRoot) opkh
  putStrLn "Writing applied Plutus scripts to files"
  createDirectoryIfMissing True baseFolder
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
        let ProgrammableLogicGlobalParams {progLogicCred} = globalParams dirEnv
            (prefixIssuerCborHex, postfixIssuerCborHex) = issuerPrefixPostfixBytes progLogicCred
        liftIO $ TIO.writeFile (baseFolder </> "prefixIssuerCborHex.txt") prefixIssuerCborHex
        liftIO $ TIO.writeFile (baseFolder </> "postfixIssuerCborHex.txt") postfixIssuerCborHex
        let programmableMinting = programmableTokenMintingScript dirEnv transferEnv
        writeAppliedScript (baseFolder </> "protocolParametersNFTMinting") "Protocol Parameters NFT" dsProtocolParamsMintingScript
        writeAppliedScript (baseFolder </> "protocolParametersSpending") "Protocol Parameters Spending" dsProtocolParamsSpendingScript
        writeAppliedScript (baseFolder </> "programmableLogicBaseSpending") "Programmable Logic Base" dsProgrammableLogicBaseScript
        writeAppliedScript (baseFolder </> "programmableLogicGlobalStake") "Programmable Logic Global" dsProgrammableLogicGlobalScript
        writeAppliedScript (baseFolder </> "directoryNodeMinting") "Directory Node Minting Policy" dsDirectoryMintingScript
        writeAppliedScript (baseFolder </> "directoryNodeSpending") "Directory Spending" dsDirectorySpendingScript
        writeAppliedScript (baseFolder </> "blacklistSpending") "Blacklist Spending" tleBlacklistSpendingScript
        writeAppliedScript (baseFolder </> "blacklistMinting") "Blacklist Minting" tleBlacklistMintingScript
        writeAppliedScript (baseFolder </> "transferLogicMinting") "Transfer Logic Minting" tleMintingScript
        writeAppliedScript (baseFolder </> "transferLogicSpending") "Transfer Logic Spending" tleTransferScript
        writeAppliedScript (baseFolder </> "transferLogicIssuerSpending") "Transfer Logic Issuer Spending" tleIssuerScript
        writeAppliedScript (baseFolder </> "programmableTokenMinting") "Programmable Token Minting" programmableMinting


runExportCommand :: ExportCommand -> IO ()
runExportCommand ExportCommand{exBaseFolder, exAppliedScript} = case exAppliedScript of
  Nothing -> exportUnapplied exBaseFolder
  Just args -> writeAppliedScripts exBaseFolder args

writeAppliedScript :: forall m lang.  (MonadIO m, C.IsPlutusScriptLanguage lang) => FilePath -> C.TextEnvelopeDescr -> C.PlutusScript lang -> m ()
writeAppliedScript path desc script = liftIO $ do
  let lang = C.plutusScriptVersion @lang
      hsh = C.hashScript $ C.PlutusScript lang script
      hshStr = C.serialiseToRawBytesHexText hsh
      path' = path <> "-" <> Text.unpack hshStr <> ".json"
  C.writeFileTextEnvelope (C.File path') (Just desc) script >>= either (error . show) pure


exportUnapplied :: FilePath -> IO ()
exportUnapplied fp = do
  putStrLn "Writing unapplied Plutus scripts to files"
  let prod    = fp </> "prod"
      binds   = fp </> "binds"
      tracing = fp </> "tracing"
  traverse_ (createDirectoryIfMissing True) [prod, binds, tracing]

  -- TODO: Why is there no difference between the scripts??

  writePlutusScriptTraceBind "Programmable Logic Base" (binds </> "programmableLogicBase.json") mkProgrammableLogicBase
  writePlutusScriptTraceBind "Programmable Logic Global" (binds </> "programmableLogicGlobal.json") mkProgrammableLogicGlobal
  writePlutusScriptTraceBind "Issuance" (binds </> "programmableTokenMinting.json") mkProgrammableLogicMinting
  writePlutusScriptTraceBind "Protocol Parameters NFT" (binds </> "protocolParametersNFTMinting.json") mkProtocolParametersMinting
  writePlutusScriptTraceBind "Always Fail" (binds </> "alwaysFail.json") alwaysFailScript
  writePlutusScriptTraceBind "Permissioned Minting" (binds </> "permissionedMinting.json") mkPermissionedMinting
  writePlutusScriptTraceBind "Permissioned Transfer" (binds </> "permissionedTransfer.json") mkPermissionedTransfer
  writePlutusScriptTraceBind "Freeze and Seize Transfer" (binds </> "freezeAndSeizeTransfer.json") mkFreezeAndSeizeTransfer
  writePlutusScriptTraceBind "Directory Node Minting Policy" (binds </> "directoryNodeMintingPolicy.json") mkDirectoryNodeMP
  writePlutusScriptTraceBind "Directory Spending" (binds </> "directorySpending.json") pmkDirectorySpending
  writePlutusScriptTraceBind "Blacklist Spending" (binds </> "blacklistSpending.json") pmkBlacklistSpending

  writePlutusScriptTrace "Programmable Logic Base" (tracing </> "programmableLogicBase.json") mkProgrammableLogicBase
  writePlutusScriptTrace "Programmable Logic Global" (tracing </> "programmableLogicGlobal.json") mkProgrammableLogicGlobal
  writePlutusScriptTrace "Issuance" (tracing </> "programmableTokenMinting.json") mkProgrammableLogicMinting
  writePlutusScriptTrace "Protocol Parameters NFT" (tracing </> "protocolParametersNFTMinting.json") mkProtocolParametersMinting
  writePlutusScriptTrace "Always Fail" (tracing </> "alwaysFail.json") alwaysFailScript
  writePlutusScriptTrace "Permissioned Minting" (tracing </> "permissionedMinting.json") mkPermissionedMinting
  writePlutusScriptTrace "Permissioned Transfer" (tracing </> "permissionedTransfer.json") mkPermissionedTransfer
  writePlutusScriptTrace "Freeze and Seize Transfer" (tracing </> "freezeAndSeizeTransfer.json") mkFreezeAndSeizeTransfer
  writePlutusScriptTrace "Directory Node Minting Policy" (tracing </> "directoryNodeMintingPolicy.json") mkDirectoryNodeMP
  writePlutusScriptTrace "Directory Spending" (tracing </> "directorySpending.json") pmkDirectorySpending
  writePlutusScriptTrace "Blacklist Spending" (tracing </> "blacklistSpending.json") pmkBlacklistSpending

  writePlutusScriptNoTrace "Programmable Logic Base" (prod </> "programmableLogicBase.json") mkProgrammableLogicBase
  writePlutusScriptNoTrace "Programmable Logic Global" (prod </> "programmableLogicGlobal.json") mkProgrammableLogicGlobal
  writePlutusScriptNoTrace "Issuance" (prod </> "programmableTokenMinting.json") mkProgrammableLogicMinting
  writePlutusScriptNoTrace "Protocol Parameters NFT" (prod </> "protocolParametersNFTMinting.json") mkProtocolParametersMinting
  writePlutusScriptNoTrace "Always Fail" (prod </> "alwaysFail.json") alwaysFailScript
  writePlutusScriptNoTrace "Permissioned Minting" (prod </> "permissionedMinting.json") mkPermissionedMinting
  writePlutusScriptNoTrace "Permissioned Transfer" (prod </> "permissionedTransfer.json") mkPermissionedTransfer
  writePlutusScriptNoTrace "Freeze and Seize Transfer" (prod </> "freezeAndSeizeTransfer.json") mkFreezeAndSeizeTransfer
  writePlutusScriptNoTrace "Directory Node Minting Policy" (prod </> "directoryNodeMintingPolicy.json") mkDirectoryNodeMP
  writePlutusScriptNoTrace "Directory Spending" (prod </> "directorySpending.json") pmkDirectorySpending
  writePlutusScriptNoTrace "Blacklist Spending" (prod </> "blacklistSpending.json") pmkBlacklistSpending

{-| Arguments for computing the applied scripts
-}
data AppliedScriptArgs =
  AppliedScriptArgs
    { asaTxIn          :: C.TxIn
    , asaIssuerCborHexTxIn :: C.TxIn
    , asaIssuerAddress :: SerialiseAddress (C.Address C.ShelleyAddr)
    }

data ExportCommand = ExportCommand
  { exBaseFolder    :: FilePath
  , exAppliedScript :: Maybe AppliedScriptArgs
  }

parseExportCommand :: Parser ExportCommand
parseExportCommand =
  ExportCommand
    <$> strArgument (help "The folder to write the scripts to" <> metavar "FOLDER")
    <*> optional parseAppliedScriptArgs

parseAppliedScriptArgs :: Parser AppliedScriptArgs
parseAppliedScriptArgs = AppliedScriptArgs <$> parseTxIn <*> parseTxIn <*> parseAddress

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
