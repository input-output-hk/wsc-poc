{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Cardano.Binary qualified as CBOR
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (first)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text, pack)
import Data.Text.Encoding qualified as Text
import Plutarch (Config (..), LogLevel (..), TracingMode (..), compile)
import Plutarch.Evaluate (applyArguments, evalScript)
import Plutarch.Prelude
import Plutarch.Script (Script, serialiseScript)
import PlutusLedgerApi.V2 (Data, ExBudget)
import SmartTokens.Contracts.ExampleTransferLogic (mkFreezeAndSeizeTransfer,
                                                   mkPermissionedTransfer)
import SmartTokens.Contracts.Issuance (mkProgrammableLogicMinting)
import SmartTokens.Contracts.ProgrammableLogicBase (mkProgrammableLogicBase,
                                                    mkProgrammableLogicGlobal)
import SmartTokens.Contracts.ProtocolParams (alwaysFailScript,
                                             mkPermissionedMinting,
                                             mkProtocolParametersMinting)
import SmartTokens.LinkedList.MintDirectory (mkDirectoryNodeMP)
import SmartTokens.LinkedList.SpendBlacklist (pmkBlacklistSpending)
import SmartTokens.LinkedList.SpendDirectory (pmkDirectorySpending)

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
writePlutusScriptTraceBind title filepath term =
  writePlutusScript (Tracing LogInfo DoTracingAndBinds) title filepath term

main :: IO ()
main = do
  putStrLn "Writing Plutus Scripts to files"
  writePlutusScriptTraceBind "Programmable Logic Base" "./compiled/programmableLogicBase.json" mkProgrammableLogicBase
  writePlutusScriptTraceBind "Programmable Logic Global" "./compiled/programmableLogicBase.json" mkProgrammableLogicGlobal
  writePlutusScriptTraceBind "Issuance" "./compiled/programmableTokenMinting.json" mkProgrammableLogicMinting
  writePlutusScriptTraceBind "Protocol Parameters NFT" "./compiled/protocolParametersNFTMinting.json" mkProtocolParametersMinting
  writePlutusScriptTraceBind "Always Fail" "./compiled/alwaysFail.json" alwaysFailScript
  writePlutusScriptTraceBind "Permissioned Minting" "./compiled/permissionedMinting.json" mkPermissionedMinting
  writePlutusScriptTraceBind "Permissioned Transfer" "./compiled/permissionedTransfer.json" mkPermissionedTransfer
  writePlutusScriptTraceBind "Freeze and Seize Transfer" "./compiled/freezeAndSeizeTransfer.json" mkFreezeAndSeizeTransfer
  writePlutusScriptTraceBind "Directory Node Minting Policy" "./compiled/directoryNodeMintingPolicy.json" mkDirectoryNodeMP
  writePlutusScriptTraceBind "Directory Spending" "./compiled/directorySpending.json" pmkDirectorySpending
  writePlutusScriptTraceBind "Blacklist Spending" "./compiled/blacklistSpending.json" pmkBlacklistSpending
