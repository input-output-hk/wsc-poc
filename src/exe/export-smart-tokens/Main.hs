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
writePlutusScriptTraceBind = writePlutusScript (Tracing LogInfo DoTracingAndBinds)

writePlutusScriptTrace :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptTrace = writePlutusScript (Tracing LogInfo DoTracing)

writePlutusScriptNoTrace :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptNoTrace = writePlutusScript NoTracing

main :: IO ()
main = do
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
