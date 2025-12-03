{-# LANGUAGE OverloadedStrings #-}
{-| Calculate the script hashes for a WST deployment
-}
module Main (main) where

import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.BaseTypes qualified as Ledger
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import System.Environment qualified
import System.Exit (exitFailure)
import Wst.Offchain.Env (DirectoryScriptRoot (..))
import Wst.Offchain.Env qualified as Env

main :: IO ()
main = System.Environment.getArgs >>= \case
  [fp, addr] -> do
    putStrLn $ "Calculating hashes using " <> fp <> " with adddress " <> addr
    operator <- decodeAddress addr
    let stakeCred = case operator of
          (C.ShelleyAddress _ntw _pmt (C.fromShelleyStakeReference -> C.StakeAddressByValue sCred)) -> Just sCred
          _ -> Nothing
    (nid, pkh) <- paymentHashAndNetworkId operator
    dirEnv <- Env.mkDirectoryEnv <$> loadFromFile fp
    let scriptRoot =
          Env.BlacklistTransferLogicScriptRoot
            (srTarget $ Env.dsScriptRoot dirEnv)
            dirEnv
            pkh
            stakeCred
    let transferLogicEnv = Env.mkTransferLogicEnv scriptRoot
        blacklistEnv     = Env.mkBlacklistEnv scriptRoot

    printAssetId dirEnv transferLogicEnv "WST"
    printTransferLogicAddress nid blacklistEnv
    printBaseCredential dirEnv
  _ -> do
    putStrLn "Usage: calculate-hashes DEPLOYMENT_ROOT_FILE_PATH ISSUER_ADDRESS"
    exitFailure

loadFromFile :: FilePath -> IO DirectoryScriptRoot
loadFromFile fp = do
  fmap Aeson.decode (BSL.readFile fp) >>= \case
    Nothing -> error "failed to decode JSON"
    Just a  -> pure a

decodeAddress :: String -> IO (C.Address C.ShelleyAddr)
decodeAddress str = case C.deserialiseAddress (C.proxyToAsType Proxy) (Text.pack str) of
  Nothing -> error "failed to deserialise address"
  Just a  -> pure a

paymentHashAndNetworkId :: C.Address C.ShelleyAddr -> IO (C.NetworkId, C.Hash C.PaymentKey)
paymentHashAndNetworkId = \case
  (C.ShelleyAddress network (C.fromShelleyPaymentCredential -> C.PaymentCredentialByKey pkh) _) -> pure (fromLedgerNetwork network, pkh)
  _ -> error "expected public key address"

printAssetId :: Env.DirectoryEnv -> Env.TransferLogicEnv -> C.AssetName -> IO ()
printAssetId dirEnv transferLogicEnv =
  print . Env.programmableTokenAssetId dirEnv transferLogicEnv

printTransferLogicAddress :: C.NetworkId -> Env.BlacklistEnv -> IO ()
printTransferLogicAddress nid env = do
  let spendingScript = Env.bleSpendingScript env
      spendingHash = C.hashScript $ C.PlutusScript C.PlutusScriptV3 spendingScript
      addr = C.makeShelleyAddressInEra C.ShelleyBasedEraConway nid (C.PaymentCredentialByScript spendingHash) C.NoStakeAddress
  print $ C.serialiseAddress addr

fromLedgerNetwork :: Ledger.Network -> C.NetworkId
fromLedgerNetwork = \case
  Ledger.Mainnet -> C.Mainnet
  Ledger.Testnet -> C.Testnet (C.NetworkMagic 42) -- ???

printBaseCredential :: Env.DirectoryEnv -> IO ()
printBaseCredential =
  print . Env.programmableLogicBaseCredential

-- token_name
-- prog_logic_base_hash

  -- spendingScript <- asks (Env.tleBlacklistSpendingScript . Env.transferLogicEnv)
  -- let policyId = scriptPolicyIdV3 mintingScript
  --     spendingHash = C.hashScript $ C.PlutusScript C.PlutusScriptV3 spendingScript
  --     addr = C.makeShelleyAddressInEra C.shelleyBasedEra nid (C.PaymentCredentialByScript spendingHash) C.NoStakeAddress
