{-# LANGUAGE TypeApplications #-}

{- | This module contains the client endpoints of the server.
-}
module Wst.Client (
  getHealthcheck,
  postInitMerkleTree,
  postUpdateMerkleTree,
  postTransferToUser,
  postTransferToIssuer,
  getAddress,
  getAllSanctionedAddresses
) where

import Servant.Client (ClientEnv, client, runClientM)
import Servant.Client.Core (ClientError)
import Servant.API (NoContent, (:<|>) ((:<|>)))
import Wst.Server.Types (API)
import Data.Data (Proxy(..))

getHealthcheck :: ClientEnv -> IO (Either ClientError NoContent)
getHealthcheck env = do
  let healthcheck :<|> _ = client (Proxy @API)
  runClientM healthcheck env

postInitMerkleTree :: ClientEnv -> String -> IO (Either ClientError String)
postInitMerkleTree env name = do
  let _ :<|> initMerkleTree :<|> _ = client (Proxy @API)
  runClientM (initMerkleTree name) env

postUpdateMerkleTree :: ClientEnv -> String -> IO (Either ClientError String)
postUpdateMerkleTree env name = do
  let _ :<|> _ :<|> updateMerkleTree :<|> _ = client (Proxy @API)
  runClientM (updateMerkleTree name) env

postTransferToUser :: ClientEnv -> String -> IO (Either ClientError String)
postTransferToUser env name = do
  let _ :<|> _ :<|> _ :<|> transferToUser :<|> _ = client (Proxy @API)
  runClientM (transferToUser name) env

postTransferToIssuer :: ClientEnv -> String -> IO (Either ClientError String)
postTransferToIssuer env name = do
  let _ :<|> _ :<|> _ :<|> _ :<|> transferToIssuer :<|> _ = client (Proxy @API)
  runClientM (transferToIssuer name) env

getAddress :: ClientEnv -> String -> IO (Either ClientError String)
getAddress env name = do
  let _ :<|> _ :<|> _ :<|> _ :<|> address :<|> _ = client (Proxy @API)
  runClientM (address name) env

getAllSanctionedAddresses :: ClientEnv -> IO (Either ClientError String)
getAllSanctionedAddresses env = do
  let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> allSanctionedAddresses = client (Proxy @API)
  runClientM allSanctionedAddresses env
