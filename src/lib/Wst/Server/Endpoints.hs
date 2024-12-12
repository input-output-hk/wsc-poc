{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TypeApplications #-}

{- | This module contains the endpoints of the server.
-}
module Wst.Server.Endpoints (
  healthcheck,
  initMerkleTree,
  updateMerkleTree,
  transferToUser,
  transferToIssuer,
  queryAddress,
  queryAllSanctionedAddresses
) where
import Servant.API (NoContent (..))
import Servant (Handler)

healthcheck :: Handler NoContent
healthcheck = pure NoContent

initMerkleTree :: String -> Handler String
initMerkleTree _arg = pure "initMerkleTree"

updateMerkleTree :: String -> Handler String
updateMerkleTree _arg = pure "updateMerkleTree"

transferToUser :: String -> Handler String
transferToUser _arg = pure "transferToUser"

transferToIssuer :: String -> Handler String
transferToIssuer _arg = pure "transferToIssuer"

queryAddress :: String -> Handler String
queryAddress name = pure name

queryAllSanctionedAddresses :: Handler String
queryAllSanctionedAddresses = pure "allSanctionedAddresses"
