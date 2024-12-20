{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

{-| servant server for stablecoin POC
-}
module Wst.Server(runServer) where

import Data.Data (Proxy (..))
import Network.Wai.Handler.Warp qualified as Warp
import Servant (Server)
import Servant.API ((:<|>) (..))
import Servant.Server (serve)
import Wst.Server.Endpoints (healthcheck, initMerkleTree, queryAddress,
                             queryAllSanctionedAddresses, transferToIssuer,
                             transferToUser, updateMerkleTree)
import Wst.Server.Types (API)

server :: Server API
server =
  healthcheck
  :<|> initMerkleTree
  :<|> updateMerkleTree
  :<|> transferToUser
  :<|> transferToIssuer
  :<|> queryAddress
  :<|> queryAllSanctionedAddresses

runServer :: IO ()
runServer = do
  let app = serve (Proxy @API) server
      port = 8081
  Warp.run port app

