{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TypeApplications #-}

{-| servant server for stablecoin POC
-}
module Wst.Server(runServer) where

import Servant (Server)
import Wst.Server.Types (API)
import Servant.Server (serve)
import Data.Data (Proxy(..))
import qualified Network.Wai.Handler.Warp as Warp
import Servant.API ((:<|>) (..))
import Wst.Server.Endpoints (healthcheck, initMerkleTree, updateMerkleTree, transferToUser, transferToIssuer, queryAddress, queryAllSanctionedAddresses)

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

