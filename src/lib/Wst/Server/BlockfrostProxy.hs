{-# LANGUAGE OverloadedStrings #-}
{-| Blockfrost proxy that injects the auth header with the API key
into all requests
-}
module Wst.Server.BlockfrostProxy(
  BlockfrostProxy,
  runBlockfrostProxy
) where

import Control.Exception (bracket_)
import Data.ByteString.Char8 qualified as B8
import Data.CaseInsensitive qualified as CI
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Client (Manager, Request (..), httpLbs, parseRequest,
                            responseBody, responseHeaders, responseStatus)
import Network.Wai (Response)
import Network.Wai qualified as Wai
import Servant.API (Raw, (:>))
import Servant.Server (ServerT)

{-| Blockfrost proxy route
-}
type BlockfrostProxy = "blockfrost-proxy" :> Raw

type Header = (CI.CI B8.ByteString, B8.ByteString)

targetUrl :: B8.ByteString
targetUrl = "https://cardano-preview.blockfrost.io/"

runBlockfrostProxy :: Manager -> Text -> ServerT Raw m
runBlockfrostProxy manager t = do
  let header :: Header
      header = ("Project_id", B8.pack $ Text.unpack t)

  pure $ \req respond ->
    bracket_
      (pure ())
      (pure ())
      (handler manager header req respond)

handler :: Manager -> Header -> Wai.Request -> (Response -> IO w) -> IO w
handler manager hd request respond = do
  initialRequest <- parseRequest $ B8.unpack targetUrl
  let path_ = Wai.rawPathInfo request <> Wai.rawQueryString request
      newRequest = initialRequest
        { method = Wai.requestMethod request
        , path = path_
        , requestHeaders = hd : requestHeaders initialRequest
        , requestBody = requestBody initialRequest
        }

  putStrLn $ "BLOCKFROST PROXY: New path: " <> B8.unpack path_
  response <- httpLbs newRequest manager
  respond $ Wai.responseLBS
    (responseStatus response)
    (responseHeaders response)
    (responseBody response)
