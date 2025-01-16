{-# LANGUAGE OverloadedStrings #-}
{-| A single route that returns the blockfrost API key used on the backend.
(not ideal to expose it like this, but we are pressed for time!)
-}
module Wst.Server.BlockfrostKey(
  BlockfrostKey,
  runBlockfrostKey
) where

import Data.Text (Text)
import Servant.API (Get, JSON, (:>))
import Servant.Server (ServerT)

{-| Blockfrost key route
-}
type BlockfrostKey = "blockfrost-key" :> Get '[JSON] Text

runBlockfrostKey :: Applicative m => Text -> ServerT BlockfrostKey m
runBlockfrostKey = pure
