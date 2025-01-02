module Main where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BSL
import Data.Proxy (Proxy (..))
import Servant.OpenApi (toOpenApi)
import System.Environment qualified
import Wst.Server.Types (APIInEra)

main :: IO ()
main = do
  fp:_ <- System.Environment.getArgs
  BSL.writeFile fp $ encodePretty $ toOpenApi $ Proxy @APIInEra
