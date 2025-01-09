module Main where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BSL
import Data.Proxy (Proxy (..))
import Servant.OpenApi (toOpenApi)
import System.Environment qualified
import Wst.Server.Types (APIInEra)

main :: IO ()
main = System.Environment.getArgs >>= \case
  [fp] -> do
    putStrLn $ "Writing OpenAPI schema to " <> fp
    BSL.writeFile fp $ encodePretty $ toOpenApi $ Proxy @APIInEra
  _    -> putStrLn "usage: write-openapi-schema OUT_FILE_PATH"

