{-| Convert an extended signing key to a mainnet address
-}
module Main (
  main
) where

import Cardano.Api qualified as C
import Convex.Wallet.Operator qualified as Operator
import Data.Text qualified as Text
import System.Environment qualified
import System.Exit (exitFailure)

main :: IO ()
main = System.Environment.getArgs >>= \case
  [fp] -> do
    putStrLn $ "Reading extended signing key from " <> fp
    C.readFileTextEnvelope (C.File fp) >>= \case
      Left err -> do
        putStrLn $ show err
        exitFailure
      Right key -> do
        let key'     = Operator.PESigningEx key
            operator = Operator.Operator key' Nothing
            addr     = Operator.operatorAddress C.Mainnet operator
        putStrLn $ Text.unpack $ C.serialiseAddress addr
  _ -> do
    putStrLn "Usage: convert-key PATH_TO_KEY"
    exitFailure
