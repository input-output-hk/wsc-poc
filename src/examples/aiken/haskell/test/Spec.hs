module Main (main) where

import Test.Tasty (defaultMain)
import Wst.Aiken.Test qualified as Unit

main :: IO ()
main = defaultMain Unit.tests
