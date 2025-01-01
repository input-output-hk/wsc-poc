module Main (main) where

import Test.Tasty (defaultMain)

import Wst.Test.UnitTest qualified as Unit

main :: IO ()
main = defaultMain Unit.tests
