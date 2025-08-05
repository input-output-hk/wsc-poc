{-| Programmable tokens test
-}
module Main(
  main
) where

import ProgrammableTokens.Test.DirectorySet qualified as DirectorySet
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain DirectorySet.tests
