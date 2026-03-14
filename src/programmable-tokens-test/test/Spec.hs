-- | Programmable tokens test
module Main (
    main,
) where

import ProgrammableTokens.Test.DirectorySet qualified as DirectorySet
import ProgrammableTokens.Test.ProgrammableLogicGlobal qualified as ProgrammableLogicGlobal
import ProgrammableTokens.Test.ProgrammableLogicMinting qualified as ProgrammableLogicMinting
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
        testGroup
            "programmable-tokens"
            [ DirectorySet.tests
            , ProgrammableLogicGlobal.tests
            , ProgrammableLogicMinting.tests
            ]
