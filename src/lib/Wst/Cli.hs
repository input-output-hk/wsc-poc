module Wst.Cli(runMain) where

import Options.Applicative (customExecParser, disambiguate, helper, idm, info,
                            prefs, showHelpOnEmpty, showHelpOnError)
import Wst.Cli.Command (Command, parseCommand)

runMain :: IO ()
runMain = do
  customExecParser
    (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
    (info (helper <*> parseCommand) idm)
    >>= runCommand

runCommand :: Command -> IO ()
runCommand com = do
  putStrLn "runCommand"
  print com
