module Main where

import App.Cli.Commands
import Control.Monad
import Options.Applicative

main :: IO ()
main = do
  join $ customExecParser
    (prefs $ showHelpOnEmpty <> showHelpOnError)
    (info (commands <**> helper) idm)
