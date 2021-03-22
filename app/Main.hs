module Main where
import App.Cli.Commands
import Control.Monad
import Options.Applicative

import qualified System.Environment as IO

main :: IO ()
main = do
  join $ customExecParser
    (prefs $ showHelpOnEmpty <> showHelpOnError)
    (info (commands <**> helper) idm)
