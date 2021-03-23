module App.Cli.Commands where

import App.Cli.Commands.GeolUoa
import App.Cli.Commands.Gis
import App.Cli.Commands.Nyse
import Options.Applicative

commands :: Parser (IO ())
commands = commandsGeneral

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdGis
  <>  cmdNyse
  <>  cmdGeolUoa
