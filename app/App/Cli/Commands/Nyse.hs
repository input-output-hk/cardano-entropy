module App.Cli.Commands.Nyse
  ( cmdNyse
  ) where

import Options.Applicative hiding (columns)

import qualified App.Options                as OPT
import qualified Cardano.Entropy.Feed.Nyse  as ENT
import qualified Cardano.Entropy.Types.Nyse as Z
import qualified Options.Applicative        as OPT

optsNyse :: Parser Z.NyseOptions
optsNyse = Z.NyseOptions
  <$> OPT.strOption
      (   OPT.long "workspace"
      <>  OPT.short 'w'
      <>  OPT.help "Workspace"
      <>  OPT.metavar "DIRECTORY"
      )
  <*> OPT.strOption
      (   OPT.long "username"
      <>  OPT.short 'u'
      <>  OPT.help "User name"
      <>  OPT.metavar "STRING"
      )
  <*> OPT.strOption
      (   OPT.long "password"
      <>  OPT.short 'p'
      <>  OPT.help "Password"
      <>  OPT.metavar "STRING"
      )
  <*> OPT.option OPT.readDate
      (   OPT.long "end-date-time"
      <>  OPT.short 'e'
      <>  OPT.help "End date time of the capture window.  Format: 'YYYY-MM-DDThh:mm:ss'"
      <>  OPT.metavar "UTC_DATE_TIME"
      )
  <*> OPT.option OPT.auto
      (   OPT.long "days"
      <>  OPT.short 'd'
      <>  OPT.help "Size of the capture window in days"
      <>  OPT.metavar "DAYS"
      <>  OPT.showDefault
      <>  OPT.value 2
      )
  <*> OPT.option OPT.auto
      (   OPT.long "headless"
      <>  OPT.help "Whether to run in headless mode"
      <>  OPT.metavar "BOOLEAN"
      <>  OPT.value True
      <>  OPT.showDefault
      )
  <*> OPT.option OPT.auto
      (   OPT.long "exit-delay"
      <>  OPT.help "Exit delay in seconds"
      <>  OPT.metavar "SECONDS"
      <>  OPT.value 0
      <>  OPT.showDefault
      )

cmdNyse :: Mod CommandFields (IO ())
cmdNyse = command "nyse"  $ flip info idm $ ENT.hashNyse <$> optsNyse
