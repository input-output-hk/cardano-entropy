module App.Cli.Commands.Gis
  ( cmdGis
  ) where

import Options.Applicative

import qualified App.Options               as OPT
import qualified Cardano.Entropy.Feed.Gis  as ENT
import qualified Cardano.Entropy.Types.Gis as Z
import qualified Options.Applicative       as OPT

optsGis :: Parser Z.GisOptions
optsGis = Z.GisOptions
  <$> OPT.strOption
      (   OPT.long "workspace"
      <>  OPT.short 'w'
      <>  OPT.help "Workspace"
      <>  OPT.metavar "DIRECTORY"
      )
  <*> OPT.option OPT.readDateTime
      (   OPT.long "end-date-time"
      <>  OPT.short 'e'
      <>  OPT.help "End data time of the capture window."
      <>  OPT.metavar "UTC_TIME"
      )
  <*> OPT.option OPT.auto
      (   OPT.long "hours"
      <>  OPT.short 'h'
      <>  OPT.help "Length of the capture window."
      <>  OPT.metavar "HOURS"
      <>  OPT.value 36
      <>  OPT.showDefault
      )

cmdGis :: Mod CommandFields (IO ())
cmdGis = command "gis"  $ flip info idm $ ENT.hashGis <$> optsGis
