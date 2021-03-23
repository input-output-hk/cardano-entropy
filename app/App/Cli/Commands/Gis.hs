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
      (   long "workspace"
      <>  short 'w'
      <>  help "Workspace"
      <>  metavar "DIRECTORY"
      )
  <*> OPT.option OPT.readTime
      (   long "end-date"
      <>  short 'e'
      <>  help "End Date"
      <>  metavar "UTC_TIME"
      )

cmdGis :: Mod CommandFields (IO ())
cmdGis = command "gis"  $ flip info idm $ ENT.hashGis <$> optsGis
