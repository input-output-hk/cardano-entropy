module App.Cli.Commands.JmaQuake
  ( cmdJmaQuake
  ) where

import Options.Applicative hiding (columns)

import qualified App.Options                    as OPT
import qualified Cardano.Entropy.Feed.JmaQuake  as ENT
import qualified Cardano.Entropy.Types.JmaQuake as Z
import qualified Options.Applicative            as OPT

optsJmaQuake :: Parser Z.JmaQuakeOptions
optsJmaQuake = Z.JmaQuakeOptions
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

cmdJmaQuake :: Mod CommandFields (IO ())
cmdJmaQuake = command "jma-quake"  $ flip info idm $ ENT.hashJmaQuake <$> optsJmaQuake
