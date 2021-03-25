module App.Cli.Commands.GeolUoa
  ( cmdGeolUoa
  ) where

import Options.Applicative

import qualified App.Options                   as OPT
import qualified Cardano.Entropy.Feed.GeolUoa  as ENT
import qualified Cardano.Entropy.Types.GeolUoa as Z
import qualified Options.Applicative           as OPT

optsGeolUoa :: Parser Z.GeolUoaOptions
optsGeolUoa = Z.GeolUoaOptions
  <$> OPT.strOption
      (   OPT.long "workspace"
      <>  OPT.short 'w'
      <>  OPT.help "Workspace"
      <>  OPT.metavar "DIRECTORY"
      )
  <*> OPT.option OPT.readDateTime
      (   OPT.long "end-date-time"
      <>  OPT.short 'e'
      <>  OPT.help "End date time of the capture window.  Format: 'YYYY-MM-DDThh:mm:ss'"
      <>  OPT.metavar "UTC_DATE_TIME"
      )

cmdGeolUoa :: Mod CommandFields (IO ())
cmdGeolUoa = command "geol-uoa"  $ flip info idm $ ENT.hashGeolUoa <$> optsGeolUoa
