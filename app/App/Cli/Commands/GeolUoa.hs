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
      (   OPT.long "end-date"
      <>  OPT.short 'e'
      <>  OPT.help "End Date"
      <>  OPT.metavar "UTC_TIME"
      )

cmdGeolUoa :: Mod CommandFields (IO ())
cmdGeolUoa = command "geol-uoa"  $ flip info idm $ ENT.hashGeolUoa <$> optsGeolUoa
