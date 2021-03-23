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

cmdGeolUoa :: Mod CommandFields (IO ())
cmdGeolUoa = command "geol-uoa"  $ flip info idm $ ENT.hashGeolUoa <$> optsGeolUoa
