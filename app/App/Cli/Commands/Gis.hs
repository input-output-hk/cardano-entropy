module App.Cli.Commands.Gis
  ( cmdGis
  ) where

import Control.Monad
import Options.Applicative

import qualified App.Options               as OPT
import qualified Cardano.Entropy.Feed.Gis  as ENT
import qualified Cardano.Entropy.Types.Gis as Z
import qualified Data.ByteString.Builder   as B
import qualified Data.ByteString.Lazy      as LBS
import qualified Options.Applicative       as OPT
import qualified System.Environment        as IO
import qualified System.IO                 as IO

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
