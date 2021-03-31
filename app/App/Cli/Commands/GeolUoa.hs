module App.Cli.Commands.GeolUoa
  ( cmdGeolUoa
  ) where

import Options.Applicative

import qualified App.Options                   as OPT
import           Data.Foldable
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
  <*> mParser (OPT.option OPT.readDateTime
      (   OPT.long "end-date"
      <>  OPT.short 'e'
      <>  OPT.help "End data time of the capture window. Defaults to now"
      <>  OPT.metavar "UTC_TIME"
      ))
  <*> mParser (OPT.option OPT.auto
      (   OPT.long "hours"
      <>  OPT.short 'h'
      <>  OPT.help "Length of the capture window. Defaults to 36"
      <>  OPT.metavar "HOURS"
      ))

mParser :: Parser a -> Parser (Maybe a)
mParser parser = asum [Just <$> parser, pure Nothing]

cmdGeolUoa :: Mod CommandFields (IO ())
cmdGeolUoa = command "geol-uoa"  $ flip info idm $ ENT.hashGeolUoa <$> optsGeolUoa
