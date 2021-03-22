module App.Cli.Commands.Nyse
  ( cmdNyse
  ) where

import Control.Monad
import Options.Applicative hiding (columns)

import qualified Cardano.Entropy.Feed.Nyse  as ENT
import qualified Cardano.Entropy.Types.Nyse as Z
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Lazy       as LBS
import qualified System.Environment         as IO
import qualified System.IO                  as IO

optsNyse :: Parser Z.NyseOptions
optsNyse = Z.NyseOptions
  <$> strOption
      (   long "workspace"
      <>  short 'w'
      <>  help "Workspace"
      <>  metavar "DIRECTORY"
      )
  <*> strOption
      (   long "username"
      <>  short 'u'
      <>  help "User name"
      <>  metavar "STRING"
      )
  <*> strOption
      (   long "password"
      <>  short 'p'
      <>  help "Password"
      <>  metavar "STRING"
      )

cmdNyse :: Mod CommandFields (IO ())
cmdNyse = command "nyse"  $ flip info idm $ ENT.hashNyse <$> optsNyse
