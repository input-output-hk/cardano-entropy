{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Entropy.Feed.JmaQuake
  ( hashJmaQuake
  ) where

import Cardano.Entropy.Types.JmaQuake (JmaQuakeOptions)
import Control.Lens
import Control.Monad.IO.Class         (liftIO)
import Control.Monad.Trans.Resource
import Crypto.Hash
import Data.Aeson                     (Value)
import Data.Generics.Product.Any
import Data.Time.Clock                (UTCTime)
import System.FilePath                ((</>))

import qualified Cardano.Entropy.Time           as DT
import qualified Data.Aeson                     as J
import qualified Data.Aeson.Lens                as J
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Streaming.HTTP as BSS
import qualified Data.List                      as L
import qualified Data.Text                      as T
import qualified Data.Time.Clock                as DT
import qualified Data.Time.Clock.POSIX          as DT
import qualified Streaming.ByteString           as BSS
import qualified System.Exit                    as IO
import qualified System.IO                      as IO
import qualified System.IO.Temp                 as IO

hashJmaQuake :: JmaQuakeOptions -> IO ()
hashJmaQuake opts = do
  let workspace   = opts ^. the @"workspace"
  let hours       = opts ^. the @"hours"
  let endDateTime = opts ^. the @"endDateTime"

  let startDateTime   = DT.addUTCTime (-DT.secondsToNominalDiffTime (hours * 60 * 60)) endDateTime
  let nowPosixMillis  = floor $ DT.utcTimeToPOSIXSeconds endDateTime * 1000

  let uri = "https://www.data.jma.go.jp/multi/data/VXSE53/en.json?_=" <> show @Int nowPosixMillis

  IO.putStrLn $ "Time window: " <> show startDateTime <> " <= t < " <> show endDateTime

  IO.putStrLn $ "Downloading: " <> show uri

  downloadPath <- IO.createTempDirectory workspace "download-jma-quake"
  req <- BSS.parseUrlThrow uri

  m <- BSS.newManager BSS.tlsManagerSettings

  latestEventsFile <- pure $ downloadPath </> "latest.json"
  selectedEventsFile <- pure $ downloadPath </> "selected.json"

  runResourceT $ do
    resp <- BSS.http req m
    liftIO . IO.putStrLn $ "Downloaded to " <> latestEventsFile
    BSS.writeFile latestEventsFile $ BSS.responseBody resp

  contents <- LBS.readFile latestEventsFile

  case J.eitherDecode @Value contents of
    Left msg -> do
      IO.hPutStrLn IO.stderr $ "Error: " <> msg
      IO.exitFailure
    Right v -> do
      let events = v ^.. J.key "report" . J._Array . each
      let selectedEvents = L.filter (inRange startDateTime endDateTime) events
      IO.putStrLn $ "Filtered to " <> selectedEventsFile
      let selectedContents = J.encode selectedEvents
      LBS.writeFile selectedEventsFile selectedContents
      liftIO . IO.putStrLn $ "Hash: " <> show (hashWith SHA256 (LBS.toStrict selectedContents))

inRange :: UTCTime -> UTCTime -> Value -> Bool
inRange a z v = case v ^? J.key "reportDateTime" . J._String . to T.unpack . to DT.parseOldDateTime . _Just of
  Just t -> a <= t && t < z
  Nothing -> False
