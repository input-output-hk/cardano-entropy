{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Entropy.Feed.GeolUoa
  ( hashGeolUoa
  ) where


import Cardano.Entropy.Types.GeolUoa (GeolUoaOptions)
import Control.Exception
import Control.Lens                  ((^.))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Crypto.Hash
import Data.Generics.Product.Any
import Data.Text                     (Text)
import Prelude                       hiding (lines)
import System.FilePath               ((</>))

import qualified Cardano.Entropy.IO             as IO
import qualified Cardano.Entropy.Time           as DT
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Streaming.HTTP as BSS
import qualified Data.List                      as L
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import qualified Data.Time.Clock                as DT
import qualified Streaming.ByteString           as BSS
import qualified System.IO                      as IO
import qualified System.IO.Temp                 as IO

hashGeolUoa :: GeolUoaOptions -> IO ()
hashGeolUoa opts = do
  let workspace     = opts ^. the @"workspace"
  let endTime       = opts ^. the @"endTime"
  let startTime     = DT.addUTCTime (-24 * 60 * 60) endTime
  let startTimeStr  = T.pack $ DT.showDateTime startTime
  let endTimeStr    = T.pack $ DT.showDateTime endTime

  downloadPath <- IO.createTempDirectory workspace "download-geol-uoa"

  let allYearFile      = downloadPath </> "all-year.txt"
  let lastDayFile = downloadPath </> "last-day.txt"

  req <- BSS.parseUrlThrow "http://www.geophysics.geol.uoa.gr/stations/gmaps3/event_output2j.php?type=cat"
  m <- BSS.newManager BSS.tlsManagerSettings
  IO.putStrLn $ "Writing to: " <> allYearFile

  runResourceT $ do
    resp <- BSS.http req m
    BSS.writeFile allYearFile $ BSS.responseBody resp

  text <- T.readFile allYearFile

  let lines = T.lines text

  T.putStrLn $ "Filtering within " <> startTimeStr <> " <= event < " <> endTimeStr <> " to: " <> T.pack lastDayFile

  runResourceT $ do
    (_, hOut) <- IO.openFileOrStd lastDayFile IO.WriteMode

    liftIO . forM_ (L.take 1 lines) $ T.hPutStrLn hOut
    let eventLines = L.drop 1 lines

    let timesWithUTC = map (\line -> (line, toUTC line)) eventLines
    let filteredLines :: [Text] = fst <$> L.filter (\(_line, utc) -> ocBetween startTimeStr endTimeStr utc) timesWithUTC
    when (null filteredLines) $
        liftIO $ throwIO $ userError "no entropy found. Try a different date"
    liftIO . forM_ filteredLines $ T.hPutStrLn hOut

  contents <- liftIO $ BS.readFile lastDayFile
  liftIO . IO.putStrLn $ "Hash: " <> show (hashWith SHA256 contents)

  return ()

toUTC :: Text -> Text
toUTC line =
  let parts = T.splitOn " " line
  in T.concat
        [parts !! 0, "-", parts !! 1, "-", parts !! 2, "T", parts !! 3, ":", parts !! 4, ":", T.take 2 (parts !! 5)]

ocBetween :: Ord a => a -> a -> a -> Bool
ocBetween l r a = l <= a && a < r
