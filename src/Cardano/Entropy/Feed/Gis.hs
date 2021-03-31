{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Entropy.Feed.Gis
  ( hashGis
  ) where

import Cardano.Entropy.Types.Gis    (GisOptions)
import Control.Lens                 ((^.))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Crypto.Hash
import Data.Function
import Data.Generics.Product.Any
import Data.Text                    (Text)
import Prelude                      hiding (lines)
import System.FilePath              ((</>))

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

hashGis :: GisOptions -> IO ()
hashGis opts = do
  let workspace     = opts ^. the @"workspace"
  let endTime       = opts ^. the @"endTime"
  let numHours      = opts ^. the @"numHours" & fromIntegral
  let startTime     = DT.addUTCTime (-numHours * 60 * 60) endTime
  let startTimeStr  = T.pack $ DT.showDateTime startTime
  let endTimeStr    = T.pack $ DT.showDateTime endTime

  downloadPath <- IO.createTempDirectory workspace "download"

  req <- BSS.parseUrlThrow "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.csv"

  monthCsvFile      <- pure $ downloadPath </> "all_month.csv"

  m <- BSS.newManager BSS.tlsManagerSettings
  IO.putStrLn $ "Writing to: " <> monthCsvFile

  runResourceT $ do
    resp <- BSS.http req m
    BSS.writeFile monthCsvFile $ BSS.responseBody resp

  text <- T.readFile monthCsvFile

  let lines = T.lines text

  dayInMonthCsvFile <- pure $ downloadPath </> "day_in_month.csv"

  T.putStrLn $ "Filtering within " <> startTimeStr <> " <= event < " <> endTimeStr <> " to: " <> T.pack dayInMonthCsvFile

  runResourceT $ do
    (_, hOut) <- IO.openFileOrStd dayInMonthCsvFile IO.WriteMode

    liftIO . forM_ (L.take 1 lines) $ T.hPutStrLn hOut

    let filteredLines :: [Text] = L.filter (ocBetween startTimeStr endTimeStr) (L.drop 1 lines)

    liftIO . forM_ filteredLines $ T.hPutStrLn hOut

  contents <- liftIO $ BS.readFile dayInMonthCsvFile
  liftIO . IO.putStrLn $ "Hash: " <> show (hashWith SHA256 contents)

  return ()

ocBetween :: Ord a => a -> a -> a -> Bool
ocBetween l r a = l <= a && a < r
