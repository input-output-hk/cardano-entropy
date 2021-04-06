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

import qualified Cardano.Entropy.IO                     as IO
import qualified Cardano.Entropy.Time                   as DT
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Builder                as B
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.ByteString.Streaming.HTTP         as BSS
import qualified Data.List                              as L
import qualified Data.Text                              as T
import qualified Data.Text.IO                           as T
import qualified Data.Time.Clock                        as DT
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor      as SVL
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor.Lazy as SVLL
import qualified Streaming.ByteString                   as BSS
import qualified System.IO                              as IO
import qualified System.IO.Temp                         as IO

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

  dirtyMonthCsvFile      <- pure $ downloadPath </> "dirty_all_month.csv"

  cleanMonthCsvFile      <- pure $ downloadPath </> "clean_all_month.csv"

  m <- BSS.newManager BSS.tlsManagerSettings
  IO.putStrLn $ "Writing to: " <> dirtyMonthCsvFile

  runResourceT $ do
    resp <- BSS.http req m
    BSS.writeFile dirtyMonthCsvFile $ BSS.responseBody resp

  IO.putStrLn $ "Cleaning to: " <> cleanMonthCsvFile

  bs <- LBS.readFile dirtyMonthCsvFile

  let c = SVL.makeCursor 44 bs -- 44 is ASCII for comma
  let dirtyRows = SVLL.toListList c
  let cleanRows = fmap (dropAtN 14) dirtyRows

  writeCsv cleanMonthCsvFile cleanRows

  text <- T.readFile cleanMonthCsvFile

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

writeCsv :: FilePath -> [[LBS.ByteString]] -> IO ()
writeCsv filePath rows = do
  hOut <- IO.openBinaryFile filePath IO.WriteMode

  forM_ rows $ \row -> do
    liftIO $ B.hPutBuilder hOut $ mconcat (L.intersperse (B.word8 44) (fmap B.lazyByteString row)) <> B.word8 10

  IO.hClose hOut

-- | Drop the nth element in the list
dropAtN :: Int -> [a] -> [a]
dropAtN n as = L.take (n - 1) as <> L.drop n as

ocBetween :: Ord a => a -> a -> a -> Bool
ocBetween l r a = l <= a && a < r
