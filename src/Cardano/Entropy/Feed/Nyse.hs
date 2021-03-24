{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Entropy.Feed.Nyse
  ( hashNyse
  ) where

import Cardano.Entropy.Types.Nyse (NyseOptions)
import Control.Lens               ((^.))
import Control.Monad              (forM, join)
import Control.Monad.IO.Class
import Control.Monad.Loops        (untilJust)
import Crypto.Hash
import Data.Aeson                 ((.=))
import Data.Bool                  (bool)
import Data.Function
import Data.Generics.Product.Any
import Data.Maybe                 (listToMaybe)
import System.FilePath            ((</>))

import qualified Cardano.Entropy.Time     as DT
import qualified Control.Concurrent       as IO
import qualified Control.Concurrent.Async as IO
import qualified Data.Aeson               as J
import qualified Data.ByteString          as BS
import qualified Data.HashMap.Strict      as HMS
import qualified Data.List                as L
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Data.Time                (Day)
import qualified Data.Time.Calendar       as DT
import qualified System.Directory         as IO
import qualified System.Exit              as IO
import qualified System.IO                as IO
import qualified System.IO.Temp           as IO
import qualified Test.WebDriver           as WD

hashNyse :: NyseOptions -> IO ()
hashNyse opts = do
  let workspace         = opts ^. the @"workspace"
  let endDate           = opts ^. the @"endDate"
  let numDays           = opts ^. the @"numDays"

  let startTime         = DT.addDays (-fromIntegral (numDays - 1)) endDate
  let days              = [startTime .. endDate]

  IO.putStrLn $ "Dates to download and proccess: " <> show [startTime .. endDate]

  csvFiles <- fmap join    . forM days      $ downloadDay opts
  contents <- fmap mconcat . forM csvFiles  $ BS.readFile

  if BS.null contents
    then do
      IO.hPutStrLn IO.stderr "No data avilable"
      IO.exitFailure
    else do
      processingPath  <- IO.createTempDirectory workspace "processing"

      let dataFile = processingPath </> "data.csv"

      IO.putStrLn $ "Writing: " <> dataFile
      BS.writeFile dataFile contents

      liftIO . IO.putStrLn $ "Hash: " <> show (hashWith SHA256 contents)

downloadDay :: NyseOptions -> Day -> IO [FilePath]
downloadDay opts day = do
  let workspace         = opts ^. the @"workspace"
  let headless          = opts ^. the @"headless"
  let username          = opts ^. the @"username"
  let password          = opts ^. the @"password"
  let exitDelaySeconds  = opts ^. the @"exitDelay"

  downloadPath  <- IO.createTempDirectory workspace "download"

  chromeConfig <- pure $ WD.defaultConfig
    & WD.useBrowser
      ( WD.chrome
        { WD.chromeOptions = join
          [ bool [] ["--headless"] headless
          ]
        , WD.chromeExperimentalOptions = HMS.fromList
          [ "prefs" .= J.object
            [ ("download.default_directory", J.toJSON @String downloadPath)
            ]
          ]
        }
      )

  WD.runSession chromeConfig $ do
    WD.openPage "http://www.eoddata.com/download.aspx"

    usernameInput <- WD.findElem $ WD.ByCSS "input[id='ctl00_cph1_ls1_txtEmail']"
    passwordInput <- WD.findElem $ WD.ByCSS "input[id='ctl00_cph1_ls1_txtPassword']"

    WD.sendKeys username usernameInput

    WD.sendKeys (password <> "\n") passwordInput

    WD.openPage "http://www.eoddata.com/download.aspx"

    nyseOption    <- WD.findElem $ WD.ByCSS "option[value='NYSE']"
    dateInput     <- WD.findElem $ WD.ByCSS "input[id='ctl00_cph1_d1_txtEndDate']"

    WD.click nyseOption
    WD.clearInput dateInput
    WD.sendKeys (T.pack (DT.showAmericanDate day) <> "\t") dateInput

    liftIO $ IO.threadDelay 1000000

    messageText   <- WD.getText =<< WD.findElem (WD.ByCSS "span[id='ctl00_cph1_d1_lblMessage']")

    result :: [FilePath] <- if T.null messageText
      then do
        WD.click =<< WD.findElem (WD.ByCSS "input[id='ctl00_cph1_d1_btnDownload']")

        result <- liftIO $ IO.race (delayReturn 10000000 TimeoutError) (waitDownloaded downloadPath)

        case result of
          Right csvFile -> do
            liftIO . IO.putStrLn $ "Downloaded: " <> csvFile
            return [csvFile]
          Left TimeoutError -> do
            liftIO . IO.hPutStrLn IO.stderr $ "Warning: Unable to download data for " <> show day
            liftIO . IO.hPutStrLn IO.stderr $ "Reason: Timeout downloading data"
            return []

      else do
        liftIO . IO.hPutStrLn IO.stderr $ "Warning: Unable to download data for " <> show day
        liftIO .  T.hPutStrLn IO.stderr $ "Reason: " <> messageText
        return []

    liftIO $ IO.threadDelay (exitDelaySeconds * 1000000)
    WD.closeSession

    return result


data TimeoutError = TimeoutError deriving (Eq, Show)

delayReturn :: Int -> a -> IO a
delayReturn timeout a = IO.threadDelay timeout >> return a

waitDownloaded :: FilePath -> IO FilePath
waitDownloaded downloadPath = untilJust $ do
  directoryContents <- liftIO $ IO.listDirectory downloadPath

  let filteredDirectoryContents = L.filter (not . L.isSuffixOf ".crdownload") directoryContents

  case listToMaybe filteredDirectoryContents of
    Just file -> return $ Just (downloadPath <> "/" <> file)
    Nothing -> do
      liftIO $ IO.threadDelay 500000
      return Nothing
