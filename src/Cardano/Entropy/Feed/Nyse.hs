{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Entropy.Feed.Nyse
  ( hashNyse
  ) where

import Cardano.Entropy.Types.Nyse (NyseOptions)
import Control.Lens               ((^.))
import Control.Monad              (join)
import Control.Monad.IO.Class
import Control.Monad.Loops        (untilJust)
import Crypto.Hash
import Data.Aeson                 ((.=))
import Data.Bool                  (bool)
import Data.Function
import Data.Generics.Product.Any
import Data.Maybe                 (listToMaybe)

import qualified Cardano.Entropy.Time     as DT
import qualified Control.Concurrent       as IO
import qualified Control.Concurrent.Async as IO
import qualified Data.Aeson               as J
import qualified Data.ByteString          as BS
import qualified Data.HashMap.Strict      as HMS
import qualified Data.List                as L
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified System.Directory         as IO
import qualified System.IO                as IO
import qualified System.IO.Temp           as IO
import qualified Test.WebDriver           as WD

hashNyse :: NyseOptions -> IO ()
hashNyse opts = do
  let workspace         = opts ^. the @"workspace"
  let username          = opts ^. the @"username"
  let password          = opts ^. the @"password"
  let date              = opts ^. the @"date"
  let headless          = opts ^. the @"headless"
  let exitDelaySeconds  = opts ^. the @"exitDelay"

  IO.putStrLn $ show date <> " to " <> DT.showAmericanDate date

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
    WD.sendKeys (T.pack (DT.showAmericanDate date) <> "\t") dateInput

    liftIO $ IO.threadDelay 1000000

    messageText   <- WD.getText =<< WD.findElem (WD.ByCSS "span[id='ctl00_cph1_d1_lblMessage']")

    if T.null messageText
      then do
        WD.click =<< WD.findElem (WD.ByCSS "input[id='ctl00_cph1_d1_btnDownload']")

        result <- liftIO $ IO.race (delayReturn 10000000 TimeoutError) (waitDownloaded downloadPath)

        case result of
          Right csvFile -> do
            liftIO . IO.putStrLn $ "Downloaded: " <> csvFile
            contents <- liftIO $ BS.readFile csvFile
            liftIO . IO.putStrLn $ "Hash: " <> show (hashWith SHA256 contents)
          Left TimeoutError -> liftIO . IO.hPutStrLn IO.stderr $ "Timeout downloading data"

        liftIO $ IO.threadDelay (exitDelaySeconds * 1000000)
      else do
        liftIO $ T.hPutStrLn IO.stderr $ "Message: " <> messageText

    WD.closeSession

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
