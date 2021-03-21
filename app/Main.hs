{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Function

import qualified Test.WebDriver as WD

chromeConfig :: WD.WDConfig
chromeConfig = WD.defaultConfig
  & WD.useBrowser WD.chrome

main :: IO ()
main = WD.runSession chromeConfig $ do
  WD.openPage "http://google.com"

  searchInput <- WD.findElem ( WD.ByCSS "input[type='text']" )

  WD.sendKeys "Hello, World!" searchInput
  WD.submit searchInput
  WD.closeSession
