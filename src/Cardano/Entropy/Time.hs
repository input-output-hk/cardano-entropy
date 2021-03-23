module Cardano.Entropy.Time
  ( parseDateTime
  , showDateTime
  , parseDate
  , showAmericanDate
  ) where

import Data.Time (Day, UTCTime)

import qualified Data.Time.Format as DT

parseDateTime :: String -> Maybe UTCTime
parseDateTime = DT.parseTimeM False DT.defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

showDateTime :: UTCTime -> String
showDateTime = DT.formatTime DT.defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

parseDate :: String -> Maybe Day
parseDate = DT.parseTimeM False DT.defaultTimeLocale "%Y-%m-%d"

showAmericanDate :: Day -> String
showAmericanDate = DT.formatTime DT.defaultTimeLocale "%m/%d/%Y"
