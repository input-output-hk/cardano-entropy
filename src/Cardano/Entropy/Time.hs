module Cardano.Entropy.Time
  ( parseTime
  , showTime
  ) where

import Data.Time (UTCTime)

import qualified Data.Time.Format as DT

parseTime :: String -> Maybe UTCTime
parseTime = DT.parseTimeM False DT.defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

showTime :: UTCTime -> String
showTime = DT.formatTime DT.defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
