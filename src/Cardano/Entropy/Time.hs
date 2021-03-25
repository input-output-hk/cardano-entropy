module Cardano.Entropy.Time
  ( parseDateTime
  , showDateTime
  , parseDate
  , parseOldDateTime
  , showAmericanDate

  , nominalSecond
  , nominalMinute
  , nominalHour
  , DT.nominalDay
  ) where

import Data.Time (Day, NominalDiffTime, UTCTime)

import qualified Data.Time.Clock  as DT
import qualified Data.Time.Format as DT

parseDateTime :: String -> Maybe UTCTime
parseDateTime = DT.parseTimeM False DT.defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

showDateTime :: UTCTime -> String
showDateTime = DT.formatTime DT.defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

parseDate :: String -> Maybe Day
parseDate = DT.parseTimeM False DT.defaultTimeLocale "%Y-%m-%d"

showAmericanDate :: Day -> String
showAmericanDate = DT.formatTime DT.defaultTimeLocale "%m/%d/%Y"

nominalSecond :: NominalDiffTime
nominalSecond = DT.secondsToNominalDiffTime 1

nominalMinute :: NominalDiffTime
nominalMinute = DT.secondsToNominalDiffTime 60

nominalHour :: NominalDiffTime
nominalHour = DT.secondsToNominalDiffTime (60 * 60)

parseOldDateTime :: String -> Maybe UTCTime
parseOldDateTime = DT.parseTimeM False DT.defaultTimeLocale "%Y/%m/%d %H:%M"
