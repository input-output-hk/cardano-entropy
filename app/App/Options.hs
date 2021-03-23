module App.Options
  ( readDate
  , readDateTime
  ) where

import Cardano.Entropy.Time (parseDate, parseDateTime)
import Data.Time            (UTCTime)
import Data.Time.Calendar   (Day)
import Options.Applicative  (ReadM, eitherReader)

readDateTime :: ReadM UTCTime
readDateTime = eitherReader go
  where go :: String -> Either String UTCTime
        go s = case parseDateTime s of
          Just t -> return t
          Nothing -> Left "Cannot parse time.  Expected: 'YYYY-mm-DDTHH:MM:SS'"

readDate :: ReadM Day
readDate = eitherReader go
  where go :: String -> Either String Day
        go s = case parseDate s of
          Just t -> return t
          Nothing -> Left "Cannot parse time.  Expected: 'YYYY-mm-DDTHH:MM:SS'"
