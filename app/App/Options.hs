module App.Options
  ( readTime
  ) where

import Cardano.Entropy.Time
import Data.Time            (UTCTime)
import Options.Applicative  (ReadM, eitherReader)

readTime :: ReadM UTCTime
readTime = eitherReader go
  where go :: String -> Either String UTCTime
        go s = case parseTime s of
          Just t -> return t
          Nothing -> Left "Cannot parse time.  Expected: 'YYYY-mm-DDTHH:MM:SS'"
