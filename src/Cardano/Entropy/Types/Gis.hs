{-# LANGUAGE DeriveGeneric #-}

module Cardano.Entropy.Types.Gis
  ( GisOptions(..)
  ) where

import Data.Time    (UTCTime)
import GHC.Generics (Generic)

data GisOptions = GisOptions
  { workspace :: FilePath
  , endTime   :: UTCTime
  } deriving (Eq, Generic, Show)
