{-# LANGUAGE DeriveGeneric #-}

module Cardano.Entropy.Types.GeolUoa
  ( GeolUoaOptions(..)
  ) where

import Data.Time    (UTCTime)
import GHC.Generics (Generic)

data GeolUoaOptions = GeolUoaOptions
  { workspace  :: FilePath
  , endTime    :: Maybe UTCTime
  , timeWindow :: Maybe Int
  } deriving (Eq, Generic, Show)
