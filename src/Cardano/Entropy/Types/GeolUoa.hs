{-# LANGUAGE DeriveGeneric #-}

module Cardano.Entropy.Types.GeolUoa
  ( GeolUoaOptions(..)
  ) where

import Data.Time    (UTCTime)
import GHC.Generics (Generic)

data GeolUoaOptions = GeolUoaOptions
  { workspace :: FilePath
  , endTime   :: UTCTime
  } deriving (Eq, Generic, Show)
