{-# LANGUAGE DeriveGeneric #-}

module Cardano.Entropy.Types.JmaQuake
  ( JmaQuakeOptions(..)
  ) where

import Data.Time.Clock (UTCTime)
import GHC.Generics    (Generic)

data JmaQuakeOptions = JmaQuakeOptions
  { workspace   :: FilePath
  , endDateTime :: UTCTime
  , numHours    :: Int
  } deriving (Eq, Generic, Show)
