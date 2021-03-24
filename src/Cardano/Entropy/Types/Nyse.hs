{-# LANGUAGE DeriveGeneric #-}

module Cardano.Entropy.Types.Nyse
  ( NyseOptions(..)
  ) where

import Data.Text    (Text)
import Data.Time    (Day)
import GHC.Generics (Generic)

data NyseOptions = NyseOptions
  { workspace :: FilePath
  , username  :: Text
  , password  :: Text
  , endDate   :: Day
  , numDays   :: Int
  , headless  :: Bool
  , exitDelay :: Int
  } deriving (Eq, Generic, Show)
