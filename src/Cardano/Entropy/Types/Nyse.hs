{-# LANGUAGE DeriveGeneric #-}

module Cardano.Entropy.Types.Nyse
  ( NyseOptions(..)
  ) where

import Data.Text    (Text)
import GHC.Generics (Generic)

data NyseOptions = NyseOptions
  { workspace :: FilePath
  , username :: Text
  , password :: Text
  } deriving (Eq, Generic, Show)
