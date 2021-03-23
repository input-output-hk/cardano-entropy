module Cardano.Entropy.IO
  ( openFileOrStd
  ) where

import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import System.IO                    (Handle, IOMode)

import qualified System.IO as IO

openFileOrStd :: MonadResource m => FilePath -> IOMode -> m (ReleaseKey, Handle)
openFileOrStd "-" IO.WriteMode = allocate (return IO.stdout) (const (return ()))
openFileOrStd "-" IO.ReadMode = allocate (return IO.stdin) (const (return ()))
openFileOrStd filePath ioMode = allocate (liftIO $ IO.openFile filePath ioMode) (liftIO . IO.hClose)
