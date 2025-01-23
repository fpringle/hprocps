module System.Proc.Tab
  ( ProcTab
  , withProcTab
  , ProcTabInfo (..)
  , getProcTabInfo
  , readProcTabInfo
  )
where

import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Ptr
import System.Proc.Error
import System.Proc.Flags
import System.Proc.Tab.Internal

getProcTabInfo :: ProcTab -> IO ProcTabInfo
getProcTabInfo (UnsafeProcTab ptr _) = fromProcTabC ptr

withProcTab :: TableConfig -> (ProcTab -> IO a) -> IO (Either ProcError a)
withProcTab cfg f =
  fmap join . withProcTabPtr cfg $ \procTabPtr -> do
    procPtr <- calloc
    traverse f $ mkProcTab procTabPtr procPtr

data ProcTabInfo = ProcTabInfo
  {
  }
  deriving (Show)

fromProcTabC :: Ptr ProcTabC -> IO ProcTabInfo
fromProcTabC _ptr = do
  pure ProcTabInfo

readProcTabInfo :: TableConfig -> IO (Either ProcError ProcTabInfo)
readProcTabInfo cfg =
  withProcTabPtr cfg fromProcTabC
