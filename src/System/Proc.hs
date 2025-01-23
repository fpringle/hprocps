module System.Proc
  ( -- * Process handles
    Proc

    -- ** Concrete process information
  , ProcInfo
  , procInfo
  , readNextProcInfo

    -- ** Process table
  , ProcTab
  , nextProc

    -- * One-shot API
  , readProcCPtrs
  , readProcInfos

    -- * Field accessors
  , SignalMask
  , Address
  , module Export
  )
where

import Control.Monad
import Data.Functor
import Data.Maybe
import Foreign
import System.Proc.C
import System.Proc.Error
import System.Proc.Flags
import System.Proc.Internal (Proc, ProcInfo (..), makeProc)
import System.Proc.Internal as Export hiding (Proc (..), ProcInfo (..), fromProcC, makeProc)
import qualified System.Proc.Internal as Internal
import System.Proc.Tab.Internal

procInfo :: Proc -> IO ProcInfo
procInfo (Internal.UnsafeProc ptr) = Internal.fromProcC ptr

nextProc :: ProcTab -> IO (Either ProcError Proc)
nextProc (UnsafeProcTab procTabPtr procPtr') = do
  procPtr <- readNextProc procTabPtr procPtr'
  pure $ Internal.makeProc procPtr

-- readNextProcInfo = nextProc >=> traverse procInfo
readNextProcInfo :: ProcTab -> IO (Either ProcError ProcInfo)
readNextProcInfo (UnsafeProcTab procTabPtr procPtr') = do
  procPtr <- readNextProc procTabPtr procPtr'
  if procPtr == nullPtr
    then pure $ Left NullPtrError
    else Right <$> Internal.fromProcC procPtr

readProcTab' :: TableConfig -> IO (Ptr (Ptr ProcC))
readProcTab' =
  branchTableConfig readProcTabSimple readProcTabPids readProcTabUids

readProcInfos' :: Ptr (Ptr ProcC) -> IO [ProcInfo]
readProcInfos' = readProcCPtrs' >=> traverse (fmap ProcInfo . readProcCInfo)

readProcCPtrs' :: Ptr (Ptr ProcC) -> IO [Ptr ProcC]
readProcCPtrs' ptr
  | ptr == nullPtr = pure []
  | otherwise = peekArray0 nullPtr ptr

readProcInfos :: TableConfig -> IO [ProcInfo]
readProcInfos = readProcTab' >=> readProcInfos'

readProcCPtrs :: TableConfig -> IO [Proc]
readProcCPtrs cfg =
  readProcTab' cfg
    >>= readProcCPtrs'
    <&> mapMaybe (rightToMaybe . makeProc)
  where
    rightToMaybe = either (const Nothing) Just
