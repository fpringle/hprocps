module System.Proc.Bindings
  ( -- * Process handles
    Proc
  , closeProc

    -- ** Read the current process
  , withSelfProc
  , openSelfProc

    -- ** Read all processes
  , withAllProcs
  , withAllProcsEither
  , withAllProcsLenient
  , openAllProcs
  , openAllProcsLenient

    -- * Process table
  , ProcTab
  , readNextProc
  , readAllProcInfos
  , readAllProcInfosE

    -- * Concrete process information
  , procInfo
  , readNextProcInfo
  , module System.Proc.Bindings.Info
  )
where

import Control.Exception
import Control.Monad
import Data.Either
import Data.Foldable
import Foreign
import GHC.Stack
import System.Proc.Bindings.C
import System.Proc.Bindings.C.Utils
import System.Proc.Bindings.Error
import System.Proc.Bindings.Info
import System.Proc.Bindings.Internal (Proc)
import qualified System.Proc.Bindings.Internal as Internal
import System.Proc.Bindings.Tab
import System.Proc.Bindings.Tab.Config
import System.Proc.Bindings.Tab.Internal

-- | Read concrete 'ProcInfo' from a 'Proc' handle.
procInfo :: HasCallStack => Proc -> IO ProcInfo
procInfo (Internal.UnsafeProc ptr) = Internal.fromProcC ptr

-- | Read the next 'Proc' from the 'ProcTab'.
readNextProc :: HasCallStack => ProcTab -> IO (Either ProcError Proc)
readNextProc (UnsafeProcTab procTabPtr procPtr') = do
  procPtr <- readNextProcC procTabPtr procPtr'
  pure $ Internal.makeProc procPtr

{- | Read the next 'ProcInfo' in the 'ProcTab'.
Combines 'readNextProc' and 'procInfo' for convenience.
-}
readNextProcInfo :: HasCallStack => ProcTab -> IO (Either ProcError ProcInfo)
readNextProcInfo (UnsafeProcTab procTabPtr procPtr') = do
  procPtr <- readNextProcC procTabPtr procPtr'
  eitherPeek Internal.fromProcC procPtr

readProcTab' :: HasCallStack => TableConfig -> IO (Ptr (Ptr ProcC))
readProcTab' cfg = do
  xprintf "readProcTab'"
  branchTableConfig readAllProcsSimple readAllProcsPids readAllProcsUids cfg

readPtrArray0 :: HasCallStack => Ptr (Ptr a) -> IO [Ptr a]
readPtrArray0 ptr = do
  xprintf $ "readPtrArray0 " <> show ptr
  if ptr == nullPtr
    then pure []
    else peekArray0 nullPtr ptr >>= xreturn

{- | Read all the 'Proc's according to a 'TableConfig'.
Errors are ignored.
It is the caller's responsibility to free the internal 'Ptr's once they're done with them.
It's safer to use 'withAllProcsLenient'.
-}
openAllProcsLenient :: HasCallStack => TableConfig -> IO (Ptr (Ptr ProcC), [Ptr ProcC], [Proc])
openAllProcsLenient cfg = do
  (ptrArray, ptrs, procs) <- openAllProcs cfg
  pure (ptrArray, ptrs, rights procs)

{- | Read all the 'Proc's according to a 'TableConfig'.
It is the caller's responsibility to free the internal 'Ptr's once they're done with them.
It's safer to use 'withAllProcs'.
-}
openAllProcs :: HasCallStack => TableConfig -> IO (Ptr (Ptr ProcC), [Ptr ProcC], [Either ProcError Proc])
openAllProcs cfg = do
  ptrArray <- readProcTab' cfg
  ptrs <- readPtrArray0 ptrArray
  pure (ptrArray, ptrs, Internal.makeProc <$> ptrs)

{- | Read all the 'ProcInfo's according to a 'TableConfig'.
Errors are ignored.
-}
readAllProcInfos :: HasCallStack => TableConfig -> IO [ProcInfo]
readAllProcInfos cfg =
  withAllProcsLenient cfg $ traverse procInfo

readAllProcInfosE :: HasCallStack => TableConfig -> IO [Either ProcError ProcInfo]
readAllProcInfosE cfg =
  withAllProcs cfg $ traverse (traverse procInfo)

withAllProcs' :: (TableConfig -> IO (Ptr a1, [Ptr a2], [a3])) -> (a3 -> IO b) -> TableConfig -> ([a3] -> IO c) -> IO c
withAllProcs' open close' cfg f =
  bracket (open cfg) close $ \(_, _, procs) -> f procs
  where
    close (ptrPtr, _ptrs, procs) = do
      free ptrPtr
      -- traverse_ free ptrs
      traverse_ close' procs

{- | Bracketed access to all the 'Proc's read according to a 'TableConfig'.
Internal pointers will be freed after use.
-}
withAllProcs :: HasCallStack => TableConfig -> ([Either ProcError Proc] -> IO a) -> IO a
withAllProcs =
  withAllProcs' openAllProcs (traverse_ closeProc)

{- | Bracketed access to all the 'Proc's read according to a 'TableConfig'.
Any error reading one 'Proc' leads to an overall error.
Internal pointers will be freed after use.
-}
withAllProcsEither :: HasCallStack => TableConfig -> ([Proc] -> IO a) -> IO (Either ProcError a)
withAllProcsEither cfg f =
  withAllProcs' openAllProcs (traverse_ closeProc) cfg $ traverse f . sequence

{- | Bracketed access to all the 'Proc's read according to a 'TableConfig'.
Errors are ignored.
Internal pointers will be freed after use.
-}
withAllProcsLenient :: HasCallStack => TableConfig -> ([Proc] -> IO a) -> IO a
withAllProcsLenient =
  withAllProcs' openAllProcsLenient closeProc

{- | Bracketed access to a 'Proc' representing the current proces or task.
The internal pointer will be freed after use.
-}
withSelfProc :: HasCallStack => (Proc -> IO a) -> IO (Either ProcError a)
withSelfProc f = do
  fmap join . Internal.withProcPtr readSelfProc $
    traverse f . Internal.makeProc

{- | Try to open a 'Proc' representing the current proces or task.
It is the caller's responsibility to free the internal 'Ptr' once they're done with it.
It's safer to use 'withSelfProc'.
-}
openSelfProc :: HasCallStack => IO (Either ProcError Proc)
openSelfProc = Internal.makeProc <$> readSelfProc

{- | Free the internal 'Ptr' of a 'Proc', if it's not null.
The 'Proc' must not be used after 'closeProc' has been called.
-}
closeProc :: HasCallStack => Proc -> IO ()
closeProc (Internal.UnsafeProc ptr) = freeProc ptr
