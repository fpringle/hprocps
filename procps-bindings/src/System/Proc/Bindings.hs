module System.Proc.Bindings
  ( -- * Process handles
    Proc
  , closeProc
  , openSelfProc
  , withSelfProc
  , openAllProcs
  , openAllProcsLenient
  , withAllProcs

    -- ** Concrete process information
  , ProcInfo
  , procInfo
  , readNextProcInfo

    -- ** Process table
  , ProcTab
  , readNextProc
  , readProcInfos

    -- * Field accessors
  , SignalMask
  , Address
  , module Export
  )
where

import Control.Exception
import Control.Monad
import Data.Either
import Data.Foldable
import Data.Functor
import Foreign
import System.Proc.Bindings.C
import System.Proc.Bindings.Error
import System.Proc.Bindings.Internal (Proc, ProcInfo (..))
import System.Proc.Bindings.Internal as Export hiding
  ( Proc (..)
  , ProcInfo (..)
  , fromProcC
  , makeProc
  , withProcPtr
  )
import qualified System.Proc.Bindings.Internal as Internal
import System.Proc.Bindings.Tab.Config
import System.Proc.Bindings.Tab.Internal

{- | Read concrete 'ProcInfo' from a 'Proc' handle.
'ProcInfo' is just a normal Haskell datatype without any pointers going on, so you can
do what you want with it.
-}
procInfo :: Proc -> IO ProcInfo
procInfo (Internal.UnsafeProc ptr) = Internal.fromProcC ptr

-- | Read the next 'Proc' from the 'ProcTab'.
readNextProc :: ProcTab -> IO (Either ProcError Proc)
readNextProc (UnsafeProcTab procTabPtr procPtr') = do
  procPtr <- readNextProcC procTabPtr procPtr'
  pure $ Internal.makeProc procPtr

-- readNextProcInfo = readNextProc >=> traverse procInfo
readNextProcInfo :: ProcTab -> IO (Either ProcError ProcInfo)
readNextProcInfo (UnsafeProcTab procTabPtr procPtr') = do
  procPtr <- readNextProcC procTabPtr procPtr'
  if procPtr == nullPtr
    then pure $ Left NullPtrError
    else Right <$> Internal.fromProcC procPtr

readProcTab' :: TableConfig -> IO (Ptr (Ptr ProcC))
readProcTab' =
  branchTableConfig readAllProcsSimple readAllProcsPids readAllProcsUids

readPtrArray0 :: Ptr (Ptr a) -> IO [Ptr a]
readPtrArray0 ptr
  | ptr == nullPtr = pure []
  | otherwise = peekArray0 nullPtr ptr

{- | Read all the 'Proc's according to a 'TableConfig'.
Errors are ignored.
It is the caller's responsibility to free the internal 'Ptr's once they're done with them.
-}
openAllProcsLenient :: TableConfig -> IO [Proc]
openAllProcsLenient cfg = rights <$> openAllProcs cfg

{- | Read all the 'Proc's according to a 'TableConfig'.
It is the caller's responsibility to free the internal 'Ptr's once they're done with them.
-}
openAllProcs :: TableConfig -> IO [Either ProcError Proc]
openAllProcs cfg =
  readProcTab' cfg
    >>= readPtrArray0
    <&> fmap Internal.makeProc

{- | Read all the 'ProcInfo's according to a 'TableConfig'.
Errors are ignored.
-}
readProcInfos :: TableConfig -> IO [ProcInfo]
readProcInfos = readProcTab' >=> readPtrArray0 >=> traverse (fmap ProcInfo . readProcCInfo)

{- | Bracketed access to all the 'Proc's read according to a 'TableConfig'.
Errors are ignored.
Internal pointers will be freed after use.
-}
withAllProcs :: TableConfig -> ([Proc] -> IO a) -> IO a
withAllProcs cfg =
  bracket (openAllProcsLenient cfg) (traverse_ closeProc)

{- | Bracketed access to a 'Proc' representing the current proces or task.
The internal pointer will be freed after use.
-}
withSelfProc :: (Proc -> IO a) -> IO (Either ProcError a)
withSelfProc f = do
  fmap join . Internal.withProcPtr readSelfProc $
    traverse f . Internal.makeProc

{- | Try to open a 'Proc' representing the current proces or task.
It is the caller's responsibility to free the internal 'Ptr' once they're done with it.
-}
openSelfProc :: IO (Either ProcError Proc)
openSelfProc = Internal.makeProc <$> readSelfProc

{- | Free the internal 'Ptr' of a 'Proc', if it's not null.
The 'Proc' must not be used after 'closeProc' has been called.
-}
closeProc :: Proc -> IO ()
closeProc (Internal.UnsafeProc ptr) = unless (ptr == nullPtr) $ freeProc ptr
