module System.Proc.Bindings.Tab
  ( ProcTab
  , withProcTab
  , openProcTab
  , closeProcTab
  , ProcTabInfo (..)
  , getProcTabInfo
  , readProcTabInfo
  )
where

import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Ptr
import System.Proc.Bindings.Error
import System.Proc.Bindings.Tab.C
import System.Proc.Bindings.Tab.Config
import System.Proc.Bindings.Tab.Internal

-- | TODO
getProcTabInfo :: ProcTab -> IO ProcTabInfo
getProcTabInfo (UnsafeProcTab ptr _) = fromProcTabC ptr

-- | Bracketed access to a 'ProcTab'. The internal pointers will be freed after use.
withProcTab :: TableConfig -> (ProcTab -> IO a) -> IO (Either ProcError a)
withProcTab cfg f =
  withProcTabPtr cfg $ \procTabPtr ->
    alloca $ \procPtr ->
      f $ UnsafeProcTab procTabPtr procPtr

{- | Open a 'ProcTab' according to a 'TableConfig'. It is the caller's responsibility
to free the internal 'Ptr's once they're done with it.
-}
openProcTab :: TableConfig -> IO (Either ProcError ProcTab)
openProcTab cfg = do
  procTabPtr <- openProcTabPtr cfg
  if procTabPtr == nullPtr
    then pure $ Left NullPtrError
    else Right . UnsafeProcTab procTabPtr <$> calloc

-- | Free the internal 'Ptr's of a 'ProcTab', if they're not null.
closeProcTab :: ProcTab -> IO ()
closeProcTab (UnsafeProcTab procTabPtr procPtr) = do
  unless (procTabPtr == nullPtr) $ closeProcTabC procTabPtr
  unless (procPtr == nullPtr) $ free procPtr

-- | TODO
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
