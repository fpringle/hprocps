module System.Proc.Bindings.Tab.Internal where

import Control.Exception
import Foreign
import System.Proc.Bindings.C
import System.Proc.Bindings.Error
import System.Proc.Bindings.Tab.C
import System.Proc.Bindings.Tab.Config

data ProcTab
  = UnsafeProcTab
      -- Guaranteed to be non-null.
      (Ptr ProcTabC)
      (Ptr ProcC)

withProcTabPtr' :: IO (Ptr ProcTabC) -> (Ptr ProcTabC -> IO a) -> IO (Either ProcError a)
withProcTabPtr' open f =
  bracket open close action
  where
    close ptr
      | ptr == nullPtr = pure ()
      | otherwise = closeProcTabC ptr
    action ptr
      | ptr == nullPtr = pure $ Left NullPtrError
      | otherwise = Right <$> f ptr

{- | Bracketed access to a @'Ptr' 'ProcTabC'@. The pointer will be freed after use.
Return a 'ProcError' if the pointer is null.
-}
withProcTabPtr :: TableConfig -> (Ptr ProcTabC -> IO a) -> IO (Either ProcError a)
withProcTabPtr cfg = withProcTabPtr' (openProcTabPtr cfg)

{- | Open a @'Ptr' 'ProcTabC'@ according to a 'TableConfig'. It is the caller's responsibility
to check that the 'Ptr' is not null, and to free it once they're done with it.
-}
openProcTabPtr :: TableConfig -> IO (Ptr ProcTabC)
openProcTabPtr = branchTableConfig openProcTabSimple openProcTabFromPids openProcTabFromUids
