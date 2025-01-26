module System.Proc.Bindings.Internal where

import Control.Exception
import Foreign
import System.Proc.Bindings.C
import System.Proc.Bindings.Error
import System.Proc.Bindings.Info.Internal

{- | A handle to information about a process, parsed from its @\/proc\/#\/@ directory.

__NOTE__ this is not a "live" handle. The underlying C struct (@proc_t@) is created,
populated with information, and returned to the user. Reading process information with
'System.Proc.Bindings.procInfo' will give you the information about the process
/at the time the 'Proc' was created/, __not__ the information at the time you call
'System.Proc.Bindings.procInfo'. That is to say, successive calls to
'System.Proc.Bindings.procInfo' should give identical results.

Internally this is just a 'Ptr'. The library is designed so that it should be impossible
to get hold of a 'Proc' that contains a 'nullPtr': functions like
'System.Proc.Bindings.readNextProc', 'System.Proc.Bindings.openAllProcs' all check to
make sure this doesn't happen.

Nevertheless, this doesn't guarantee total type safety: Once the internal pointer has been
freed (either by 'System.Proc.Bindings.closeProc' or by the procps C library itself),
there's nothing stopping you from trying to use it again via 'System.Proc.Bindings.procInfo'
etc, which will cause undefined behaviour.
Don't do that.

For better type-safety, use the @hprocps@ library, which forbids this kind of bug using
monadic regions.
-}
newtype Proc = UnsafeProc (Ptr ProcC)
  deriving (Eq)

makeProc :: Ptr ProcC -> Either ProcError Proc
makeProc procPtr
  | procPtr == nullPtr = Left NullPtrError
  | otherwise = Right $ UnsafeProc procPtr

fromProcC :: Ptr ProcC -> IO ProcInfo
fromProcC ptr =
  ProcInfo <$> readProcCInfo ptr

withProcPtr :: IO (Ptr ProcC) -> (Ptr ProcC -> IO a) -> IO (Either ProcError a)
withProcPtr open f =
  bracket open freeProc $ \ptr ->
    if ptr == nullPtr
      then pure $ Left NullPtrError
      else Right <$> f ptr
