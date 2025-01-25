module System.Proc.Bindings.Tab.Internal where

import Control.Exception
import Foreign
import System.Proc.Bindings.C
import System.Proc.Bindings.C.Utils
import System.Proc.Bindings.Error
import System.Proc.Bindings.Tab.C
import System.Proc.Bindings.Tab.Config

{- | The proctab (process table) is a persistent data structure holding the information
the library needs to read process information. While 'System.Proc.Bindings.Proc's are
typically ephemeral, the 'ProcTab' is more long lived. Typically it will be created and
populated by 'System.Proc.Bindings.Tab.openProcTab' (or 'System.Proc.Bindings.Tab.withProcTab', which handles memory management); the 'TableConfig'
tells the underlying library what information we want to read, but no actual process information
is read yet. We then query the 'ProcTab' for information about processes, using functions like
'System.Proc.Bindings.readNextProc' or 'System.Proc.Bindings.readNextProcInfo'
-}
data ProcTab
  = UnsafeProcTab
      -- PROCTAB* pointer, guaranteed to be non-null.
      (Ptr ProcTabC)
      -- A reusable proc_t pointer to save time allocating and freeing memory.
      -- will be used as the second argument to the @readproc@ C function.
      (Ptr ProcC)

withProcTabPtr' :: IO (Ptr ProcTabC) -> (Ptr ProcTabC -> IO a) -> IO (Either ProcError a)
withProcTabPtr' open = bracket open closeProcTabC . eitherPeek

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
