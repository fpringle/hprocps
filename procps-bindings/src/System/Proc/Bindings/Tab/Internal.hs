{- HLINT ignore "Avoid lambda" -}
module System.Proc.Bindings.Tab.Internal where

import Control.Exception
import Foreign
import GHC.Stack
import System.Proc.Bindings.C
import System.Proc.Bindings.C.Utils
import System.Proc.Bindings.Error
import System.Proc.Bindings.Tab.C
import System.Proc.Bindings.Tab.Config

{- | The proctab (process table) is a persistent handle-like data structure holding the information
the library needs to read process information. While 'System.Proc.Bindings.Proc's are
typically ephemeral, the 'ProcTab' is more long lived.

Typically it will be created and populated by 'System.Proc.Bindings.Tab.openProcTab'
(or 'System.Proc.Bindings.Tab.withProcTab', which handles memory management); the 'TableConfig'
tells the underlying library what information we want to read, but no actual process information
is read yet.

We then query the 'ProcTab' for information about processes, using functions like
'System.Proc.Bindings.readNextProc' or 'System.Proc.Bindings.readNextProcInfo'
-}
data ProcTab
  = UnsafeProcTab
      -- PROCTAB* pointer, guaranteed to be non-null.
      (Ptr ProcTabC)
      -- A reusable proc_t pointer to save time allocating and freeing memory.
      -- will be used as the second argument to the @readproc@ C function.
      (Ptr Proc)

{- | Open a @'Ptr' 'ProcTabC'@ according to a 'TableConfig'. It is the caller's responsibility
to check that the 'Ptr' is not null, and to free it once they're done with it.

It's safer to use 'withProcTabPtr', which will automatically clean up all the pointers.

The @IO ()@ return value is a cleanup function to free any memory that was allocated for
@openproc@, e.g an array of @uid_t@s or @pid_t@s.
-}
openProcTabPtr :: HasCallStack => TableConfig -> IO (Ptr ProcTabC, IO ())
openProcTabPtr = openTableConfig openProcTabSimple openProcTabFromPids openProcTabFromUids

{- | Bracketed access to a @'Ptr' 'ProcTabC'@. The pointer will be freed after use.
Return a 'ProcError' if the pointer is null.
-}
withProcTabPtr :: HasCallStack => TableConfig -> (Ptr ProcTabC -> IO a) -> IO (Either ProcError a)
withProcTabPtr cfg f =
  branchTableConfig
    (\flags -> bracketProcTab (openProcTabSimple flags))
    (\flags pidPtr -> bracketProcTab (openProcTabFromPids flags pidPtr))
    (\flags uidPtr nuid -> bracketProcTab (openProcTabFromUids flags uidPtr nuid))
    cfg
  where
    bracketProcTab open = bracket open closeProcTabC (eitherPeek f)
