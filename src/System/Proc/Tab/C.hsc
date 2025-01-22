{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}

#include <proc/readproc.h>
#include "readproc-wrapper.h"

module System.Proc.Tab.C
  ( ProcTabC (..)
  , DirC (..)

    -- * Bracketed functions
  , withProcTabCSimple
  , withProcTabCPids
  , withProcTabCUids

    -- * FFI Functions
  , openProcWrapperSimple
  , closeProc
  )
where

import Control.Exception
import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.Posix.Types
import System.Proc.Error

data DirC = DirC

{-
typedef struct PROCTAB {
    DIR*	procfs;
    DIR*	taskdir;  // for threads
    pid_t	taskdir_user;  // for threads
    int         did_fake; // used when taskdir is missing

    int(*finder)(struct PROCTAB *__restrict const, proc_t *__restrict const);
    proc_t*(*reader)(struct PROCTAB *__restrict const, proc_t *__restrict const);
    int(*taskfinder)(struct PROCTAB *__restrict const, const proc_t *__restrict const, proc_t *__restrict const, char *__restrict const);
    proc_t*(*taskreader)(struct PROCTAB *__restrict const, const proc_t *__restrict const, proc_t *__restrict const, char *__restrict const);

    pid_t*	pids;	// pids of the procs
    uid_t*	uids;	// uids of procs
    int		nuid;	// cannot really sentinel-terminate unsigned short[]
    int         i;  // generic
    unsigned	flags;
    unsigned    u;  // generic
    void *      vp; // generic
    char        path[PROCPATHLEN];  // must hold /proc/2000222000/task/2000222000/cmdline
    unsigned pathlen;        // length of string in the above (w/o '\0')
} PROCTAB;
-}
data ProcTabC = ProcTabC
  { ptcProcFs :: Ptr DirC
  , ptcTaskDir :: Ptr DirC
  , ptcTaskDirUser :: CPid
  , ptcDidFake :: CInt
  , ptcFinder :: Ptr ()
  , ptcReader :: Ptr ()
  , ptcTaskFinder :: Ptr ()
  , ptcTaskReader :: Ptr ()
  , ptcPids :: Ptr CPid
  , ptcUids :: Ptr CUid
  , ptcNUid :: CInt
  , ptcI :: CInt
  , ptcFlags :: CUInt
  , ptcU :: CUInt
  , ptcVP :: Ptr ()
  , ptcPath :: CString
  , ptcPathLen :: CUInt
  }

instance Storable ProcTabC where
  peek ptr =
    ProcTabC
      <$> (#peek PROCTAB, procfs) ptr
      <*> (#peek PROCTAB, taskdir) ptr
      <*> (#peek PROCTAB, taskdir_user) ptr
      <*> (#peek PROCTAB, did_fake) ptr
      <*> (#peek PROCTAB, finder) ptr
      <*> (#peek PROCTAB, reader) ptr
      <*> (#peek PROCTAB, taskfinder) ptr
      <*> (#peek PROCTAB, taskreader) ptr
      <*> (#peek PROCTAB, pids) ptr
      <*> (#peek PROCTAB, uids) ptr
      <*> (#peek PROCTAB, nuid) ptr
      <*> (#peek PROCTAB, i) ptr
      <*> (#peek PROCTAB, flags) ptr
      <*> (#peek PROCTAB, u) ptr
      <*> (#peek PROCTAB, vp) ptr
      <*> (#peek PROCTAB, path) ptr
      <*> (#peek PROCTAB, pathlen) ptr
  alignment _ = #alignment PROCTAB
  sizeOf _ = #size PROCTAB
  poke ptr (ProcTabC {..}) = do
    (#poke PROCTAB, procfs) ptr ptcProcFs
    (#poke PROCTAB, taskdir) ptr ptcTaskDir
    (#poke PROCTAB, taskdir_user) ptr ptcTaskDirUser
    (#poke PROCTAB, did_fake) ptr ptcDidFake
    (#poke PROCTAB, finder) ptr ptcFinder
    (#poke PROCTAB, reader) ptr ptcReader
    (#poke PROCTAB, taskfinder) ptr ptcTaskFinder
    (#poke PROCTAB, taskreader) ptr ptcTaskReader
    (#poke PROCTAB, pids) ptr ptcPids
    (#poke PROCTAB, uids) ptr ptcUids
    (#poke PROCTAB, nuid) ptr ptcNUid
    (#poke PROCTAB, i) ptr ptcI
    (#poke PROCTAB, flags) ptr ptcFlags
    (#poke PROCTAB, u) ptr ptcU
    (#poke PROCTAB, vp) ptr ptcVP
    (#poke PROCTAB, path) ptr ptcPath
    (#poke PROCTAB, pathlen) ptr ptcPathLen

foreign import capi unsafe "openproc_wrapper_simple" openProcWrapperSimple :: CInt -> IO (Ptr ProcTabC)

foreign import capi unsafe "openproc_wrapper_pids" openProcWrapperPids :: CInt -> Ptr CPid -> IO (Ptr ProcTabC)

foreign import capi unsafe "openproc_wrapper_uids" openProcWrapperUids :: CInt -> Ptr CUid -> CInt -> IO (Ptr ProcTabC)

foreign import capi unsafe "closeproc_wrapper" closeProc :: Ptr ProcTabC -> IO ()

withProcTabCPtr :: IO (Ptr ProcTabC) -> (Ptr ProcTabC -> IO a) -> IO (Either ProcError a)
withProcTabCPtr open f = bracket open closeProc $ \ptr ->
  if ptr == nullPtr
    then pure $ Left NullPtrError
    else Right <$> f ptr

withProcTabCSimple :: CInt -> (Ptr ProcTabC -> IO a) -> IO (Either ProcError a)
withProcTabCSimple flags = withProcTabCPtr $ openProcWrapperSimple flags

withProcTabCPids :: CInt -> Ptr CPid -> (Ptr ProcTabC -> IO a) -> IO (Either ProcError a)
withProcTabCPids flags pids = withProcTabCPtr $ openProcWrapperPids flags pids

withProcTabCUids :: CInt -> Ptr CUid -> CInt -> (Ptr ProcTabC -> IO a) -> IO (Either ProcError a)
withProcTabCUids flags uids nuid = withProcTabCPtr $ openProcWrapperUids flags uids nuid
