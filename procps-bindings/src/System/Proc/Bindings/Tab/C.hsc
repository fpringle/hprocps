{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}

#include "proctab-handle.h"

module System.Proc.Bindings.Tab.C
  ( ProcTabC
  , openProcTabSimple
  , openProcTabFromPids
  , openProcTabFromUids
  , closeProcTabC
  )
where

import Data.Kind
import Foreign
import Foreign.C.Types
import System.Posix.Types

data ProcTabC :: Type

foreign import capi unsafe "openproctab_simple" openProcTabSimple :: CInt -> IO (Ptr ProcTabC)

foreign import capi unsafe "openproctab_pids" openProcTabFromPids :: CInt -> Ptr CPid -> IO (Ptr ProcTabC)

foreign import capi unsafe "openproctab_uids" openProcTabFromUids :: CInt -> Ptr CUid -> CInt -> IO (Ptr ProcTabC)

foreign import capi unsafe "closeproctab" closeProcTabC :: Ptr ProcTabC -> IO ()
