{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}

#include "proctab-handle.h"

module System.Proc.Bindings.Tab.C
  ( ProcTabC
  , openProcTabSimple
  , openProcTabPids
  , openProcTabUids
  , closeProcTabC
  )
where

import Data.Kind
import Foreign
import Foreign.C.Types
import System.Posix.Types

data ProcTabC :: Type

foreign import capi unsafe "openproctab_simple" openProcTabSimple :: CInt -> IO (Ptr ProcTabC)

foreign import capi unsafe "openproctab_pids" openProcTabPids :: CInt -> Ptr CPid -> IO (Ptr ProcTabC)

foreign import capi unsafe "openproctab_uids" openProcTabUids :: CInt -> Ptr CUid -> CInt -> IO (Ptr ProcTabC)

foreign import capi unsafe "closeproctab" closeProcTabC :: Ptr ProcTabC -> IO ()
