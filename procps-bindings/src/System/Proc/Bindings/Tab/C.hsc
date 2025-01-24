{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}

#include "proctab-handle.h"

module System.Proc.Bindings.Tab.C
  ( ProcTabC
  , openProcTabSimple
  , openProcTabFromPids
  , openProcTabFromUids
  , closeProcTabC

    -- * Read proctab fields
  , readProcTabPath
  , readProcTabFlags
  , readProcTabPids
  , readProcTabUids
  , readProcTabDidFake

    -- * Utils
  , readNullTermArray
  , readNullTermArrayMaybe
  )
where

import Data.Kind
import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import System.Posix.Types

data ProcTabC :: Type

foreign import capi unsafe "openproctab_simple" openProcTabSimple :: CInt -> IO (Ptr ProcTabC)

foreign import capi unsafe "openproctab_pids" openProcTabFromPids :: CInt -> Ptr CPid -> IO (Ptr ProcTabC)

foreign import capi unsafe "openproctab_uids" openProcTabFromUids :: CInt -> Ptr CUid -> CInt -> IO (Ptr ProcTabC)

foreign import capi unsafe "closeproctab" closeProcTabC :: Ptr ProcTabC -> IO ()

readNullTermArray :: (Storable a, Num a, Eq a) => Ptr a -> IO [a]
readNullTermArray ptr = fromMaybe [] <$> readNullTermArrayMaybe ptr

readNullTermArrayMaybe :: (Storable a, Num a, Eq a) => Ptr a -> IO (Maybe [a])
readNullTermArrayMaybe ptr
  | ptr == nullPtr = pure Nothing
  | otherwise = Just <$> peekArray0 0 ptr

readProcTabPath :: Ptr ProcTabC -> IO FilePath
readProcTabPath ptr = do
  pathlen :: CUInt <- peek $ #{ptr PROCTAB, pathlen} ptr
  let pathPtr :: Ptr CChar = #{ptr PROCTAB, path} ptr
  peekCStringLen (pathPtr, fromIntegral pathlen)

readProcTabFlags :: Ptr ProcTabC -> IO CUInt
readProcTabFlags = peek . #{ptr PROCTAB, flags}

readProcTabPids :: Ptr ProcTabC -> IO [CPid]
readProcTabPids = readNullTermArray . #{ptr PROCTAB, pids}

readProcTabUids :: Ptr ProcTabC -> IO [CUid]
readProcTabUids ptr = do
  nuid :: CInt <- peek $ #{ptr PROCTAB, nuid} ptr
  uidPtr :: Ptr CUid <- peek $ #{ptr PROCTAB, uids} ptr
  peekArray (fromIntegral nuid) uidPtr

readProcTabDidFake :: Ptr ProcTabC -> IO Bool
readProcTabDidFake ptr = do
  didFake :: CInt <- peek $ #{ptr PROCTAB, did_fake} ptr
  pure $ toBool didFake
