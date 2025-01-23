module System.Proc.Tab.Internal
  ( ProcTab (..)
  , mkProcTab
  , ProcTabC
  , withProcTabPtr
  , withProcTabPtrSimple
  , withProcTabPtrPids
  , withProcTabPtrUids
  , branchTableConfig
  )
where

import Control.Exception
import Foreign
import Foreign.C.Types
import System.Posix.Types
import System.Proc.C
import System.Proc.Error
import System.Proc.Flags
import System.Proc.Tab.C

data ProcTab
  = UnsafeProcTab
      -- Guaranteed to be non-null.
      (Ptr ProcTabC)
      (Ptr ProcC)

mkProcTab :: Ptr ProcTabC -> Ptr ProcC -> Either ProcError ProcTab
mkProcTab ptr procPtr
  | ptr == nullPtr = Left NullPtrError
  | otherwise = Right $ UnsafeProcTab ptr procPtr

withProcTabPtr' :: IO (Ptr ProcTabC) -> (Ptr ProcTabC -> IO a) -> IO (Either ProcError a)
withProcTabPtr' open f =
  bracket open close action
  where
    close ptr
      | ptr == nullPtr = pure ()
      | otherwise = closeProcTab ptr
    action ptr
      | ptr == nullPtr = pure $ Left NullPtrError
      | otherwise = Right <$> f ptr

withProcTabPtrSimple :: CInt -> (Ptr ProcTabC -> IO a) -> IO (Either ProcError a)
withProcTabPtrSimple flags = withProcTabPtr' $ openProcTabSimple flags

withProcTabPtrPids :: CInt -> Ptr CPid -> (Ptr ProcTabC -> IO a) -> IO (Either ProcError a)
withProcTabPtrPids flags pids = withProcTabPtr' $ openProcTabPids flags pids

withProcTabPtrUids :: CInt -> Ptr CUid -> CInt -> (Ptr ProcTabC -> IO a) -> IO (Either ProcError a)
withProcTabPtrUids flags uids nuid = withProcTabPtr' $ openProcTabUids flags uids nuid

branchTableConfig ::
  (CInt -> IO a) ->
  (CInt -> Ptr CPid -> IO a) ->
  (CInt -> Ptr CUid -> CInt -> IO a) ->
  (TableConfig -> IO a)
branchTableConfig simple onPids onUids (TableConfig flags tabFilter) = case tabFilter of
  NoFilter -> simple flagsInt
  ByPids pids ->
    withArray0 0 pids $ \pidPtr -> onPids flagsInt pidPtr
  ByUids uids ->
    withArrayLen uids $ \nuid uidPtr -> onUids flagsInt uidPtr (fromIntegral nuid)
  where
    flagsInt = flagsAsInt flags

withProcTabPtr :: TableConfig -> (Ptr ProcTabC -> IO a) -> IO (Either ProcError a)
withProcTabPtr cfg f =
  branchTableConfig
    (`withProcTabPtrSimple` f)
    (\flgs pidPtr -> withProcTabPtrPids flgs pidPtr f)
    (\flgs uidPtr nuid -> withProcTabPtrUids flgs uidPtr nuid f)
    cfg
