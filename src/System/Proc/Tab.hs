module System.Proc.Tab
  ( ProcTab (..)
  , withProcTabSimple
  , withProcTabUids
  , withProcTabPids
  )
where

import Control.Monad
import Data.List
import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.Posix.Types
import System.Proc.Error
import System.Proc.Tab.C

data Dir = TODO
  deriving (Show)

data ProcTab = ProcTab
  { procFs :: Dir
  , taskDir :: Dir
  , taskDirUser :: CPid
  , pids :: [CPid]
  , uids :: [CUid]
  , flags :: CFlags
  , path :: Maybe FilePath
  }
  deriving (Show)

readStringMaybe :: CString -> IO (Maybe String)
readStringMaybe cStr
  | cStr == nullPtr = pure Nothing
  | otherwise = Just <$> peekCString cStr

readStringLenMaybe :: Int -> CString -> IO (Maybe String)
readStringLenMaybe len cStr
  | cStr == nullPtr = pure Nothing
  | otherwise = Just <$> peekCStringLen (cStr, len)

fromProcTabC :: ProcTabC -> IO ProcTab
fromProcTabC ProcTabC {..} = do
  pids <- readArrayNulTerminated ptcPids
  uids <- peekArray (fromEnum ptcNUid) ptcUids
  path <- readStringLenMaybe (fromEnum ptcPathLen) ptcPath
  pure $
    ProcTab
      { procFs = TODO
      , taskDir = TODO
      , taskDirUser = ptcTaskDirUser
      , flags = fromIntegral ptcFlags
      , ..
      }

-- TODO
type CFlags = CInt

type Flags = CInt

withArrayNull :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withArrayNull vals f = do
  allocaArray0 len $ \ptr -> do
    pokeArray ptr vals
    f ptr
  where
    len = length vals

withProcTabSimple :: Flags -> (ProcTab -> IO a) -> IO (Either ProcError a)
withProcTabSimple flgs f = withProcTabCSimple flgs $ peek >=> fromProcTabC >=> f

readArrayNulTerminated :: (Storable a, Num a, Eq a) => Ptr a -> IO [a]
readArrayNulTerminated ptr
  | ptr == nullPtr = pure []
  | otherwise = go 0
  where
    go n = do
      val <- peekElemOff ptr n
      if val == 0
        then pure []
        else (val :) <$> go (n + 1)

withProcTabPids :: Flags -> [CPid] -> (ProcTab -> IO a) -> IO (Either ProcError a)
withProcTabPids flgs pids f =
  withArrayNull pids $ \pidArr ->
    withProcTabCPids flgs pidArr $ peek >=> fromProcTabC >=> f

withProcTabUids :: Flags -> [CUid] -> (ProcTab -> IO a) -> IO (Either ProcError a)
withProcTabUids flgs uids f =
  withArray uids $ \uidArr ->
    withProcTabCUids flgs uidArr nuid $ peek >=> fromProcTabC >=> f
  where
    nuid = genericLength uids
