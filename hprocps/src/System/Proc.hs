module System.Proc
  ( -- * Process handles
    Proc
  , readSelfProc
  , readAllProcs
  , readAllProcsLenient
  , readAllProcsThrow

    -- * Process table
  , ProcTab
  , readNextProc
  , readNextProcMaybe
  , readAllProcInfos

    -- * Regioned monad for resource safety
  , module System.Proc.Monad

    -- * Concrete process information
  , procInfo
  , readNextProcInfo
  , module System.Proc.Bindings.Info

    -- * Re-exports
  , module System.Proc.Bindings.Tab.Config
  , module System.Proc.Bindings.Error
  )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Foldable
import qualified System.Proc.Bindings as B
import System.Proc.Bindings.Error
import System.Proc.Bindings.Info
import System.Proc.Bindings.Tab.Config
import System.Proc.Monad hiding (RegionT)
import System.Proc.Monad.Internal (RegionT (..))
import System.Proc.Tab.Internal

newtype Proc s = UnsafeProc {unProc :: B.Proc}

procInfo :: MonadIO m => Proc s -> m B.ProcInfo
procInfo = liftIO . B.procInfo . unProc

readNextProc :: ProcTab s -> RegionM s (Proc s)
readNextProc =
  fmap UnsafeProc . RegionT . ProcM . ExceptT . lift . B.readNextProc . unProcTab

readNextProcMaybe :: ProcTab s -> RegionM s (Maybe (Proc s))
readNextProcMaybe =
  fmap (fmap UnsafeProc . rightToMaybe)
    . RegionT
    . ProcM
    . lift
    . lift
    . B.readNextProc
    . unProcTab

rightToMaybe :: Either e a -> Maybe a
rightToMaybe = either (const Nothing) Just
{-# INLINE rightToMaybe #-}

readSelfProc :: RegionM s (Proc s)
readSelfProc = UnsafeProc <$> allocateMEither B.openSelfProc B.closeProc

readAllProcsLenient :: TableConfig -> RegionM s [Proc s]
readAllProcsLenient cfg = fmap UnsafeProc <$> allocateM (B.openAllProcsLenient cfg) (traverse_ B.closeProc)

readAllProcs :: TableConfig -> RegionM s [Either ProcError (Proc s)]
readAllProcs cfg = fmap (fmap UnsafeProc) <$> allocateM (B.openAllProcs cfg) (traverse_ (traverse_ B.closeProc))

readAllProcsThrow :: TableConfig -> RegionM s [Proc s]
readAllProcsThrow cfg = readAllProcs cfg >>= either throwProcErrorT pure . sequence

readNextProcInfo :: MonadIO m => ProcTab s -> m (Either ProcError B.ProcInfo)
readNextProcInfo = liftIO . B.readNextProcInfo . unProcTab

readAllProcInfos :: MonadIO m => TableConfig -> m [B.ProcInfo]
readAllProcInfos = liftIO . B.readAllProcInfos
