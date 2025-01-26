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
import System.Proc.Monad
import System.Proc.Monad.Internal (RegionT (..))
import System.Proc.Tab.Internal

{- | This is the equivalent of 'B.Proc' from @procps-bindings@, see the documentation
there for more details.

The only difference is the @s@ parameter. This plays the same role as it does in
'Control.Monad.ST.ST' - see the documentation of 'RegionT' for more.
-}
newtype Proc s = UnsafeProc {unProc :: B.Proc}

-- | Read concrete 'ProcInfo' from a 'Proc' handle.
procInfo :: MonadIO m => Proc s -> m B.ProcInfo
procInfo = liftIO . B.procInfo . unProc

-- | Read the next 'Proc' from the 'ProcTab'.
readNextProc :: ProcTab s -> RegionM s (Proc s)
readNextProc =
  fmap UnsafeProc . RegionT . ProcM . ExceptT . lift . B.readNextProc . unProcTab

-- | Read the next 'Proc' from the 'ProcTab', if there is one. Otherwise return 'Nothing'.
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

-- | Open a 'Proc' representing the current proces or task.
readSelfProc :: RegionM s (Proc s)
readSelfProc = UnsafeProc <$> allocateMEither B.openSelfProc B.closeProc

{- | Read all the 'Proc's according to a 'TableConfig'.
Errors are ignored.
-}
readAllProcsLenient :: TableConfig -> RegionM s [Proc s]
readAllProcsLenient cfg = fmap UnsafeProc <$> allocateM (B.openAllProcsLenient cfg) (traverse_ B.closeProc)

-- | Read all the 'Proc's according to a 'TableConfig'.
readAllProcs :: TableConfig -> RegionM s [Either ProcError (Proc s)]
readAllProcs cfg = fmap (fmap UnsafeProc) <$> allocateM (B.openAllProcs cfg) (traverse_ (traverse_ B.closeProc))

{- | Read all the 'Proc's according to a 'TableConfig'.
Any errors are thrown using 'throwProcErrorT'.
-}
readAllProcsThrow :: TableConfig -> RegionM s [Proc s]
readAllProcsThrow cfg = readAllProcs cfg >>= either throwProcErrorT pure . sequence

-- | Read the next 'ProcInfo' in the 'ProcTab'.
readNextProcInfo :: MonadIO m => ProcTab s -> m (Either ProcError B.ProcInfo)
readNextProcInfo = liftIO . B.readNextProcInfo . unProcTab

{- | Read all the 'ProcInfo's according to a 'TableConfig'.
Errors are ignored.
-}
readAllProcInfos :: MonadIO m => TableConfig -> m [B.ProcInfo]
readAllProcInfos = liftIO . B.readAllProcInfos
