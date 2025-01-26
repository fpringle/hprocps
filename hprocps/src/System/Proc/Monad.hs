{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Proc.Monad
  ( -- * Monadic region transformer
    RegionT
  , runRegionT
  , throwProcErrorT

    -- * Resource monad
  , ProcM (..)
  , runProcM
  , throwProcErrorM
  , allocateM
  , allocateMEither
  , RegionM
  , runRegionM
  )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Resource
import System.Proc.Bindings.Error
import System.Proc.Monad.Internal

{- | A simple newtype - most of the functions in this library operate in this monad.

We use the 'MonadResource' instance to handle automatic freeing of internal pointers;
and we combine with 'RegionT' (see t'RegionM') to prevent leaking freed resources.
-}
newtype ProcM a = ProcM {unProcM :: ExceptT ProcError (ResourceT IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadResource)

-- | Run a 'ProcM', nothing exciting going on here.
runProcM :: ProcM a -> IO (Either ProcError a)
runProcM = runResourceT . runExceptT . unProcM

{- | Throw a 'ProcError' in the 'ProcM' monad.

We could just derive a [MonadError](https://hackage.haskell.org/package/mtl/docs/Control-Monad-Error-Class.html#t:MonadError)
instance and use @throwError@, but that would incur an unnecessary dependency on @mtl@.
-}
throwProcErrorM :: ProcError -> ProcM a
throwProcErrorM = ProcM . throwE
{-# INLINE throwProcErrorM #-}

{- | Run a 'RegionT'. This is analogous to 'Control.Monad.ST.runST'. The rank-2 signature
(the @forall s@) is where the magic happens. This ensures that our return value @a@ doesn't
depend on @s@. For example trying to return a 'System.Proc.Proc' or 'System.Proc.Tab.ProcTab'
will result in a compile-time error. This prevents us from leaking resources after they've
been released.
-}
runRegionT :: (forall s. RegionT s m a) -> m a
runRegionT (RegionT io) = io

-- | Type alias for convenience.
type RegionM s a = RegionT s ProcM a

-- | Alias for 'runRegionT'.
runRegionM :: (forall s. RegionM s a) -> ProcM a
runRegionM = runRegionT
{-# INLINE runRegionM #-}

-- | Throw a 'ProcError' in the t'RegionM' monad. See 'throwProcErrorM'.
throwProcErrorT :: ProcError -> RegionM s a
throwProcErrorT = RegionT . throwProcErrorM
{-# INLINE throwProcErrorT #-}

-- | Allocate a resource in the t'RegionM' monad.
allocateM :: MonadResource m => IO a -> (a -> IO ()) -> m a
allocateM open close = snd <$> allocate open close

{- | Allocate a resource in the t'RegionM' monad, with the option of failure during allocation.

Allocation failures are re-thrown using 'throwProcErrorT'.
-}
allocateMEither :: IO (Either ProcError a) -> (a -> IO ()) -> RegionM s a
allocateMEither open close =
  liftIO open >>= \case
    Left err -> throwProcErrorT err
    Right a -> allocateM (pure a) close
