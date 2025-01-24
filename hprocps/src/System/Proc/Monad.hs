{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Proc.Monad
  ( RegionT
  , runRegionT
  , throwProcErrorT
  , ProcM (..)
  , runProcM
  , RegionM
  , runRegionM
  , throwProcErrorM
  , allocateM
  , allocateMEither
  )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Resource
import System.Proc.Bindings.Error
import System.Proc.Monad.Internal

newtype ProcM a = ProcM {unProcM :: ExceptT ProcError (ResourceT IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadResource)

runProcM :: ProcM a -> IO (Either ProcError a)
runProcM = runResourceT . runExceptT . unProcM

throwProcErrorM :: ProcError -> ProcM a
throwProcErrorM = ProcM . throwE
{-# INLINE throwProcErrorM #-}

runRegionT :: (forall s. RegionT s m a) -> m a
runRegionT (RegionT io) = io

type RegionM s a = RegionT s ProcM a

runRegionM :: (forall s. RegionM s a) -> ProcM a
runRegionM = runRegionT
{-# INLINE runRegionM #-}

throwProcErrorT :: ProcError -> RegionM s a
throwProcErrorT = RegionT . throwProcErrorM
{-# INLINE throwProcErrorT #-}

allocateM :: IO a -> (a -> IO ()) -> RegionM s a
allocateM open close = snd <$> allocate open close

allocateMEither :: IO (Either ProcError a) -> (a -> IO ()) -> RegionM s a
allocateMEither open close =
  liftIO open >>= \case
    Left err -> throwProcErrorT err
    Right a -> allocateM (pure a) close
