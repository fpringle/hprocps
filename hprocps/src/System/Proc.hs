module System.Proc
  ( -- * Process handles
    B.Proc
  , readSelfProc
  , readAllProcs
  , readAllProcsLenient
  , readAllProcsThrow

    -- * Process table
  , ProcTab
  , readNextProc
  , readNextProcMaybe

    -- * Regioned monad for resource safety
  , module System.Proc.Monad

    -- * Re-exports
  , module System.Proc.Bindings.Tab.Config
  , module System.Proc.Bindings.Error
  )
where

import Control.Monad
import Control.Monad.IO.Class
import qualified System.Proc.Bindings as B
import System.Proc.Bindings.Error
import System.Proc.Bindings.Tab.Config
import System.Proc.Monad
import System.Proc.Tab.Internal

-- | Read the next 'B.Proc' from the 'ProcTab'.
readNextProc :: ProcTab s -> RegionM s B.Proc
readNextProc = readNextProcEither >=> either throwProcErrorT pure

-- | Read the next 'B.Proc' from the 'ProcTab', if there is one. Otherwise return 'Nothing'.
readNextProcMaybe :: ProcTab s -> RegionM s (Maybe B.Proc)
readNextProcMaybe pt = rightToMaybe <$> readNextProcEither pt

rightToMaybe :: Either e a -> Maybe a
rightToMaybe = either (const Nothing) Just
{-# INLINE rightToMaybe #-}

-- DRY
readNextProcEither :: ProcTab s -> RegionM s (Either ProcError B.Proc)
readNextProcEither (UnsafeProcTab _ pt) = liftIO (B.readNextProc pt)

-- | Open a 'B.Proc' representing the current proces or task.
readSelfProc :: ProcM B.Proc
readSelfProc = liftIO B.readSelfProc >>= either throwProcErrorM pure

{- | Read all the 'B.Proc's according to a 'TableConfig'.
Errors are ignored.
-}
readAllProcsLenient :: MonadIO m => TableConfig -> m [B.Proc]
readAllProcsLenient = liftIO . B.readAllProcsLenient

-- | Read all the 'B.Proc's according to a 'TableConfig'.
readAllProcs :: MonadIO m => TableConfig -> m [Either ProcError B.Proc]
readAllProcs = liftIO . B.readAllProcs

{- | Read all the 'B.Proc's according to a 'TableConfig'.
Any errors are thrown using 'throwProcErrorT'.
-}
readAllProcsThrow :: TableConfig -> ProcM [B.Proc]
readAllProcsThrow cfg = liftIO (B.readAllProcs cfg) >>= either throwProcErrorM pure . sequence
