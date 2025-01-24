{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Proc.Monad.Internal where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

newtype RegionT s m a = RegionT (m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadResource)
