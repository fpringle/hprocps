{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Proc.Monad.Internal where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

{- | This monad transformer gives us type safety in the same vein as the
'Control.Monad.ST.ST' monad. It is used by functions like 'System.Proc.readNextProc' etc
to ensure that our access to memory managed by unsafe pointers is limited to \"regions\".

For example, the following code will not compile:

@
main :: IO ()
main = void . 'System.Proc.Monad.runProcM' $ do
  p <- 'System.Proc.Monad.runRegionM' $ do
    -- we can open the 'System.Proc.Proc' inside t'System.Proc.Monad.RegionM'
    self <- 'System.Proc.readSelfProc'

    -- this is fine
    'System.Proc.procInfo' self >>= liftIO . print . 'System.Proc.Bindings.Info.cmd'

    -- this is not
    pure self

  -- this would cause undefined behaviour if it was allowed
  'System.Proc.procInfo' p >>= liftIO . print . 'System.Proc.Bindings.Info.cmd'
@

This is derived from a [technique](https://okmij.org/ftp/Haskell/regions.html#light-weight)
described by Oleg Kiselyov and Chung-chieh Shan, and demonstrated in the
[regions](https://hackage.haskell.org/package/regions) package.

Combined with the 'MonadResource' instance provided by 'System.Proc.Monad.ProcM' (see
t'System.Proc.Monad.RegionM' for a convenient type alias), this gives us strong
guarantees that we won't segfault or read garbage data from freed memory.
-}
newtype RegionT s m a = RegionT (m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadResource)
