{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Proc.Monad.Internal where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

{- | This monad transformer gives us type safety in the same vein as the
'Control.Monad.ST.ST' monad. It is used by functions like 'System.Proc.readNextProc' etc
to ensure that our access to memory managed by unsafe pointers is limited to \"regions\".

For example, the following code uses only functions in "procps-bindings" and compiles fine,
but will probably segfault on the final line:

@
main :: IO ()
main = void $ do
  Right pt <- do
    let cfg :: 'System.Proc.Bindings.Tab.Config.TableConfig' = ...

    'System.Proc.Bindings.Tab.withProcTab' cfg $ \\proctab -> do
      'System.Proc.Bindings.readNextProc' proctab >>= print . fmap 'System.Proc.Bindings.cmd'

      pure proctab

  'System.Proc.Bindings.readNextProc' pt >>= print . fmap 'System.Proc.Bindings.cmd'
@

However, the following equivalent code will refuse to compile:

@
main :: IO ()
main = void . 'System.Proc.Monad.runProcM' $ do
  pt <- 'System.Proc.Monad.runRegionM' $ do
    let cfg :: 'System.Proc.Bindings.Tab.Config.TableConfig' = ...

    -- we can open the 'System.Proc.Tab.ProcTab' inside t'System.Proc.Monad.RegionM'
    proctab <- 'System.Proc.Tab.newProcTab' cfg

    -- this is fine
    'System.Proc.readNextProc' proctab >>= liftIO . print . 'System.Proc.Bindings.cmd'

    -- this is not
    pure proctab

  -- this would cause undefined behaviour if it was allowed
  'System.Proc.Tab.getProcTabInfo' pt >>= liftIO . print
@

This is derived from a [technique](https://okmij.org/ftp/Haskell/regions.html#light-weight)
described by Oleg Kiselyov and Chung-chieh Shan, and demonstrated in
the [regions](https://hackage.haskell.org/package/regions) package.

Combined with the 'MonadResource' instance provided by 'System.Proc.Monad.ProcM'
(see t'System.Proc.Monad.RegionM' for a convenient type alias), this gives us strong
guarantees that we won't segfault or read garbage data from freed memory.
-}
newtype RegionT s m a = RegionT (m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadResource)
