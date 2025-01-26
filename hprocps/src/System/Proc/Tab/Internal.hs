module System.Proc.Tab.Internal
  ( ProcTab (..)
  , newProcTab
  , getProcTabInfo
  )
where

import Control.Monad.IO.Class
import qualified System.Proc.Bindings.Tab as B
import qualified System.Proc.Bindings.Tab.Config as B
import System.Proc.Monad

{- | This is the equivalent of 'B.ProcTab' from @procps-bindings@, see the documentation
there for more details.

The only difference is the @s@ parameter. This plays the same role as it does in
'Control.Monad.ST.ST' - see the documentation of 'RegionT' for more.
-}
newtype ProcTab s = UnsafeProcTab {unProcTab :: B.ProcTab}

-- | Read information about a 'ProcTab'.
getProcTabInfo :: MonadIO m => ProcTab s -> m B.ProcTabInfo
getProcTabInfo = liftIO . B.getProcTabInfo . unProcTab

{- | Create a 'ProcTab' from a 'B.TableConfig'. The 'Control.Monad.Trans.Resource.MonadResource'
instance ensures that internal resources will be freed after use.
-}
newProcTab :: B.TableConfig -> RegionM s (ProcTab s)
newProcTab cfg =
  UnsafeProcTab <$> allocateMEither (B.openProcTab cfg) B.closeProcTab
