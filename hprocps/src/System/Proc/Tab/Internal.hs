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

newtype ProcTab s = UnsafeProcTab {unProcTab :: B.ProcTab}

getProcTabInfo :: MonadIO m => ProcTab s -> m B.ProcTabInfo
getProcTabInfo = liftIO . B.getProcTabInfo . unProcTab

newProcTab :: B.TableConfig -> RegionM s (ProcTab s)
newProcTab cfg =
  UnsafeProcTab <$> allocateMEither (B.openProcTab cfg) B.closeProcTab
