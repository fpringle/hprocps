module System.Proc.Tab
  ( ProcTab
  , newProcTab
  , getProcTabInfo
  , readProcTabInfo
  )
where

import Control.Monad.IO.Class
import qualified System.Proc.Bindings.Error as B
import qualified System.Proc.Bindings.Tab as B
import qualified System.Proc.Bindings.Tab.Config as B
import System.Proc.Tab.Internal

readProcTabInfo :: MonadIO m => B.TableConfig -> m (Either B.ProcError B.ProcTabInfo)
readProcTabInfo = liftIO . B.readProcTabInfo
