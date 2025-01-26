module System.Proc.Tab
  ( ProcTab
  , newProcTab
  , getProcTabInfo
  , releaseProcTab
  , readProcTabInfo
  )
where

import Control.Monad.IO.Class
import qualified System.Proc.Bindings.Error as B
import qualified System.Proc.Bindings.Tab as B
import qualified System.Proc.Bindings.Tab.Config as B
import System.Proc.Tab.Internal

{- | For convenience: read 'B.ProcTabInfo' according to a 'B.TableConfig', skipping all the C stuff
in the middle.
-}
readProcTabInfo :: MonadIO m => B.TableConfig -> m (Either B.ProcError B.ProcTabInfo)
readProcTabInfo = liftIO . B.readProcTabInfo
