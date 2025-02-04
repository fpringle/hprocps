{- |

== Example

@
import Control.Monad.IO.Class
import System.Proc
import System.Proc.Tab
import Text.Show.Pretty

main :: IO ()
main = do
  let flags =
        'fillMem'
          <> 'fillCom'
          <> 'fillUser'
          <> 'fillGroup'
          <> 'fillStatus'
          <> 'fillStat'
          <> 'fillArg'
      procFilter = 'ByUids' [1000]
      config = 'TableConfig' flags procFilter

  result <- 'runProcM' $ 'runRegionT' $ do
    proctab <- 'newProcTab' config
    info <- 'getProcTabInfo' proctab
    liftIO $ pPrint info

    proc <- 'readNextProc' proctab
    liftIO $ pPrint proc

  case result of
    Left err -> print err
    Right _ -> pure ()
@

Output:

@
ProcTabInfo
  { didFake = False
  , pids = []
  , uids = [ 1000 ]
  , flags = 16763
  , path = ""
  }
Proc
  { taskId = 1962
  , parentPid = 1
  , userModeCPUTime = 5
  , startTimeInSeconds = 904
  , ...
  }
@
-}
module System.Proc
  ( -- * Process handles
    B.Proc
  , readSelfProc
  , readAllProcs
  , readAllProcsLenient
  , readAllProcsThrow

    -- * Process table
  , ProcTab
  , readNextProcMaybe

    -- * Regioned monad for resource safety
  , module System.Proc.Monad

    -- * Re-exports
  , module System.Proc.Bindings.Tab.Config
  , module System.Proc.Bindings.Error
  )
where

import Control.Monad.IO.Class
import qualified System.Proc.Bindings as B
import System.Proc.Bindings.Error
import System.Proc.Bindings.Tab.Config
import System.Proc.Monad
import System.Proc.Tab.Internal

-- | Read the next 'B.Proc' from the 'ProcTab', if there is one. Otherwise return 'Nothing'.
readNextProcMaybe :: ProcTab s -> RegionM s (Maybe B.Proc)
readNextProcMaybe (UnsafeProcTab _ pt) = liftIO (B.readNextProc pt)

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
