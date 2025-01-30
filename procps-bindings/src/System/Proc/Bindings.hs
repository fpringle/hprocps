{- |

== Example

@
import Text.Show.Pretty
import System.Proc.Bindings
import System.Proc.Bindings.Tab

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

  result \<- 'withProcTab' config $ \\proctab -> do
    info <- 'getProcTabInfo' proctab
    pPrint info

    eProc <- 'readNextProc' proctab
    case eProc of
      Left err -> print err
      Right proc -> pPrint proc

  case result of
    Left err -> print err
    Right _ -> pure ()

  pure ()
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
module System.Proc.Bindings
  ( -- * Process information
    Proc

    -- ** Read the current process
  , System.Proc.Bindings.readSelfProc

    -- ** Read all processes
  , readAllProcs
  , readAllProcsLenient

    -- * Process table
  , ProcTab
  , readNextProc

    -- * Concrete process information
  , taskId
  , parentPid
  , majDelta
  , minDelta
  , cpuUsagePercent
  , processStateCode
  , userModeCPUTime
  , kernelModeCPUTime
  , cumulativeUserModeCPUTime
  , cumulativeKernelModeCPUTime
  , startTimeInSeconds
  , SignalMask
  , pendingSignalMask
  , blockSignalMask
  , ignoredSignalMask
  , caughtSignalMask
  , perTaskPendingSignals
  , Address
  , codeStartAddress
  , codeEndAddress
  , stackBottomAddress
  , kernelStackPointer
  , kernelInstructionPointer
  , kernelWaitChannelAddress
  , kernelSchedulingPriority
  , niceLevel
  , rss
  , alarm
  , totalVirtualMemInPages
  , residentNonSwappedMemInPages
  , sharedMemInPages
  , textResidentSetInPages
  , libraryResidentSetInPages
  , dataAndStackResidentSetInPages
  , dirtyPages
  , vmSizeInKb
  , vmLockedPagesInKb
  , vmRssInKb
  , vmRssAnonInKb
  , vmRssFileBackedInKb
  , vmRssSharedInKb
  , vmDataSizeInKb
  , vmStackSizeInKb
  , vmSwapSizeInKb
  , vmExeInKb
  , vmTotalLibraryPagesInKb
  , realTimePriority
  , schedulingClass
  , virtualMemoryInPages
  , residentSetSizeLimit
  , kernelFlags
  , minorPageFaults
  , majorPageFaults
  , cumulativeMinorPageFaults
  , cumulativeMajorPageFaults
  , environment
  , cmdline
  , cgroup
  , cgroupName
  , supplementaryGids
  , supplementaryGroupNames
  , effectiveUserName
  , realUserName
  , savedUserName
  , filesystemUserName
  , realGroupName
  , effectiveGroupname
  , savedGroupName
  , filesystemGroupName
  , cmd
  , processGroupId
  , sessionId
  , numberOfThreasds
  , threadGroupId
  , ttyNumber
  , effectiveUserId
  , effectiveGroupId
  , realUserId
  , realGroupId
  , savedUserId
  , savedGroupId
  , filesystemUserId
  , filesystemGroupId
  , terminalProcessGroupId
  , exitSignal
  , cpu
  , oomScore
  , oomAdjustment
  , namespaces
  , systemdContainerName
  , systemdSessionOwnerUid
  , systemdLoginSessionSeat
  , systemdLoginSessionId
  , systemdSliceUnit
  , systemdSystemUnitId
  , systemdUserUnitId
  , lxcContainerName
  )
where

import Control.Exception
import Data.Either
import Foreign
import GHC.Stack
import System.Proc.Bindings.C as C
import System.Proc.Bindings.C.Utils
import System.Proc.Bindings.Error
import System.Proc.Bindings.Tab
import System.Proc.Bindings.Tab.Internal

bracketReadProc :: HasCallStack => IO (Ptr Proc) -> IO (Either ProcError Proc)
bracketReadProc open =
  bracket open freeProc $ eitherPeek peekProc

-- | Read the next 'Proc' from the 'ProcTab'.
readNextProc :: HasCallStack => ProcTab -> IO (Either ProcError Proc)
readNextProc (UnsafeProcTab procTabPtr procPtr') = do
  readNextProcPtr procTabPtr procPtr'
    >>= eitherPeek peekProc

readProcTab' :: HasCallStack => TableConfig -> IO (Ptr (Ptr Proc))
readProcTab' cfg = do
  xprintf "readProcTab'"
  branchTableConfig readAllProcsSimple readAllProcsPids readAllProcsUids cfg

readPtrArray0 :: HasCallStack => Ptr (Ptr a) -> IO [Ptr a]
readPtrArray0 ptr = do
  xprintf $ "readPtrArray0 " <> show ptr
  if ptr == nullPtr
    then pure []
    else peekArray0 nullPtr ptr >>= xreturn

{- | Read all the 'Proc's according to a 'TableConfig'.
Errors are ignored.
-}
readAllProcsLenient :: HasCallStack => TableConfig -> IO [Proc]
readAllProcsLenient cfg = rights <$> readAllProcs cfg

-- | Read all the 'Proc's according to a 'TableConfig'.
readAllProcs :: HasCallStack => TableConfig -> IO [Either ProcError Proc]
readAllProcs cfg =
  bracket (readProcTab' cfg) freeIfNotNull $ \ptrArray ->
    bracket (readPtrArray0 ptrArray) (traverse freeProc) $ traverse $ eitherPeek peekProc
  where
    freeIfNotNull ptr
      | ptr == nullPtr = pure ()
      | otherwise = free ptr

-- | Try to read a 'Proc' representing the current proces or task.
readSelfProc :: HasCallStack => IO (Either ProcError Proc)
readSelfProc = bracketReadProc C.readSelfProc
