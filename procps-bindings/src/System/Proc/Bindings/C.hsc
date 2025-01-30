{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
-- {-# OPTIONS_GHC -fplugin Debug.Breakpoint #-}

#include "proc-handle.h"

module System.Proc.Bindings.C
  ( Proc (..)
  , SignalMask
  , Address
  , callocProc
  , freeProc
  , peekProc
  , readNextProcPtr
  , readAllProcsSimple
  , readAllProcsPids
  , readAllProcsUids
  , readSelfProc
  )
where

-- import Debug.Breakpoint
import Foreign
import Foreign.C.Types
import GHC.Stack
import System.Posix.Types
import System.Proc.Bindings.C.Utils
import System.Proc.Bindings.Tab.C

{- | The reason we type alias this is because the procps library will use a different
type to represent addresses depending on the system and some other C macro stuff.

Specifically:

@
if defined(k64test) || (defined(_ABIN32) && _MIPS_SIM == _ABIN32)
type Address = 'CULLong'
else
type Address = 'CULong'
endif
@
-}
#if defined(k64test) || (defined(_ABIN32) && _MIPS_SIM == _ABIN32)
type Address = CULLong
#else
type Address = CULong
#endif

{- | The reason we type alias this is because the @procps@ library will use a different
type to represent signal masks depending on whether the @SIGNAL_STRING@ macro is defined.

Specifically:

@
ifdef SIGNAL_STRING
type SignalMask = Maybe String
else
type SignalMask = 'CLLong'
endif
@
-}
#ifdef SIGNAL_STRING
type SignalMask = Maybe String
#else
type SignalMask = CLLong
#endif

{- | All the information we've read about a single process.

This is the "escape hatch" from all the funky C stuff, memory management, bracketed access etc:
'Proc' is just a normal Haskell datatype without any pointers going on, so you can
do what you want with it.

The fields of 'Proc' come from @proc_t@, and are parsed from various files
in the @\/proc\/#\/@ directory.

For more detailed information, see the following man pages:

- [proc_pid_statm.5](https://man7.org/linux/man-pages/man5/proc_pid_statm.5.html)
- [proc_pid_status.5](https://man7.org/linux/man-pages/man5/proc_pid_status.5.html)
- [proc_pid_ns.5](https://man7.org/linux/man-pages/man5/proc_pid_ns.5.html)
- [proc_pid_stat.5](https://man7.org/linux/man-pages/man5/proc_pid_stat.5.html)
- [proc_pid_environ.5](https://man7.org/linux/man-pages/man5/proc_pid_environ.5.html)
- [proc_pid_cmdline.5](https://man7.org/linux/man-pages/man5/proc_pid_cmdline.5.html)
- [proc_pid_cgroup.5](https://man7.org/linux/man-pages/man5/proc_pid_cgroup.5.html)
- [proc_pid_task.5](https://man7.org/linux/man-pages/man5/proc_pid_task.5.html)
-}
data Proc = Proc
  { taskId :: CInt 
  {- ^ Task ID, the POSIX thread ID (see also: 'threadGroupId').

  Always read.
  -}
  , parentPid :: CInt 
  {- ^ Process ID of parent process.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat' or
  'System.Proc.Bindings.Tab.Config.flagStatus'.
  -}
  , majDelta :: CULong 
  {- ^ Major page faults since last update.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat' (special).
  -}
  , minDelta :: CULong 
  {- ^ Minor page faults since last update.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat' (special).
  -}
  , cpuUsagePercent :: CUInt 
  {- ^ %CPU usage (is not filled in by 'System.Proc.Bindings.readNextProc').

  Read when 'System.Proc.Bindings.Tab.Config.flagStat' (special).
  -}
  , processStateCode :: CChar 
  {- ^ Single-char code for process state.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat' or
  'System.Proc.Bindings.Tab.Config.flagStatus'.

  From [proc_pid_stat](https://man7.org/linux/man-pages/man5/proc_pid_stat.5.html):

  @
  R      Running

  S      Sleeping in an interruptible wait

  D      Waiting in uninterruptible disk sleep

  Z      Zombie

  T      Stopped (on a signal) or (before Linux 2.6.33) trace stopped

  t      Tracing stop (Linux 2.6.33 onward)

  W      Paging (only before Linux 2.6.0)

  X      Dead (from Linux 2.6.0 onward)

  x      Dead (Linux 2.6.33 to 3.13 only)

  K      Wakekill (Linux 2.6.33 to 3.13 only)

  W      Waking (Linux 2.6.33 to 3.13 only)

  P      Parked (Linux 3.9 to 3.13 only)

  I      Idle (Linux 4.14 onward)
  @
  -}
  -- , procc_pad_1 :: CChar 
  -- , procc_pad_2 :: CChar 
  -- , procc_pad_3 :: CChar 
  , userModeCPUTime :: CULLong 
  {- ^ User-mode CPU time accumulated by process.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , kernelModeCPUTime :: CULLong 
  {- ^ Kernel-mode CPU time accumulated by process.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , cumulativeUserModeCPUTime :: CULLong 
  {- ^ Cumulative utime of process and reaped children.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , cumulativeKernelModeCPUTime :: CULLong 
  {- ^ Cumulative stime of process and reaped children.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , startTimeInSeconds :: CULLong 
  {- ^ Start time of process: seconds since system boot.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , pendingSignalMask :: SignalMask
  {- ^ Mask of pending signals, per-task for readtask() but per-proc for readproc().

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , blockSignalMask :: SignalMask
  {- ^ Mask of blocked signals.

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , ignoredSignalMask :: SignalMask
  {- ^ Mask of ignored signals.

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , caughtSignalMask :: SignalMask
  {- ^ Mask of caught signals.

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , perTaskPendingSignals :: SignalMask
  {- ^ Mask of per task pending signals.

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , codeStartAddress :: Address 
  {- ^ Address of beginning of code segment.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , codeEndAddress :: Address 
  {- ^ Address of end of code segment.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , stackBottomAddress :: Address 
  {- ^ Address of the bottom of stack for the process.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , kernelStackPointer :: Address 
  {- ^ Kernel stack pointer.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , kernelInstructionPointer :: Address 
  {- ^ Kernel instruction pointer.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , kernelWaitChannelAddress :: Address 
  {- ^ Address of kernel wait channel proc is sleeping in.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat' (special).
  -}
  , kernelSchedulingPriority :: CLong 
  {- ^ Kernel scheduling priority.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , niceLevel :: CLong 
  {- ^ Standard unix nice level of process.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , rss :: CLong 
  {- ^ Identical to 'residentNonSwappedMemInPages'.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , alarm :: CLong 
  {- ^ Not used since Linux 2.6.17.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.

  From [proc_pid_stat](https://man7.org/linux/man-pages/man5/proc_pid_stat.5.html):

  @
  The time in jiffies before the next SIGALRM is sent
  to the process due to an interval timer.  Since
  Linux 2.6.17, this field is no longer maintained,
  and is hard coded as 0.
  @
  -}
  , totalVirtualMemInPages :: CLong 
  {- ^ Total virtual memory (as # pages).

  Read when 'System.Proc.Bindings.Tab.Config.flagMem'.
  -}
  , residentNonSwappedMemInPages :: CLong 
  {- ^ Resident non-swapped memory (as # pages).

  Read when 'System.Proc.Bindings.Tab.Config.flagMem'.
  -}
  , sharedMemInPages :: CLong 
  {- ^ Shared (mmap'd) memory (as # pages).

  Read when 'System.Proc.Bindings.Tab.Config.flagMem'.
  -}
  , textResidentSetInPages :: CLong 
  {- ^ Text (exe) resident set (as # pages).

  Read when 'System.Proc.Bindings.Tab.Config.flagMem'.
  -}
  , libraryResidentSetInPages :: CLong 
  {- ^ Library resident set (always 0 w/ 2.6).

  Read when 'System.Proc.Bindings.Tab.Config.flagMem'.
  -}
  , dataAndStackResidentSetInPages :: CLong 
  {- ^ Data+stack resident set (as # pages).

  Read when 'System.Proc.Bindings.Tab.Config.flagMem'.
  -}
  , dirtyPages :: CLong 
  {- ^ Dirty pages (always 0 w/ 2.6).

  Read when 'System.Proc.Bindings.Tab.Config.flagMem'.
  -}
  , vmSizeInKb :: CULong 
  {- ^ Equals 'totalVirtualMemInPages' (as kb).

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , vmLockedPagesInKb :: CULong 
  {- ^ Locked pages (as kb).

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , vmRssInKb :: CULong 
  {- ^ Equals 'rss' and/or 'residentNonSwappedMemInPages' (as kb).

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , vmRssAnonInKb :: CULong 
  {- ^ The @anonymous@ portion of vm_rss (as kb).

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , vmRssFileBackedInKb :: CULong 
  {- ^ The @file-backed@ portion of vm_rss (as kb).

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , vmRssSharedInKb :: CULong 
  {- ^ The @shared@ portion of vm_rss (as kb).

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , vmDataSizeInKb :: CULong 
  {- ^ Data only size (as kb).

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , vmStackSizeInKb :: CULong 
  {- ^ Stack only size (as kb).

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , vmSwapSizeInKb :: CULong 
  {- ^ Based on linux-2.6.34 "swap ents" (as kb).

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , vmExeInKb :: CULong 
  {- ^ Equals 'textResidentSetInPages' (as kb).

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , vmTotalLibraryPagesInKb :: CULong 
  {- ^ Total, not just used, library pages (as kb).

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , realTimePriority :: CULong 
  {- ^ Real-time priority.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , schedulingClass :: CULong 
  {- ^ Scheduling class.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , virtualMemoryInPages :: CULong 
  {- ^ Number of pages of virtual memory.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , residentSetSizeLimit :: CULong 
  {- ^ Resident set size limit?

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , kernelFlags :: CULong 
  {- ^ Kernel flags for the process.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , minorPageFaults :: CULong 
  {- ^ Number of minor page faults since process start.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , majorPageFaults :: CULong 
  {- ^ Number of major page faults since process start.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , cumulativeMinorPageFaults :: CULong 
  {- ^ Cumulative min_flt of process and child processes.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , cumulativeMajorPageFaults :: CULong 
  {- ^ Cumulative maj_flt of process and child processes.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , environment :: [String]
  {- ^ Environment string vector (/proc/#/environ).

  Read when 'System.Proc.Bindings.Tab.Config.flagEnviron'.
  -}
  , cmdline :: [String]
  {- ^ Command line string vector (/proc/#/cmdline).

  Read when 'System.Proc.Bindings.Tab.Config.fillCom'
  or 'System.Proc.Bindings.Tab.Config.fillArg'.
  -}
  , cgroup :: [String]
  {- ^ Cgroup string vector (/proc/#/cgroup).

  Read when 'System.Proc.Bindings.Tab.Config.flagCGroup'.
  -}
  , cgroupName :: Maybe String
  {- ^ Name portion of above (if possible).

  Read when 'System.Proc.Bindings.Tab.Config.flagCGroup'
  and not 'System.Proc.Bindings.Tab.Config.editCGroupAsSingleVector'.
  -}
  , supplementaryGids :: Maybe String
  {- ^ Supplementary gids as comma delimited str.

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , supplementaryGroupNames :: Maybe String
  -- ^ Supp grp names as comma delimited str, derived from supgid.
  , effectiveUserName :: Maybe String
  {- ^ Effective user name.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat' or
  'System.Proc.Bindings.Tab.Config.flagStatus'.
  -}
  , realUserName :: Maybe String
  {- ^ Real user name.

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , savedUserName :: Maybe String
  {- ^ Saved user name.

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , filesystemUserName :: Maybe String
  {- ^ Filesystem user name.

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , realGroupName :: Maybe String
  {- ^ Real group name.

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , effectiveGroupname :: Maybe String
  {- ^ Effective group name.

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , savedGroupName :: Maybe String
  {- ^ Saved group name.

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , filesystemGroupName :: Maybe String
  {- ^ Filesystem group name.

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , cmd :: Maybe String
  {- ^ Basename of executable file in call to exec(2).

  Read when 'System.Proc.Bindings.Tab.Config.flagStat' or
  'System.Proc.Bindings.Tab.Config.flagStatus'.
  -}
  -- , procc_ring :: () -- struct proc_t *
  -- , procc_next :: () -- struct proc_t *
  , processGroupId :: CInt 
  {- ^ Process group ID.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , sessionId :: CInt 
  {- ^ Session ID.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , numberOfThreasds :: CInt 
  {- ^ Number of threads, or 0 if no clue.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat' or
  'System.Proc.Bindings.Tab.Config.flagStatus'.
  -}
  , threadGroupId :: CInt 
  {- ^ Thread group ID, the POSIX PID (see also: 'taskId').

  Always read.
  -}
  , ttyNumber :: CInt 
  {- ^ Full device number of controlling terminal.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , effectiveUserId :: CInt 
  {- ^ Effective user ID.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat' or
  'System.Proc.Bindings.Tab.Config.flagStatus'.
  -}
  , effectiveGroupId :: CInt 
  {- ^ Effective group ID.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat' or
  'System.Proc.Bindings.Tab.Config.flagStatus'.
  -}
  , realUserId :: CInt 
  {- ^ Real user ID.

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , realGroupId :: CInt 
  {- ^ Real group ID.

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , savedUserId :: CInt 
  {- ^ Saved user ID.

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , savedGroupId :: CInt 
  {- ^ Saved group ID.

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , filesystemUserId :: CInt 
  {- ^ Fs user ID (used for file access only).

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , filesystemGroupId :: CInt 
  {- ^ Fs group ID (used for file access only).

  Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
  -}
  , terminalProcessGroupId :: CInt 
  {- ^ Terminal process group ID.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , exitSignal :: CInt 
  {- ^ Might not be SIGCHLD.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , cpu :: CInt 
  {- ^ Current (or most recent?) CPU.

  Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
  -}
  , oomScore :: CInt 
  {- ^ (badness for OOM killer).

  Read when 'System.Proc.Bindings.Tab.Config.flagOOM'.
  -}
  , oomAdjustment :: CInt 
  {- ^ (adjustment to OOM score).

  Read when 'System.Proc.Bindings.Tab.Config.flagOOM'.
  -}
  , namespaces :: [CLong]
  {- ^ Inode number of namespaces.

  Read when 'System.Proc.Bindings.Tab.Config.flagNS'.
  -}
  , systemdContainerName :: Maybe String
  -- ^ Systemd vm/container name.
  , systemdSessionOwnerUid :: Maybe String
  -- ^ Systemd session owner uid.
  , systemdLoginSessionSeat :: Maybe String
  -- ^ Systemd login session seat.
  , systemdLoginSessionId :: Maybe String
  -- ^ Systemd login session ID.
  , systemdSliceUnit :: Maybe String
  -- ^ Systemd slice unit.
  , systemdSystemUnitId :: Maybe String
  -- ^ Systemd system unit ID.
  , systemdUserUnitId :: Maybe String
  -- ^ Systemd user unit ID.
  , lxcContainerName :: Maybe String
  -- ^ Lxc container name.
  }
  deriving (Show, Eq)

readLongList :: Ptr CLong -> IO [CLong]
readLongList = readNullTermArray
{-# INLINE readLongList #-}

peekProc :: HasCallStack => Ptr Proc -> IO Proc
peekProc ptr = do
  xprintf $ "peekProc " <> show ptr
  taskId <- (#peek proc_t, tid) ptr
  parentPid <- (#peek proc_t, ppid) ptr
  majDelta <- (#peek proc_t, maj_delta) ptr
  minDelta <- (#peek proc_t, min_delta) ptr
  cpuUsagePercent <- (#peek proc_t, pcpu) ptr
  processStateCode <- (#peek proc_t, state) ptr
  -- procc_pad_1 <- (#peek proc_t, pad_1) ptr
  -- procc_pad_2 <- (#peek proc_t, pad_2) ptr
  -- procc_pad_3 <- (#peek proc_t, pad_3) ptr
  userModeCPUTime <- (#peek proc_t, utime) ptr
  kernelModeCPUTime <- (#peek proc_t, stime) ptr
  cumulativeUserModeCPUTime <- (#peek proc_t, cutime) ptr
  cumulativeKernelModeCPUTime <- (#peek proc_t, cstime) ptr
  startTimeInSeconds <- (#peek proc_t, start_time) ptr
#ifdef SIGNAL_STRING
  pendingSignalMask <- (readStringMaybe $ (#ptr proc_t, signal) ptr)
  blockSignalMask <- (readStringMaybe $ (#ptr proc_t, blocked) ptr)
  ignoredSignalMask <- (readStringMaybe $ (#ptr proc_t, sigignore) ptr)
  caughtSignalMask <- (readStringMaybe $ (#ptr proc_t, sigcatch) ptr)
  perTaskPendingSignals <- (readStringMaybe $ (#ptr proc_t, _sigpnd) ptr)
#else
  pendingSignalMask <- (#peek proc_t, signal) ptr
  blockSignalMask <- (#peek proc_t, blocked) ptr
  ignoredSignalMask <- (#peek proc_t, sigignore) ptr
  caughtSignalMask <- (#peek proc_t, sigcatch) ptr
  perTaskPendingSignals <- (#peek proc_t, _sigpnd) ptr
#endif
  codeStartAddress <- (#peek proc_t, start_code) ptr
  codeEndAddress <- (#peek proc_t, end_code) ptr
  stackBottomAddress <- (#peek proc_t, start_stack) ptr
  kernelStackPointer <- (#peek proc_t, kstk_esp) ptr
  kernelInstructionPointer <- (#peek proc_t, kstk_eip) ptr
  kernelWaitChannelAddress <- (#peek proc_t, wchan) ptr
  kernelSchedulingPriority <- (#peek proc_t, priority) ptr
  niceLevel <- (#peek proc_t, nice) ptr
  rss <- (#peek proc_t, rss) ptr
  alarm <- (#peek proc_t, alarm) ptr
  totalVirtualMemInPages <- (#peek proc_t, size) ptr
  residentNonSwappedMemInPages <- (#peek proc_t, resident) ptr
  sharedMemInPages <- (#peek proc_t, share) ptr
  textResidentSetInPages <- (#peek proc_t, trs) ptr
  libraryResidentSetInPages <- (#peek proc_t, lrs) ptr
  dataAndStackResidentSetInPages <- (#peek proc_t, drs) ptr
  dirtyPages <- (#peek proc_t, dt) ptr
  vmSizeInKb <- (#peek proc_t, vm_size) ptr
  vmLockedPagesInKb <- (#peek proc_t, vm_lock) ptr
  vmRssInKb <- (#peek proc_t, vm_rss) ptr
  vmRssAnonInKb <- (#peek proc_t, vm_rss_anon) ptr
  vmRssFileBackedInKb <- (#peek proc_t, vm_rss_file) ptr
  vmRssSharedInKb <- (#peek proc_t, vm_rss_shared) ptr
  vmDataSizeInKb <- (#peek proc_t, vm_data) ptr
  vmStackSizeInKb <- (#peek proc_t, vm_stack) ptr
  vmSwapSizeInKb <- (#peek proc_t, vm_swap) ptr
  vmExeInKb <- (#peek proc_t, vm_exe) ptr
  vmTotalLibraryPagesInKb <- (#peek proc_t, vm_lib) ptr
  realTimePriority <- (#peek proc_t, rtprio) ptr
  schedulingClass <- (#peek proc_t, sched) ptr
  virtualMemoryInPages <- (#peek proc_t, vsize) ptr
  residentSetSizeLimit <- (#peek proc_t, rss_rlim) ptr
  kernelFlags <- (#peek proc_t, flags) ptr
  minorPageFaults <- (#peek proc_t, min_flt) ptr
  majorPageFaults <- (#peek proc_t, maj_flt) ptr
  cumulativeMinorPageFaults <- (#peek proc_t, cmin_flt) ptr
  cumulativeMajorPageFaults <- (#peek proc_t, cmaj_flt) ptr
  environment <- readStringList =<< (#peek proc_t, environ) ptr
  cmdline <- readStringList =<< (#peek proc_t, cmdline) ptr
  cgroup <- readStringList =<< (#peek proc_t, cgroup) ptr
  cgroupName <- readStringMaybe =<< (#peek proc_t, cgname) ptr
  supplementaryGids <- readStringMaybe =<< (#peek proc_t, supgid) ptr
  supplementaryGroupNames <- readStringMaybe =<< (#peek proc_t, supgrp) ptr
  effectiveUserName <- readStringMaybe ((#ptr proc_t, euser) ptr)
  realUserName <- readStringMaybe ((#ptr proc_t, ruser) ptr)
  savedUserName <- readStringMaybe ((#ptr proc_t, suser) ptr)
  filesystemUserName <- readStringMaybe ((#ptr proc_t, fuser) ptr)
  realGroupName <- readStringMaybe ((#ptr proc_t, rgroup) ptr)
  effectiveGroupname <- readStringMaybe ((#ptr proc_t, egroup) ptr)
  savedGroupName <- readStringMaybe ((#ptr proc_t, sgroup) ptr)
  filesystemGroupName <- readStringMaybe ((#ptr proc_t, fgroup) ptr)
  cmd <- readStringMaybe ((#ptr proc_t, cmd) ptr)
  -- procc_ring <- (#peek proc_t, ring) ptr
  -- procc_next <- (#peek proc_t, next) ptr
  processGroupId <- (#peek proc_t, pgrp) ptr
  sessionId <- (#peek proc_t, session) ptr
  numberOfThreasds <- (#peek proc_t, nlwp) ptr
  threadGroupId <- (#peek proc_t, tgid) ptr
  ttyNumber <- (#peek proc_t, tty) ptr
  effectiveUserId <- (#peek proc_t, euid) ptr
  effectiveGroupId <- (#peek proc_t, egid) ptr
  realUserId <- (#peek proc_t, ruid) ptr
  realGroupId <- (#peek proc_t, rgid) ptr
  savedUserId <- (#peek proc_t, suid) ptr
  savedGroupId <- (#peek proc_t, sgid) ptr
  filesystemUserId <- (#peek proc_t, fuid) ptr
  filesystemGroupId <- (#peek proc_t, fgid) ptr
  terminalProcessGroupId <- (#peek proc_t, tpgid) ptr
  exitSignal <- (#peek proc_t, exit_signal) ptr
  cpu <- (#peek proc_t, processor) ptr
  oomScore <- (#peek proc_t, oom_score) ptr
  oomAdjustment <- (#peek proc_t, oom_adj) ptr
  namespaces <- readLongList ((#ptr proc_t, ns) ptr)
  systemdContainerName <- readStringMaybe =<< (#peek proc_t, sd_mach) ptr
  systemdSessionOwnerUid <- readStringMaybe =<< (#peek proc_t, sd_ouid) ptr
  systemdLoginSessionSeat <- readStringMaybe =<< (#peek proc_t, sd_seat) ptr
  systemdLoginSessionId <- readStringMaybe =<< (#peek proc_t, sd_sess) ptr
  systemdSliceUnit <- readStringMaybe =<< (#peek proc_t, sd_slice) ptr
  systemdSystemUnitId <- readStringMaybe =<< (#peek proc_t, sd_unit) ptr
  systemdUserUnitId <- readStringMaybe =<< (#peek proc_t, sd_uunit) ptr
  lxcContainerName <- readStringMaybe =<< (#peek proc_t, lxcname) ptr
  pure Proc {..}

callocProc :: IO (Ptr Proc)
callocProc = callocBytes (#size proc_t)

foreign import capi unsafe "read_proc_wrapper" readNextProcPtr :: Ptr ProcTabC -> Ptr Proc -> IO (Ptr Proc)

foreign import capi unsafe "free_proc_wrapper" freeProc :: Ptr Proc -> IO ()

foreign import capi unsafe "readallprocs_simple" readAllProcsSimple :: CInt -> IO (Ptr (Ptr Proc))

foreign import capi unsafe "readallprocs_pids" readAllProcsPids :: CInt -> Ptr CPid -> IO (Ptr (Ptr Proc))

foreign import capi unsafe "readallprocs_uids" readAllProcsUids :: CInt -> Ptr CUid -> CInt -> IO (Ptr (Ptr Proc))

foreign import capi unsafe "lookup_self_wrapper" readSelfProc :: IO (Ptr Proc)
