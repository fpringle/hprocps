module System.Proc.Bindings.Info.Internal where

import Foreign.C.Types
import GHC.Show
import System.Proc.Bindings.C

{- | All the information we've read about a single process.

This is the "escape hatch" from all the funky C stuff, memory management, bracketed access etc:
'ProcInfo' is just a normal Haskell datatype without any pointers going on, so you can
do what you want with it.
-}
newtype ProcInfo = ProcInfo {unProcInfo :: ProcInfo'}

instance Show ProcInfo where
  showsPrec d pInfo =
    showParen (d > 10) $
      showString "ProcInfo {" . showFields . showString "}"
    where
      fields :: [(String, ProcInfo -> String)]
      fields =
        [ ("taskId", show . taskId)
        , ("parentPid", show . parentPid)
        , ("majDelta", show . majDelta)
        , ("minDelta", show . minDelta)
        , ("cpuUsagePercent", show . cpuUsagePercent)
        , ("processStateCode", show . processStateCode)
        , ("userModeCPUTime", show . userModeCPUTime)
        , ("kernelModeCPUTime", show . kernelModeCPUTime)
        , ("cumulativeUserModeCPUTime", show . cumulativeUserModeCPUTime)
        , ("cumulativeKernelModeCPUTime", show . cumulativeKernelModeCPUTime)
        , ("startTimeInSeconds", show . startTimeInSeconds)
        , ("pendingSignalMask", show . pendingSignalMask)
        , ("blockSignalMask", show . blockSignalMask)
        , ("ignoredSignalMask", show . ignoredSignalMask)
        , ("caughtSignalMask", show . caughtSignalMask)
        , ("perTaskPendingSignals", show . perTaskPendingSignals)
        , ("codeStartAddress", show . codeStartAddress)
        , ("codeEndAddress", show . codeEndAddress)
        , ("stackBottomAddress", show . stackBottomAddress)
        , ("kernelStackPointer", show . kernelStackPointer)
        , ("kernelInstructionPointer", show . kernelInstructionPointer)
        , ("kernelWaitChannelAddress", show . kernelWaitChannelAddress)
        , ("kernelSchedulingPriority", show . kernelSchedulingPriority)
        , ("niceLevel", show . niceLevel)
        , ("rss", show . rss)
        , ("alarm", show . alarm)
        , ("totalVirtualMemInPages", show . totalVirtualMemInPages)
        , ("residentNonSwappedMemInPages", show . residentNonSwappedMemInPages)
        , ("sharedMemInPages", show . sharedMemInPages)
        , ("textResidentSetInPages", show . textResidentSetInPages)
        , ("libraryResidentSetInPages", show . libraryResidentSetInPages)
        , ("dataAndStackResidentSetInPages", show . dataAndStackResidentSetInPages)
        , ("dirtyPages", show . dirtyPages)
        , ("vmSizeInKb", show . vmSizeInKb)
        , ("vmLockedPagesInKb", show . vmLockedPagesInKb)
        , ("vmRssInKb", show . vmRssInKb)
        , ("vmRssAnonInKb", show . vmRssAnonInKb)
        , ("vmRssFileBackedInKb", show . vmRssFileBackedInKb)
        , ("vmRssSharedInKb", show . vmRssSharedInKb)
        , ("vmDataSizeInKb", show . vmDataSizeInKb)
        , ("vmStackSizeInKb", show . vmStackSizeInKb)
        , ("vmSwapSizeInKb", show . vmSwapSizeInKb)
        , ("vmExeInKb", show . vmExeInKb)
        , ("vmTotalLibraryPagesInKb", show . vmTotalLibraryPagesInKb)
        , ("realTimePriority", show . realTimePriority)
        , ("schedulingClass", show . schedulingClass)
        , ("virtualMemoryInPages", show . virtualMemoryInPages)
        , ("residentSetSizeLimit", show . residentSetSizeLimit)
        , ("kernelFlags", show . kernelFlags)
        , ("minorPageFaults", show . minorPageFaults)
        , ("majorPageFaults", show . majorPageFaults)
        , ("cumulativeMinorPageFaults", show . cumulativeMinorPageFaults)
        , ("cumulativeMajorPageFaults", show . cumulativeMajorPageFaults)
        , ("environment", show . environment)
        , ("cmdline", show . cmdline)
        , ("cgroup", show . cgroup)
        , ("cgroupName", show . cgroupName)
        , ("supplementaryGids", show . supplementaryGids)
        , ("supplementaryGroupNames", show . supplementaryGroupNames)
        , ("effectiveUserName", show . effectiveUserName)
        , ("realUserName", show . realUserName)
        , ("savedUserName", show . savedUserName)
        , ("filesystemUserName", show . filesystemUserName)
        , ("realGroupName", show . realGroupName)
        , ("effectiveGroupname", show . effectiveGroupname)
        , ("savedGroupName", show . savedGroupName)
        , ("filesystemGroupName", show . filesystemGroupName)
        , ("cmd", show . cmd)
        , ("processGroupId", show . processGroupId)
        , ("sessionId", show . sessionId)
        , ("numberOfThreasds", show . numberOfThreasds)
        , ("threadGroupId", show . threadGroupId)
        , ("ttyNumber", show . ttyNumber)
        , ("effectiveUserId", show . effectiveUserId)
        , ("effectiveGroupId", show . effectiveGroupId)
        , ("realUserId", show . realUserId)
        , ("realGroupId", show . realGroupId)
        , ("savedUserId", show . savedUserId)
        , ("savedGroupId", show . savedGroupId)
        , ("filesystemUserId", show . filesystemUserId)
        , ("filesystemGroupId", show . filesystemGroupId)
        , ("terminalProcessGroupId", show . terminalProcessGroupId)
        , ("exitSignal", show . exitSignal)
        , ("cpu", show . cpu)
        , ("oomScore", show . oomScore)
        , ("oomAdjustment", show . oomAdjustment)
        , ("namespaces", show . namespaces)
        , ("systemdContainerName", show . systemdContainerName)
        , ("systemdSessionOwnerUid", show . systemdSessionOwnerUid)
        , ("systemdLoginSessionSeat", show . systemdLoginSessionSeat)
        , ("systemdLoginSessionId", show . systemdLoginSessionId)
        , ("systemdSliceUnit", show . systemdSliceUnit)
        , ("systemdSystemUnitId", show . systemdSystemUnitId)
        , ("systemdUserUnitId", show . systemdUserUnitId)
        , ("lxcContainerName", show . lxcContainerName)
        ]

      showFields = go fields

      go [] = id
      go [(field, getField)] =
        showString field
          . showString " = "
          . showString (getField pInfo)
      go ((field, getField) : rest) =
        showString field
          . showString " = "
          . showString (getField pInfo)
          . showCommaSpace
          . go rest

------------------------------------------------------------
-- Accessors
-- These all come from fields of proc_t in readproc.h

{- | Task ID, the POSIX thread ID (see also: 'threadGroupId').

Always read.
-}
taskId :: ProcInfo -> CInt
taskId = procc_tid . unProcInfo

{- | Process ID of parent process.

Read when 'System.Proc.Bindings.Tab.Config.flagStat' or
'System.Proc.Bindings.Tab.Config.flagStatus'.
-}
parentPid :: ProcInfo -> CInt
parentPid = procc_ppid . unProcInfo

{- | Major page faults since last update.

Read when 'System.Proc.Bindings.Tab.Config.flagStat' (special).
-}
majDelta :: ProcInfo -> CULong
majDelta = procc_maj_delta . unProcInfo

{- | Minor page faults since last update.

Read when 'System.Proc.Bindings.Tab.Config.flagStat' (special).
-}
minDelta :: ProcInfo -> CULong
minDelta = procc_min_delta . unProcInfo

{- | %CPU usage (is not filled in by 'System.Proc.Bindings.readNextProc').

Read when 'System.Proc.Bindings.Tab.Config.flagStat' (special).
-}
cpuUsagePercent :: ProcInfo -> CUInt
cpuUsagePercent = procc_pcpu . unProcInfo

{- | Single-char code for process state.

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
processStateCode :: ProcInfo -> CChar
processStateCode = procc_state . unProcInfo

{-
procc_pad_1 :: ProcInfo -> CChar
procc_pad_1 = procc_pad_1 . unProcInfo

procc_pad_2 :: ProcInfo -> CChar
procc_pad_2 = procc_pad_2 . unProcInfo

procc_pad_3 :: ProcInfo -> CChar
procc_pad_3 = procc_pad_3 . unProcInfo
-}

{- | User-mode CPU time accumulated by process.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
userModeCPUTime :: ProcInfo -> CULLong
userModeCPUTime = procc_utime . unProcInfo

{- | Kernel-mode CPU time accumulated by process.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
kernelModeCPUTime :: ProcInfo -> CULLong
kernelModeCPUTime = procc_stime . unProcInfo

{- | Cumulative utime of process and reaped children.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
cumulativeUserModeCPUTime :: ProcInfo -> CULLong
cumulativeUserModeCPUTime = procc_cutime . unProcInfo

{- | Cumulative stime of process and reaped children.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
cumulativeKernelModeCPUTime :: ProcInfo -> CULLong
cumulativeKernelModeCPUTime = procc_cstime . unProcInfo

{- | Start time of process: seconds since system boot.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
startTimeInSeconds :: ProcInfo -> CULLong
startTimeInSeconds = procc_start_time . unProcInfo

{- | Mask of pending signals, per-task for readtask() but per-proc for readproc().

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
pendingSignalMask :: ProcInfo -> SignalMask
pendingSignalMask = procc_signal . unProcInfo

{- | Mask of blocked signals.

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
blockSignalMask :: ProcInfo -> SignalMask
blockSignalMask = procc_blocked . unProcInfo

{- | Mask of ignored signals.

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
ignoredSignalMask :: ProcInfo -> SignalMask
ignoredSignalMask = procc_sigignore . unProcInfo

{- | Mask of caught signals.

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
caughtSignalMask :: ProcInfo -> SignalMask
caughtSignalMask = procc_sigcatch . unProcInfo

{- | Mask of per task pending signals.

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
perTaskPendingSignals :: ProcInfo -> SignalMask
perTaskPendingSignals = procc__sigpnd . unProcInfo

{- | Address of beginning of code segment.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
codeStartAddress :: ProcInfo -> Address
codeStartAddress = procc_start_code . unProcInfo

{- | Address of end of code segment.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
codeEndAddress :: ProcInfo -> Address
codeEndAddress = procc_end_code . unProcInfo

{- | Address of the bottom of stack for the process.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
stackBottomAddress :: ProcInfo -> Address
stackBottomAddress = procc_start_stack . unProcInfo

{- | Kernel stack pointer.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
kernelStackPointer :: ProcInfo -> Address
kernelStackPointer = procc_kstk_esp . unProcInfo

{- | Kernel instruction pointer.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
kernelInstructionPointer :: ProcInfo -> Address
kernelInstructionPointer = procc_kstk_eip . unProcInfo

{- | Address of kernel wait channel proc is sleeping in.

Read when 'System.Proc.Bindings.Tab.Config.flagStat' (special).
-}
kernelWaitChannelAddress :: ProcInfo -> Address
kernelWaitChannelAddress = procc_wchan . unProcInfo

{- | Kernel scheduling priority.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
kernelSchedulingPriority :: ProcInfo -> CLong
kernelSchedulingPriority = procc_priority . unProcInfo

{- | Standard unix nice level of process.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
niceLevel :: ProcInfo -> CLong
niceLevel = procc_nice . unProcInfo

{- | Identical to 'residentNonSwappedMemInPages'.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
rss :: ProcInfo -> CLong
rss = procc_rss . unProcInfo

{- | Not used since Linux 2.6.17.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.

From [proc_pid_stat](https://man7.org/linux/man-pages/man5/proc_pid_stat.5.html):

@
The time in jiffies before the next SIGALRM is sent
to the process due to an interval timer.  Since
Linux 2.6.17, this field is no longer maintained,
and is hard coded as 0.
@
-}
alarm :: ProcInfo -> CLong
alarm = procc_alarm . unProcInfo

{- | Total virtual memory (as # pages).

Read when 'System.Proc.Bindings.Tab.Config.flagMem'.
-}
totalVirtualMemInPages :: ProcInfo -> CLong
totalVirtualMemInPages = procc_size . unProcInfo

{- | Resident non-swapped memory (as # pages).

Read when 'System.Proc.Bindings.Tab.Config.flagMem'.
-}
residentNonSwappedMemInPages :: ProcInfo -> CLong
residentNonSwappedMemInPages = procc_resident . unProcInfo

{- | Shared (mmap'd) memory (as # pages).

Read when 'System.Proc.Bindings.Tab.Config.flagMem'.
-}
sharedMemInPages :: ProcInfo -> CLong
sharedMemInPages = procc_share . unProcInfo

{- | Text (exe) resident set (as # pages).

Read when 'System.Proc.Bindings.Tab.Config.flagMem'.
-}
textResidentSetInPages :: ProcInfo -> CLong
textResidentSetInPages = procc_trs . unProcInfo

{- | Library resident set (always 0 w/ 2.6).

Read when 'System.Proc.Bindings.Tab.Config.flagMem'.
-}
libraryResidentSetInPages :: ProcInfo -> CLong
libraryResidentSetInPages = procc_lrs . unProcInfo

{- | Data+stack resident set (as # pages).

Read when 'System.Proc.Bindings.Tab.Config.flagMem'.
-}
dataAndStackResidentSetInPages :: ProcInfo -> CLong
dataAndStackResidentSetInPages = procc_drs . unProcInfo

{- | Dirty pages (always 0 w/ 2.6).

Read when 'System.Proc.Bindings.Tab.Config.flagMem'.
-}
dirtyPages :: ProcInfo -> CLong
dirtyPages = procc_dt . unProcInfo

{- | Equals 'totalVirtualMemInPages' (as kb).

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
vmSizeInKb :: ProcInfo -> CULong
vmSizeInKb = procc_vm_size . unProcInfo

{- | Locked pages (as kb).

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
vmLockedPagesInKb :: ProcInfo -> CULong
vmLockedPagesInKb = procc_vm_lock . unProcInfo

{- | Equals 'rss' and/or 'residentNonSwappedMemInPages' (as kb).

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
vmRssInKb :: ProcInfo -> CULong
vmRssInKb = procc_vm_rss . unProcInfo

{- | The @anonymous@ portion of vm_rss (as kb).

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
vmRssAnonInKb :: ProcInfo -> CULong
vmRssAnonInKb = procc_vm_rss_anon . unProcInfo

{- | The @file-backed@ portion of vm_rss (as kb).

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
vmRssFileBackedInKb :: ProcInfo -> CULong
vmRssFileBackedInKb = procc_vm_rss_file . unProcInfo

{- | The @shared@ portion of vm_rss (as kb).

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
vmRssSharedInKb :: ProcInfo -> CULong
vmRssSharedInKb = procc_vm_rss_shared . unProcInfo

{- | Data only size (as kb).

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
vmDataSizeInKb :: ProcInfo -> CULong
vmDataSizeInKb = procc_vm_data . unProcInfo

{- | Stack only size (as kb).

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
vmStackSizeInKb :: ProcInfo -> CULong
vmStackSizeInKb = procc_vm_stack . unProcInfo

{- | Based on linux-2.6.34 "swap ents" (as kb).

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
vmSwapSizeInKb :: ProcInfo -> CULong
vmSwapSizeInKb = procc_vm_swap . unProcInfo

{- | Equals 'textResidentSetInPages' (as kb).

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
vmExeInKb :: ProcInfo -> CULong
vmExeInKb = procc_vm_exe . unProcInfo

{- | Total, not just used, library pages (as kb).

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
vmTotalLibraryPagesInKb :: ProcInfo -> CULong
vmTotalLibraryPagesInKb = procc_vm_lib . unProcInfo

{- | Real-time priority.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
realTimePriority :: ProcInfo -> CULong
realTimePriority = procc_rtprio . unProcInfo

{- | Scheduling class.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
schedulingClass :: ProcInfo -> CULong
schedulingClass = procc_sched . unProcInfo

{- | Number of pages of virtual memory.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
virtualMemoryInPages :: ProcInfo -> CULong
virtualMemoryInPages = procc_vsize . unProcInfo

{- | Resident set size limit?

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
residentSetSizeLimit :: ProcInfo -> CULong
residentSetSizeLimit = procc_rss_rlim . unProcInfo

{- | Kernel flags for the process.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
kernelFlags :: ProcInfo -> CULong
kernelFlags = procc_flags . unProcInfo

{- | Number of minor page faults since process start.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
minorPageFaults :: ProcInfo -> CULong
minorPageFaults = procc_min_flt . unProcInfo

{- | Number of major page faults since process start.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
majorPageFaults :: ProcInfo -> CULong
majorPageFaults = procc_maj_flt . unProcInfo

{- | Cumulative min_flt of process and child processes.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
cumulativeMinorPageFaults :: ProcInfo -> CULong
cumulativeMinorPageFaults = procc_cmin_flt . unProcInfo

{- | Cumulative maj_flt of process and child processes.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
cumulativeMajorPageFaults :: ProcInfo -> CULong
cumulativeMajorPageFaults = procc_cmaj_flt . unProcInfo

{- | Environment string vector (/proc/#/environ).

Read when 'System.Proc.Bindings.Tab.Config.flagEnviron'.
-}
environment :: ProcInfo -> [String]
environment = procc_environ . unProcInfo

{- | Command line string vector (/proc/#/cmdline).

Read when 'System.Proc.Bindings.Tab.Config.fillCom'
or 'System.Proc.Bindings.Tab.Config.fillArg'.
-}
cmdline :: ProcInfo -> [String]
cmdline = procc_cmdline . unProcInfo

{- | Cgroup string vector (/proc/#/cgroup).

Read when 'System.Proc.Bindings.Tab.Config.flagCGroup'.
-}
cgroup :: ProcInfo -> [String]
cgroup = procc_cgroup . unProcInfo

{- | Name portion of above (if possible).

Read when 'System.Proc.Bindings.Tab.Config.flagCGroup'
and not 'System.Proc.Bindings.Tab.Config.editCGroupAsSingleVector'.
-}
cgroupName :: ProcInfo -> Maybe String
cgroupName = procc_cgname . unProcInfo

{- | Supplementary gids as comma delimited str.

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
supplementaryGids :: ProcInfo -> Maybe String
supplementaryGids = procc_supgid . unProcInfo

-- | Supp grp names as comma delimited str, derived from supgid.
supplementaryGroupNames :: ProcInfo -> Maybe String
supplementaryGroupNames = procc_supgrp . unProcInfo

{- | Effective user name.

Read when 'System.Proc.Bindings.Tab.Config.flagStat' or
'System.Proc.Bindings.Tab.Config.flagStatus'.
-}
effectiveUserName :: ProcInfo -> Maybe String
effectiveUserName = procc_euser . unProcInfo

{- | Real user name.

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
realUserName :: ProcInfo -> Maybe String
realUserName = procc_ruser . unProcInfo

{- | Saved user name.

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
savedUserName :: ProcInfo -> Maybe String
savedUserName = procc_suser . unProcInfo

{- | Filesystem user name.

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
filesystemUserName :: ProcInfo -> Maybe String
filesystemUserName = procc_fuser . unProcInfo

{- | Real group name.

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
realGroupName :: ProcInfo -> Maybe String
realGroupName = procc_rgroup . unProcInfo

{- | Effective group name.

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
effectiveGroupname :: ProcInfo -> Maybe String
effectiveGroupname = procc_egroup . unProcInfo

{- | Saved group name.

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
savedGroupName :: ProcInfo -> Maybe String
savedGroupName = procc_sgroup . unProcInfo

{- | Filesystem group name.

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
filesystemGroupName :: ProcInfo -> Maybe String
filesystemGroupName = procc_fgroup . unProcInfo

{- | Basename of executable file in call to exec(2).

Read when 'System.Proc.Bindings.Tab.Config.flagStat' or
'System.Proc.Bindings.Tab.Config.flagStatus'.
-}
cmd :: ProcInfo -> Maybe String
cmd = procc_cmd . unProcInfo

{-
procc_ring :: ProcInfo -> ()
procc_ring = procc_ring . unProcInfo

procc_next :: ProcInfo -> () -- struct proc_t *
procc_next = procc_next . unProcInfo
-}

{- | Process group ID.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
processGroupId :: ProcInfo -> CInt
processGroupId = procc_pgrp . unProcInfo

{- | Session ID.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
sessionId :: ProcInfo -> CInt
sessionId = procc_session . unProcInfo

{- | Number of threads, or 0 if no clue.

Read when 'System.Proc.Bindings.Tab.Config.flagStat' or
'System.Proc.Bindings.Tab.Config.flagStatus'.
-}
numberOfThreasds :: ProcInfo -> CInt
numberOfThreasds = procc_nlwp . unProcInfo

{- | Thread group ID, the POSIX PID (see also: 'taskId').

Always read.
-}
threadGroupId :: ProcInfo -> CInt
threadGroupId = procc_tgid . unProcInfo

{- | Full device number of controlling terminal.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
ttyNumber :: ProcInfo -> CInt
ttyNumber = procc_tty . unProcInfo

{- | Effective user ID.

Read when 'System.Proc.Bindings.Tab.Config.flagStat' or
'System.Proc.Bindings.Tab.Config.flagStatus'.
-}
effectiveUserId :: ProcInfo -> CInt
effectiveUserId = procc_euid . unProcInfo

{- | Effective group ID.

Read when 'System.Proc.Bindings.Tab.Config.flagStat' or
'System.Proc.Bindings.Tab.Config.flagStatus'.
-}
effectiveGroupId :: ProcInfo -> CInt
effectiveGroupId = procc_egid . unProcInfo

{- | Real user ID.

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
realUserId :: ProcInfo -> CInt
realUserId = procc_ruid . unProcInfo

{- | Real group ID.

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
realGroupId :: ProcInfo -> CInt
realGroupId = procc_rgid . unProcInfo

{- | Saved user ID.

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
savedUserId :: ProcInfo -> CInt
savedUserId = procc_suid . unProcInfo

{- | Saved group ID.

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
savedGroupId :: ProcInfo -> CInt
savedGroupId = procc_sgid . unProcInfo

{- | Fs user ID (used for file access only).

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
filesystemUserId :: ProcInfo -> CInt
filesystemUserId = procc_fuid . unProcInfo

{- | Fs group ID (used for file access only).

Read when 'System.Proc.Bindings.Tab.Config.fillStatus'.
-}
filesystemGroupId :: ProcInfo -> CInt
filesystemGroupId = procc_fgid . unProcInfo

{- | Terminal process group ID.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
terminalProcessGroupId :: ProcInfo -> CInt
terminalProcessGroupId = procc_tpgid . unProcInfo

{- | Might not be SIGCHLD.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
exitSignal :: ProcInfo -> CInt
exitSignal = procc_exit_signal . unProcInfo

{- | Current (or most recent?) CPU.

Read when 'System.Proc.Bindings.Tab.Config.flagStat'.
-}
cpu :: ProcInfo -> CInt
cpu = procc_processor . unProcInfo

{- | (badness for OOM killer).

Read when 'System.Proc.Bindings.Tab.Config.flagOOM'.
-}
oomScore :: ProcInfo -> CInt
oomScore = procc_oom_score . unProcInfo

{- | (adjustment to OOM score).

Read when 'System.Proc.Bindings.Tab.Config.flagOOM'.
-}
oomAdjustment :: ProcInfo -> CInt
oomAdjustment = procc_oom_adj . unProcInfo

{- | Inode number of namespaces.

Read when 'System.Proc.Bindings.Tab.Config.flagNS'.
-}
namespaces :: ProcInfo -> [CLong]
namespaces = procc_ns . unProcInfo

-- | Systemd vm/container name.
systemdContainerName :: ProcInfo -> Maybe String
systemdContainerName = procc_sd_mach . unProcInfo

-- | Systemd session owner uid.
systemdSessionOwnerUid :: ProcInfo -> Maybe String
systemdSessionOwnerUid = procc_sd_ouid . unProcInfo

-- | Systemd login session seat.
systemdLoginSessionSeat :: ProcInfo -> Maybe String
systemdLoginSessionSeat = procc_sd_seat . unProcInfo

-- | Systemd login session ID.
systemdLoginSessionId :: ProcInfo -> Maybe String
systemdLoginSessionId = procc_sd_sess . unProcInfo

-- | Systemd slice unit.
systemdSliceUnit :: ProcInfo -> Maybe String
systemdSliceUnit = procc_sd_slice . unProcInfo

-- | Systemd system unit ID.
systemdSystemUnitId :: ProcInfo -> Maybe String
systemdSystemUnitId = procc_sd_unit . unProcInfo

-- | Systemd user unit ID.
systemdUserUnitId :: ProcInfo -> Maybe String
systemdUserUnitId = procc_sd_uunit . unProcInfo

-- | Lxc container name.
lxcContainerName :: ProcInfo -> Maybe String
lxcContainerName = procc_lxcname . unProcInfo
