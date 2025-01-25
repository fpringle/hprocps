module System.Proc.Bindings.Internal where

import Control.Exception
import Foreign
import Foreign.C.Types
import GHC.Show
import System.Proc.Bindings.C
import System.Proc.Bindings.Error

{- | A handle to information about a process, parsed from its @\/proc\/#\/@ directory.

__NOTE__ this is not a "live" handle. The underlying C struct (@proc_t@) is created,
populated with information, and returned to the user. Reading process information with
'System.Proc.Bindings.procInfo' will give you the information about the process
/at the time the 'Proc' was created/, __not__ the information at the time you call
'System.Proc.Bindings.procInfo'. That is to say, successive calls to
'System.Proc.Bindings.procInfo' should give identical results.

Internally this is just a 'Ptr'. The library is designed so that it should be impossible
to get hold of a 'Proc' that contains a 'nullPtr': functions like
'System.Proc.Bindings.readNextProc', 'System.Proc.Bindings.openAllProcs' all check to
make sure this doesn't happen.

Nevertheless, this doesn't guarantee total type safety: Once the internal pointer has been
freed (either by 'System.Proc.Bindings.closeProc' or by the procps C library itself),
there's nothing stopping you from trying to use it again via 'System.Proc.Bindings.procInfo'
etc, which will cause undefined behaviour.
Don't do that.

For better type-safety, use the @hprocps@ library, which forbids this kind of bug using
monadic regions.
-}
newtype Proc = UnsafeProc (Ptr ProcC)
  deriving (Eq)

makeProc :: Ptr ProcC -> Either ProcError Proc
makeProc procPtr
  | procPtr == nullPtr = Left NullPtrError
  | otherwise = Right $ UnsafeProc procPtr

fromProcC :: Ptr ProcC -> IO ProcInfo
fromProcC ptr =
  ProcInfo <$> readProcCInfo ptr

withProcPtr :: IO (Ptr ProcC) -> (Ptr ProcC -> IO a) -> IO (Either ProcError a)
withProcPtr open f =
  bracket open freeProc $ \ptr ->
    if ptr == nullPtr
      then pure $ Left NullPtrError
      else Right <$> f ptr

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

taskId :: ProcInfo -> CInt
taskId = procc_tid . unProcInfo

parentPid :: ProcInfo -> CInt
parentPid = procc_ppid . unProcInfo

majDelta :: ProcInfo -> CULong
majDelta = procc_maj_delta . unProcInfo

minDelta :: ProcInfo -> CULong
minDelta = procc_min_delta . unProcInfo

cpuUsagePercent :: ProcInfo -> CUInt
cpuUsagePercent = procc_pcpu . unProcInfo

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

userModeCPUTime :: ProcInfo -> CULLong
userModeCPUTime = procc_utime . unProcInfo

kernelModeCPUTime :: ProcInfo -> CULLong
kernelModeCPUTime = procc_stime . unProcInfo

cumulativeUserModeCPUTime :: ProcInfo -> CULLong
cumulativeUserModeCPUTime = procc_cutime . unProcInfo

cumulativeKernelModeCPUTime :: ProcInfo -> CULLong
cumulativeKernelModeCPUTime = procc_cstime . unProcInfo

startTimeInSeconds :: ProcInfo -> CULLong
startTimeInSeconds = procc_start_time . unProcInfo

pendingSignalMask :: ProcInfo -> SignalMask
pendingSignalMask = procc_signal . unProcInfo

blockSignalMask :: ProcInfo -> SignalMask
blockSignalMask = procc_blocked . unProcInfo

ignoredSignalMask :: ProcInfo -> SignalMask
ignoredSignalMask = procc_sigignore . unProcInfo

caughtSignalMask :: ProcInfo -> SignalMask
caughtSignalMask = procc_sigcatch . unProcInfo

perTaskPendingSignals :: ProcInfo -> SignalMask
perTaskPendingSignals = procc__sigpnd . unProcInfo

codeStartAddress :: ProcInfo -> Address
codeStartAddress = procc_start_code . unProcInfo

codeEndAddress :: ProcInfo -> Address
codeEndAddress = procc_end_code . unProcInfo

stackBottomAddress :: ProcInfo -> Address
stackBottomAddress = procc_start_stack . unProcInfo

kernelStackPointer :: ProcInfo -> Address
kernelStackPointer = procc_kstk_esp . unProcInfo

kernelInstructionPointer :: ProcInfo -> Address
kernelInstructionPointer = procc_kstk_eip . unProcInfo

kernelWaitChannelAddress :: ProcInfo -> Address
kernelWaitChannelAddress = procc_wchan . unProcInfo

kernelSchedulingPriority :: ProcInfo -> CLong
kernelSchedulingPriority = procc_priority . unProcInfo

niceLevel :: ProcInfo -> CLong
niceLevel = procc_nice . unProcInfo

rss :: ProcInfo -> CLong
rss = procc_rss . unProcInfo

alarm :: ProcInfo -> CLong
alarm = procc_alarm . unProcInfo

totalVirtualMemInPages :: ProcInfo -> CLong
totalVirtualMemInPages = procc_size . unProcInfo

residentNonSwappedMemInPages :: ProcInfo -> CLong
residentNonSwappedMemInPages = procc_resident . unProcInfo

sharedMemInPages :: ProcInfo -> CLong
sharedMemInPages = procc_share . unProcInfo

textResidentSetInPages :: ProcInfo -> CLong
textResidentSetInPages = procc_trs . unProcInfo

libraryResidentSetInPages :: ProcInfo -> CLong
libraryResidentSetInPages = procc_lrs . unProcInfo

dataAndStackResidentSetInPages :: ProcInfo -> CLong
dataAndStackResidentSetInPages = procc_drs . unProcInfo

dirtyPages :: ProcInfo -> CLong
dirtyPages = procc_dt . unProcInfo

vmSizeInKb :: ProcInfo -> CULong
vmSizeInKb = procc_vm_size . unProcInfo

vmLockedPagesInKb :: ProcInfo -> CULong
vmLockedPagesInKb = procc_vm_lock . unProcInfo

vmRssInKb :: ProcInfo -> CULong
vmRssInKb = procc_vm_rss . unProcInfo

vmRssAnonInKb :: ProcInfo -> CULong
vmRssAnonInKb = procc_vm_rss_anon . unProcInfo

vmRssFileBackedInKb :: ProcInfo -> CULong
vmRssFileBackedInKb = procc_vm_rss_file . unProcInfo

vmRssSharedInKb :: ProcInfo -> CULong
vmRssSharedInKb = procc_vm_rss_shared . unProcInfo

vmDataSizeInKb :: ProcInfo -> CULong
vmDataSizeInKb = procc_vm_data . unProcInfo

vmStackSizeInKb :: ProcInfo -> CULong
vmStackSizeInKb = procc_vm_stack . unProcInfo

vmSwapSizeInKb :: ProcInfo -> CULong
vmSwapSizeInKb = procc_vm_swap . unProcInfo

vmExeInKb :: ProcInfo -> CULong
vmExeInKb = procc_vm_exe . unProcInfo

vmTotalLibraryPagesInKb :: ProcInfo -> CULong
vmTotalLibraryPagesInKb = procc_vm_lib . unProcInfo

realTimePriority :: ProcInfo -> CULong
realTimePriority = procc_rtprio . unProcInfo

schedulingClass :: ProcInfo -> CULong
schedulingClass = procc_sched . unProcInfo

virtualMemoryInPages :: ProcInfo -> CULong
virtualMemoryInPages = procc_vsize . unProcInfo

residentSetSizeLimit :: ProcInfo -> CULong
residentSetSizeLimit = procc_rss_rlim . unProcInfo

kernelFlags :: ProcInfo -> CULong
kernelFlags = procc_flags . unProcInfo

minorPageFaults :: ProcInfo -> CULong
minorPageFaults = procc_min_flt . unProcInfo

majorPageFaults :: ProcInfo -> CULong
majorPageFaults = procc_maj_flt . unProcInfo

cumulativeMinorPageFaults :: ProcInfo -> CULong
cumulativeMinorPageFaults = procc_cmin_flt . unProcInfo

cumulativeMajorPageFaults :: ProcInfo -> CULong
cumulativeMajorPageFaults = procc_cmaj_flt . unProcInfo

environment :: ProcInfo -> [String]
environment = procc_environ . unProcInfo

cmdline :: ProcInfo -> [String]
cmdline = procc_cmdline . unProcInfo

cgroup :: ProcInfo -> [String]
cgroup = procc_cgroup . unProcInfo

cgroupName :: ProcInfo -> Maybe String
cgroupName = procc_cgname . unProcInfo

supplementaryGids :: ProcInfo -> Maybe String
supplementaryGids = procc_supgid . unProcInfo

supplementaryGroupNames :: ProcInfo -> Maybe String
supplementaryGroupNames = procc_supgrp . unProcInfo

effectiveUserName :: ProcInfo -> Maybe String
effectiveUserName = procc_euser . unProcInfo

realUserName :: ProcInfo -> Maybe String
realUserName = procc_ruser . unProcInfo

savedUserName :: ProcInfo -> Maybe String
savedUserName = procc_suser . unProcInfo

filesystemUserName :: ProcInfo -> Maybe String
filesystemUserName = procc_fuser . unProcInfo

realGroupName :: ProcInfo -> Maybe String
realGroupName = procc_rgroup . unProcInfo

effectiveGroupname :: ProcInfo -> Maybe String
effectiveGroupname = procc_egroup . unProcInfo

savedGroupName :: ProcInfo -> Maybe String
savedGroupName = procc_sgroup . unProcInfo

filesystemGroupName :: ProcInfo -> Maybe String
filesystemGroupName = procc_fgroup . unProcInfo

cmd :: ProcInfo -> Maybe String
cmd = procc_cmd . unProcInfo

{-
procc_ring :: ProcInfo -> ()
procc_ring = procc_ring . unProcInfo

procc_next :: ProcInfo -> () -- struct proc_t *
procc_next = procc_next . unProcInfo
-}

processGroupId :: ProcInfo -> CInt
processGroupId = procc_pgrp . unProcInfo

sessionId :: ProcInfo -> CInt
sessionId = procc_session . unProcInfo

numberOfThreasds :: ProcInfo -> CInt
numberOfThreasds = procc_nlwp . unProcInfo

threadGroupId :: ProcInfo -> CInt
threadGroupId = procc_tgid . unProcInfo

ttyNumber :: ProcInfo -> CInt
ttyNumber = procc_tty . unProcInfo

effectiveUserId :: ProcInfo -> CInt
effectiveUserId = procc_euid . unProcInfo

effectiveGroupId :: ProcInfo -> CInt
effectiveGroupId = procc_egid . unProcInfo

realUserId :: ProcInfo -> CInt
realUserId = procc_ruid . unProcInfo

realGroupId :: ProcInfo -> CInt
realGroupId = procc_rgid . unProcInfo

savedUserId :: ProcInfo -> CInt
savedUserId = procc_suid . unProcInfo

savedGroupId :: ProcInfo -> CInt
savedGroupId = procc_sgid . unProcInfo

filesystemUserId :: ProcInfo -> CInt
filesystemUserId = procc_fuid . unProcInfo

filesystemGroupId :: ProcInfo -> CInt
filesystemGroupId = procc_fgid . unProcInfo

terminalProcessGroupId :: ProcInfo -> CInt
terminalProcessGroupId = procc_tpgid . unProcInfo

exitSignal :: ProcInfo -> CInt
exitSignal = procc_exit_signal . unProcInfo

cpu :: ProcInfo -> CInt
cpu = procc_processor . unProcInfo

oomScore :: ProcInfo -> CInt
oomScore = procc_oom_score . unProcInfo

oomAdjustment :: ProcInfo -> CInt
oomAdjustment = procc_oom_adj . unProcInfo

namespaces :: ProcInfo -> [CLong]
namespaces = procc_ns . unProcInfo

systemdContainerName :: ProcInfo -> Maybe String
systemdContainerName = procc_sd_mach . unProcInfo

systemdSessionOwnerUid :: ProcInfo -> Maybe String
systemdSessionOwnerUid = procc_sd_ouid . unProcInfo

systemdLoginSessionSeat :: ProcInfo -> Maybe String
systemdLoginSessionSeat = procc_sd_seat . unProcInfo

systemdLoginSessionId :: ProcInfo -> Maybe String
systemdLoginSessionId = procc_sd_sess . unProcInfo

systemdSliceUnit :: ProcInfo -> Maybe String
systemdSliceUnit = procc_sd_slice . unProcInfo

systemdSystemUnitId :: ProcInfo -> Maybe String
systemdSystemUnitId = procc_sd_unit . unProcInfo

systemdUserUnitId :: ProcInfo -> Maybe String
systemdUserUnitId = procc_sd_uunit . unProcInfo

lxcContainerName :: ProcInfo -> Maybe String
lxcContainerName = procc_lxcname . unProcInfo
