{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
-- {-# OPTIONS_GHC -fplugin Debug.Breakpoint #-}

#include "proc-handle.h"

module System.Proc.Bindings.C
  ( Proc' (..)
  , ProcC
  , ProcInfo'
  , SignalMask
  , Address
  , freeProc
  , readProcCInfo
  , readNextProcC
  , readAllProcsSimple
  , readAllProcsPids
  , readAllProcsUids
  , readSelfProc
  )
where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.Posix.Types
import System.Proc.Bindings.Tab.C

#if defined(k64test) || (defined(_ABIN32) && _MIPS_SIM == _ABIN32)
type Address = CULLong
#else
type Address = CULong
#endif

#ifdef SIGNAL_STRING
type SignalMask = Maybe String
#else
type SignalMask = CLLong
#endif

data Proc' string charArray stringList longList procPtr = Proc
  { procc_tid :: CInt 
  , procc_ppid :: CInt 
  , procc_maj_delta :: CULong 
  , procc_min_delta :: CULong 
  , procc_pcpu :: CUInt 
  , procc_state :: CChar 
  , procc_pad_1 :: CChar 
  , procc_pad_2 :: CChar 
  , procc_pad_3 :: CChar 
  , procc_utime :: CULLong 
  , procc_stime :: CULLong 
  , procc_cutime :: CULLong 
  , procc_cstime :: CULLong 
  , procc_start_time :: CULLong 
#ifdef SIGNAL_STRING
  , procc_signal :: charArray
  , procc_blocked :: charArray
  , procc_sigignore :: charArray
  , procc_sigcatch :: charArray
  , procc__sigpnd :: charArray
#else
  , procc_signal :: CLLong 
  , procc_blocked :: CLLong 
  , procc_sigignore :: CLLong 
  , procc_sigcatch :: CLLong 
  , procc__sigpnd :: CLLong 
#endif
  , procc_start_code :: Address 
  , procc_end_code :: Address 
  , procc_start_stack :: Address 
  , procc_kstk_esp :: Address 
  , procc_kstk_eip :: Address 
  , procc_wchan :: Address 
  , procc_priority :: CLong 
  , procc_nice :: CLong 
  , procc_rss :: CLong 
  , procc_alarm :: CLong 
  , procc_size :: CLong 
  , procc_resident :: CLong 
  , procc_share :: CLong 
  , procc_trs :: CLong 
  , procc_lrs :: CLong 
  , procc_drs :: CLong 
  , procc_dt :: CLong 
  , procc_vm_size :: CULong 
  , procc_vm_lock :: CULong 
  , procc_vm_rss :: CULong 
  , procc_vm_rss_anon :: CULong 
  , procc_vm_rss_file :: CULong 
  , procc_vm_rss_shared :: CULong 
  , procc_vm_data :: CULong 
  , procc_vm_stack :: CULong 
  , procc_vm_swap :: CULong 
  , procc_vm_exe :: CULong 
  , procc_vm_lib :: CULong 
  , procc_rtprio :: CULong 
  , procc_sched :: CULong 
  , procc_vsize :: CULong 
  , procc_rss_rlim :: CULong 
  , procc_flags :: CULong 
  , procc_min_flt :: CULong 
  , procc_maj_flt :: CULong 
  , procc_cmin_flt :: CULong 
  , procc_cmaj_flt :: CULong 
  , procc_environ :: stringList
  , procc_cmdline :: stringList
  , procc_cgroup :: stringList
  , procc_cgname :: string
  , procc_supgid :: string
  , procc_supgrp :: string
  , procc_euser :: charArray
  , procc_ruser :: charArray
  , procc_suser :: charArray
  , procc_fuser :: charArray
  , procc_rgroup :: charArray
  , procc_egroup :: charArray
  , procc_sgroup :: charArray
  , procc_fgroup :: charArray
  , procc_cmd :: charArray
  , procc_ring :: procPtr -- struct proc_t *
  , procc_next :: procPtr -- struct proc_t *
  , procc_pgrp :: CInt 
  , procc_session :: CInt 
  , procc_nlwp :: CInt 
  , procc_tgid :: CInt 
  , procc_tty :: CInt 
  , procc_euid :: CInt 
  , procc_egid :: CInt 
  , procc_ruid :: CInt 
  , procc_rgid :: CInt 
  , procc_suid :: CInt 
  , procc_sgid :: CInt 
  , procc_fuid :: CInt 
  , procc_fgid :: CInt 
  , procc_tpgid :: CInt 
  , procc_exit_signal :: CInt 
  , procc_processor :: CInt 
  , procc_oom_score :: CInt 
  , procc_oom_adj :: CInt 
  , procc_ns :: longList
  , procc_sd_mach :: string
  , procc_sd_ouid :: string
  , procc_sd_seat :: string
  , procc_sd_sess :: string
  , procc_sd_slice :: string
  , procc_sd_unit :: string
  , procc_sd_uunit :: string
  , procc_lxcname :: string
  }

type ProcC = Proc' CString CString (Ptr CString) (Ptr CLong) (Ptr ())

type ProcInfo' = Proc' (Maybe String) (Maybe String) [String] [CLong] ()

readStringMaybe :: CString -> IO (Maybe String)
readStringMaybe cStr
  | cStr == nullPtr = pure Nothing
  | otherwise = Just <$> peekCString cStr

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = go
  where
    go [] = pure []
    go (a:as) =
      f a >>= \case
        Nothing -> go as
        Just b -> (b:) <$> go as

readStringList :: Ptr CString -> IO [String]
readStringList ptr
  | ptr == nullPtr = pure []
  | otherwise = peekArray0 nullPtr ptr >>= mapMaybeM readStringMaybe

readLongList :: Ptr CLong -> IO [CLong]
readLongList = readNullTermArray

readProcCInfo :: Ptr ProcC -> IO ProcInfo'
readProcCInfo pcPtr = do
  pc <- peek pcPtr
  new_cgname <- readStringMaybe $ procc_cgname pc
  new_supgid <- readStringMaybe $ procc_supgid pc
  new_supgrp <- readStringMaybe $ procc_supgrp pc
  new_euser <- readStringMaybe $ #{ptr proc_t, euser} pcPtr
  new_ruser <- readStringMaybe $ #{ptr proc_t, ruser} pcPtr
  new_suser <- readStringMaybe $ #{ptr proc_t, suser} pcPtr
  new_fuser <- readStringMaybe $ #{ptr proc_t, fuser} pcPtr
  new_rgroup <- readStringMaybe $ #{ptr proc_t, rgroup} pcPtr
  new_egroup <- readStringMaybe $ #{ptr proc_t, egroup} pcPtr
  new_sgroup <- readStringMaybe $ #{ptr proc_t, sgroup} pcPtr
  new_fgroup <- readStringMaybe $ #{ptr proc_t, fgroup} pcPtr
  new_cmd <- readStringMaybe $ #{ptr proc_t, cmd} pcPtr
  new_sd_mach <- readStringMaybe $ procc_sd_mach pc
  new_sd_ouid <- readStringMaybe $ procc_sd_ouid pc
  new_sd_seat <- readStringMaybe $ procc_sd_seat pc
  new_sd_sess <- readStringMaybe $ procc_sd_sess pc
  new_sd_slice <- readStringMaybe $ procc_sd_slice pc
  new_sd_unit <- readStringMaybe $ procc_sd_unit pc
  new_sd_uunit <- readStringMaybe $ procc_sd_uunit pc
  new_lxcname <- readStringMaybe $ procc_lxcname pc

  new_environ <- readStringList $ procc_environ pc
  new_cmdline <- readStringList $ procc_cmdline pc
  new_cgroup <- readStringList $ procc_cgroup pc

  new_ns <- readLongList $ #{ptr proc_t, ns} pcPtr

#ifdef SIGNAL_STRING
  new_signal <- readStringMaybe $ #{ptr proc_t, signal} pcPtr
  new_blocked <- readStringMaybe $ #{ptr proc_t, blocked} pcPtr
  new_sigignore <- readStringMaybe $ #{ptr proc_t, sigignore} pcPtr
  new_sigcatch <- readStringMaybe $ #{ptr proc_t, sigcatch} pcPtr
  new__sigpnd <- readStringMaybe $ #{ptr proc_t, _sigpnd} pcPtr
#endif

  let new_pc :: ProcInfo' =
        pc
          { procc_ring = ()
          , procc_next = ()
          , procc_cgname = new_cgname
          , procc_supgid = new_supgid
          , procc_supgrp = new_supgrp
          , procc_euser = new_euser
          , procc_ruser = new_ruser
          , procc_suser = new_suser
          , procc_fuser = new_fuser
          , procc_rgroup = new_rgroup
          , procc_egroup = new_egroup
          , procc_sgroup = new_sgroup
          , procc_fgroup = new_fgroup
          , procc_cmd = new_cmd
          , procc_sd_mach = new_sd_mach
          , procc_sd_ouid = new_sd_ouid
          , procc_sd_seat = new_sd_seat
          , procc_sd_sess = new_sd_sess
          , procc_sd_slice = new_sd_slice
          , procc_sd_unit = new_sd_unit
          , procc_sd_uunit = new_sd_uunit
          , procc_lxcname = new_lxcname

          , procc_environ = new_environ
          , procc_cmdline = new_cmdline
          , procc_cgroup = new_cgroup

          , procc_ns = new_ns

#ifdef SIGNAL_STRING
          , procc_signal = new_signal
          , procc_blocked = new_blocked
          , procc_sigignore = new_sigignore
          , procc_sigcatch = new_sigcatch
          , procc__sigpnd = new__sigpnd
#endif
          }

  pure new_pc

instance Storable ProcC where
  alignment _ = #alignment proc_t
  sizeOf _ = #size proc_t

  poke ptr (Proc {..}) = do
    (#poke proc_t, tid) ptr procc_tid
    (#poke proc_t, ppid) ptr procc_ppid
    (#poke proc_t, maj_delta) ptr procc_maj_delta
    (#poke proc_t, min_delta) ptr procc_min_delta
    (#poke proc_t, pcpu) ptr procc_pcpu
    (#poke proc_t, state) ptr procc_state
    (#poke proc_t, pad_1) ptr procc_pad_1
    (#poke proc_t, pad_2) ptr procc_pad_2
    (#poke proc_t, pad_3) ptr procc_pad_3
    (#poke proc_t, utime) ptr procc_utime
    (#poke proc_t, stime) ptr procc_stime
    (#poke proc_t, cutime) ptr procc_cutime
    (#poke proc_t, cstime) ptr procc_cstime
    (#poke proc_t, start_time) ptr procc_start_time
    (#poke proc_t, signal) ptr procc_signal
    (#poke proc_t, blocked) ptr procc_blocked
    (#poke proc_t, sigignore) ptr procc_sigignore
    (#poke proc_t, sigcatch) ptr procc_sigcatch
    (#poke proc_t, _sigpnd) ptr procc__sigpnd
    (#poke proc_t, start_code) ptr procc_start_code
    (#poke proc_t, end_code) ptr procc_end_code
    (#poke proc_t, start_stack) ptr procc_start_stack
    (#poke proc_t, kstk_esp) ptr procc_kstk_esp
    (#poke proc_t, kstk_eip) ptr procc_kstk_eip
    (#poke proc_t, wchan) ptr procc_wchan
    (#poke proc_t, priority) ptr procc_priority
    (#poke proc_t, nice) ptr procc_nice
    (#poke proc_t, rss) ptr procc_rss
    (#poke proc_t, alarm) ptr procc_alarm
    (#poke proc_t, size) ptr procc_size
    (#poke proc_t, resident) ptr procc_resident
    (#poke proc_t, share) ptr procc_share
    (#poke proc_t, trs) ptr procc_trs
    (#poke proc_t, lrs) ptr procc_lrs
    (#poke proc_t, drs) ptr procc_drs
    (#poke proc_t, dt) ptr procc_dt
    (#poke proc_t, vm_size) ptr procc_vm_size
    (#poke proc_t, vm_lock) ptr procc_vm_lock
    (#poke proc_t, vm_rss) ptr procc_vm_rss
    (#poke proc_t, vm_rss_anon) ptr procc_vm_rss_anon
    (#poke proc_t, vm_rss_file) ptr procc_vm_rss_file
    (#poke proc_t, vm_rss_shared) ptr procc_vm_rss_shared
    (#poke proc_t, vm_data) ptr procc_vm_data
    (#poke proc_t, vm_stack) ptr procc_vm_stack
    (#poke proc_t, vm_swap) ptr procc_vm_swap
    (#poke proc_t, vm_exe) ptr procc_vm_exe
    (#poke proc_t, vm_lib) ptr procc_vm_lib
    (#poke proc_t, rtprio) ptr procc_rtprio
    (#poke proc_t, sched) ptr procc_sched
    (#poke proc_t, vsize) ptr procc_vsize
    (#poke proc_t, rss_rlim) ptr procc_rss_rlim
    (#poke proc_t, flags) ptr procc_flags
    (#poke proc_t, min_flt) ptr procc_min_flt
    (#poke proc_t, maj_flt) ptr procc_maj_flt
    (#poke proc_t, cmin_flt) ptr procc_cmin_flt
    (#poke proc_t, cmaj_flt) ptr procc_cmaj_flt
    (#poke proc_t, environ) ptr procc_environ
    (#poke proc_t, cmdline) ptr procc_cmdline
    (#poke proc_t, cgroup) ptr procc_cgroup
    (#poke proc_t, cgname) ptr procc_cgname
    (#poke proc_t, supgid) ptr procc_supgid
    (#poke proc_t, supgrp) ptr procc_supgrp
    (#poke proc_t, euser) ptr procc_euser
    (#poke proc_t, ruser) ptr procc_ruser
    (#poke proc_t, suser) ptr procc_suser
    (#poke proc_t, fuser) ptr procc_fuser
    (#poke proc_t, rgroup) ptr procc_rgroup
    (#poke proc_t, egroup) ptr procc_egroup
    (#poke proc_t, sgroup) ptr procc_sgroup
    (#poke proc_t, fgroup) ptr procc_fgroup
    (#poke proc_t, cmd) ptr procc_cmd
    (#poke proc_t, ring) ptr procc_ring
    (#poke proc_t, next) ptr procc_next
    (#poke proc_t, pgrp) ptr procc_pgrp
    (#poke proc_t, session) ptr procc_session
    (#poke proc_t, nlwp) ptr procc_nlwp
    (#poke proc_t, tgid) ptr procc_tgid
    (#poke proc_t, tty) ptr procc_tty
    (#poke proc_t, euid) ptr procc_euid
    (#poke proc_t, egid) ptr procc_egid
    (#poke proc_t, ruid) ptr procc_ruid
    (#poke proc_t, rgid) ptr procc_rgid
    (#poke proc_t, suid) ptr procc_suid
    (#poke proc_t, sgid) ptr procc_sgid
    (#poke proc_t, fuid) ptr procc_fuid
    (#poke proc_t, fgid) ptr procc_fgid
    (#poke proc_t, tpgid) ptr procc_tpgid
    (#poke proc_t, exit_signal) ptr procc_exit_signal
    (#poke proc_t, processor) ptr procc_processor
    (#poke proc_t, oom_score) ptr procc_oom_score
    (#poke proc_t, oom_adj) ptr procc_oom_adj
    (#poke proc_t, ns) ptr procc_ns
    (#poke proc_t, sd_mach) ptr procc_sd_mach
    (#poke proc_t, sd_ouid) ptr procc_sd_ouid
    (#poke proc_t, sd_seat) ptr procc_sd_seat
    (#poke proc_t, sd_sess) ptr procc_sd_sess
    (#poke proc_t, sd_slice) ptr procc_sd_slice
    (#poke proc_t, sd_unit) ptr procc_sd_unit
    (#poke proc_t, sd_uunit) ptr procc_sd_uunit
    (#poke proc_t, lxcname) ptr procc_lxcname

  peek ptr =
    Proc
      <$> (#peek proc_t, tid) ptr
      <*> (#peek proc_t, ppid) ptr
      <*> (#peek proc_t, maj_delta) ptr
      <*> (#peek proc_t, min_delta) ptr
      <*> (#peek proc_t, pcpu) ptr
      <*> (#peek proc_t, state) ptr
      <*> (#peek proc_t, pad_1) ptr
      <*> (#peek proc_t, pad_2) ptr
      <*> (#peek proc_t, pad_3) ptr
      <*> (#peek proc_t, utime) ptr
      <*> (#peek proc_t, stime) ptr
      <*> (#peek proc_t, cutime) ptr
      <*> (#peek proc_t, cstime) ptr
      <*> (#peek proc_t, start_time) ptr
      <*> (#peek proc_t, signal) ptr
      <*> (#peek proc_t, blocked) ptr
      <*> (#peek proc_t, sigignore) ptr
      <*> (#peek proc_t, sigcatch) ptr
      <*> (#peek proc_t, _sigpnd) ptr
      <*> (#peek proc_t, start_code) ptr
      <*> (#peek proc_t, end_code) ptr
      <*> (#peek proc_t, start_stack) ptr
      <*> (#peek proc_t, kstk_esp) ptr
      <*> (#peek proc_t, kstk_eip) ptr
      <*> (#peek proc_t, wchan) ptr
      <*> (#peek proc_t, priority) ptr
      <*> (#peek proc_t, nice) ptr
      <*> (#peek proc_t, rss) ptr
      <*> (#peek proc_t, alarm) ptr
      <*> (#peek proc_t, size) ptr
      <*> (#peek proc_t, resident) ptr
      <*> (#peek proc_t, share) ptr
      <*> (#peek proc_t, trs) ptr
      <*> (#peek proc_t, lrs) ptr
      <*> (#peek proc_t, drs) ptr
      <*> (#peek proc_t, dt) ptr
      <*> (#peek proc_t, vm_size) ptr
      <*> (#peek proc_t, vm_lock) ptr
      <*> (#peek proc_t, vm_rss) ptr
      <*> (#peek proc_t, vm_rss_anon) ptr
      <*> (#peek proc_t, vm_rss_file) ptr
      <*> (#peek proc_t, vm_rss_shared) ptr
      <*> (#peek proc_t, vm_data) ptr
      <*> (#peek proc_t, vm_stack) ptr
      <*> (#peek proc_t, vm_swap) ptr
      <*> (#peek proc_t, vm_exe) ptr
      <*> (#peek proc_t, vm_lib) ptr
      <*> (#peek proc_t, rtprio) ptr
      <*> (#peek proc_t, sched) ptr
      <*> (#peek proc_t, vsize) ptr
      <*> (#peek proc_t, rss_rlim) ptr
      <*> (#peek proc_t, flags) ptr
      <*> (#peek proc_t, min_flt) ptr
      <*> (#peek proc_t, maj_flt) ptr
      <*> (#peek proc_t, cmin_flt) ptr
      <*> (#peek proc_t, cmaj_flt) ptr
      <*> (#peek proc_t, environ) ptr
      <*> (#peek proc_t, cmdline) ptr
      <*> (#peek proc_t, cgroup) ptr
      <*> (#peek proc_t, cgname) ptr
      <*> (#peek proc_t, supgid) ptr
      <*> (#peek proc_t, supgrp) ptr
      <*> (#peek proc_t, euser) ptr
      <*> (#peek proc_t, ruser) ptr
      <*> (#peek proc_t, suser) ptr
      <*> (#peek proc_t, fuser) ptr
      <*> (#peek proc_t, rgroup) ptr
      <*> (#peek proc_t, egroup) ptr
      <*> (#peek proc_t, sgroup) ptr
      <*> (#peek proc_t, fgroup) ptr
      <*> (#peek proc_t, cmd) ptr
      <*> (#peek proc_t, ring) ptr
      <*> (#peek proc_t, next) ptr
      <*> (#peek proc_t, pgrp) ptr
      <*> (#peek proc_t, session) ptr
      <*> (#peek proc_t, nlwp) ptr
      <*> (#peek proc_t, tgid) ptr
      <*> (#peek proc_t, tty) ptr
      <*> (#peek proc_t, euid) ptr
      <*> (#peek proc_t, egid) ptr
      <*> (#peek proc_t, ruid) ptr
      <*> (#peek proc_t, rgid) ptr
      <*> (#peek proc_t, suid) ptr
      <*> (#peek proc_t, sgid) ptr
      <*> (#peek proc_t, fuid) ptr
      <*> (#peek proc_t, fgid) ptr
      <*> (#peek proc_t, tpgid) ptr
      <*> (#peek proc_t, exit_signal) ptr
      <*> (#peek proc_t, processor) ptr
      <*> (#peek proc_t, oom_score) ptr
      <*> (#peek proc_t, oom_adj) ptr
      <*> (#peek proc_t, ns) ptr
      <*> (#peek proc_t, sd_mach) ptr
      <*> (#peek proc_t, sd_ouid) ptr
      <*> (#peek proc_t, sd_seat) ptr
      <*> (#peek proc_t, sd_sess) ptr
      <*> (#peek proc_t, sd_slice) ptr
      <*> (#peek proc_t, sd_unit) ptr
      <*> (#peek proc_t, sd_uunit) ptr
      <*> (#peek proc_t, lxcname) ptr

foreign import capi unsafe "read_proc_wrapper" readNextProcC :: Ptr ProcTabC -> Ptr ProcC -> IO (Ptr ProcC)

foreign import capi unsafe "free_proc_wrapper" freeProc :: Ptr ProcC -> IO ()

foreign import capi unsafe "readallprocs_simple" readAllProcsSimple :: CInt -> IO (Ptr (Ptr ProcC))

foreign import capi unsafe "readallprocs_pids" readAllProcsPids :: CInt -> Ptr CPid -> IO (Ptr (Ptr ProcC))

foreign import capi unsafe "readallprocs_uids" readAllProcsUids :: CInt -> Ptr CUid -> CInt -> IO (Ptr (Ptr ProcC))

foreign import capi unsafe "lookup_self_wrapper" readSelfProc :: IO (Ptr ProcC)
