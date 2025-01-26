{- |
The fields of 'ProcInfo' come from @proc_t@, and are parsed from various files
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
module System.Proc.Bindings.Info
  ( -- * Concrete process information
    ProcInfo

    -- * Field accessors
  , module Export

    -- ** Auxiliary types
  , SignalMask
  , Address
  )
where

import System.Proc.Bindings.C
import System.Proc.Bindings.Info.Internal
import System.Proc.Bindings.Info.Internal as Export hiding (ProcInfo (..))
