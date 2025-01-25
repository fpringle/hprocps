#include <proc/readproc.h>

module System.Proc.Bindings.Flags.C
  ( -- * Binary flags
    Flags (..)
  , flagsAsInt
  )
where

import Data.Bits
import Data.Foldable
import Foreign.C.Types

------------------------------------------------------------
-- Binary flags

{- | What information do we want about the processes?

This type will be translated into a C @int@ to be passed to the underlying library
(see 'flagsAsInt').

It's easiest to construct these using the 'Semigroup' instance:

@
myFlags :: 'Flags'
myFlags =
  'System.Proc.Bindings.Tab.Config.fillMem'
    <> 'System.Proc.Bindings.Tab.Config.fillCom'
    <> 'System.Proc.Bindings.Tab.Config.fillEnv'
    <> 'System.Proc.Bindings.Tab.Config.fillUser'
    <> 'System.Proc.Bindings.Tab.Config.fillGroup'
@
-}
data Flags = Flags
  { -- | Read @\/proc\/#\/statm@?
    flagMem :: Bool
  , -- | Read @\/proc\/#\/cmdline@?
    flagCom :: Bool
  , -- | Read @\/proc\/#\/environ@?
    flagEnv :: Bool
  , -- | Resolve user id number -> user name?
    flagUser :: Bool
  , -- | Resolve group id number -> group name?
    flagGroup :: Bool
  , -- | Read @\/proc\/#\/status@?
    flagStatus :: Bool
  , -- | Read @\/proc\/#\/stat@?
    flagStat :: Bool
  , -- | Read @\/proc\/#\/cmdline@?
    flagArg :: Bool
  , -- | Read @\/proc\/#\/cgroup@?
    flagCGroup :: Bool
  , -- | Resolve supplementary group id -> group name?
    flagSupGroup :: Bool
  , -- | Read @\/proc\/#\/oom_{score,adj}@?
    flagOOM :: Bool
  , -- | Read @\/proc\/#\/ns/@?
    flagNS :: Bool
  , -- | Read systemd information?
    flagSystemd :: Bool
  , -- | Read LXC name, if possible?
    flagLxc :: Bool
  , -- | Treat threads as if they were processes?
    flagLooseTasks :: Bool
  }

{- | Translate our nice haskell 'Flags' type into a C @int@. Basically a bitwise @OR@ of
the fields (see all the lines at the end of @readproc.h@ starting with @#define PROC_@).
-}
flagsAsInt :: Flags -> CInt
flagsAsInt Flags {..} =
  foldl' f (0 :: CInt) bits
  where
    f :: CInt -> (Bool, CInt) -> CInt
    f acc (False, _) = acc
    f acc (True, fl) = acc .|. fl

    bits =
      [ (flagMem, #{const PROC_FILLMEM})
      , (flagCom, #{const PROC_FILLCOM})
      , (flagEnv, #{const PROC_FILLENV})
      , (flagUser, #{const PROC_FILLUSR})
      , (flagGroup, #{const PROC_FILLGRP})
      , (flagStatus, #{const PROC_FILLSTATUS})
      , (flagStat, #{const PROC_FILLSTAT})
      , (flagArg, #{const PROC_FILLARG})
      , (flagCGroup, #{const PROC_FILLCGROUP})
      , (flagSupGroup, #{const PROC_FILLSUPGRP})
      , (flagOOM, #{const PROC_FILLOOM})
      , (flagNS, #{const PROC_FILLNS})
      , (flagSystemd, #{const PROC_FILLSYSTEMD})
      , (flagLxc, #{const PROC_FILL_LXC})
      , (flagLooseTasks, #{const PROC_LOOSE_TASKS})
      ]

instance Semigroup Flags where
  f1 <> f2 =
    Flags
      { flagMem = flagMem f1 || flagMem f2
      , flagCom = flagCom f1 || flagCom f2
      , flagEnv = flagEnv f1 || flagEnv f2
      , flagUser = flagUser f1 || flagUser f2
      , flagGroup = flagGroup f1 || flagGroup f2
      , flagStatus = flagStatus f1 || flagStatus f2
      , flagStat = flagStat f1 || flagStat f2
      , flagArg = flagArg f1 || flagArg f2
      , flagCGroup = flagCGroup f1 || flagCGroup f2
      , flagSupGroup = flagSupGroup f1 || flagSupGroup f2
      , flagOOM = flagOOM f1 || flagOOM f2
      , flagNS = flagNS f1 || flagNS f2
      , flagSystemd = flagSystemd f1 || flagSystemd f2
      , flagLxc = flagLxc f1 || flagLxc f2
      , flagLooseTasks = flagLooseTasks f1 || flagLooseTasks f2
      }

instance Monoid Flags where
  mempty =
    Flags
      { flagMem = False
      , flagCom = False
      , flagEnv = False
      , flagUser = False
      , flagGroup = False
      , flagStatus = False
      , flagStat = False
      , flagArg = False
      , flagCGroup = False
      , flagSupGroup = False
      , flagOOM = False
      , flagNS = False
      , flagSystemd = False
      , flagLxc = False
      , flagLooseTasks = False
      }
