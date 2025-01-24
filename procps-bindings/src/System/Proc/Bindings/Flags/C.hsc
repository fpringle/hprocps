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

data Flags = Flags
  { flagMem :: Bool
  , flagCom :: Bool
  , flagEnv :: Bool
  , flagUser :: Bool
  , flagGroup :: Bool
  , flagStatus :: Bool
  , flagStat :: Bool
  , flagArg :: Bool
  , flagCGroup :: Bool
  , flagSupGroup :: Bool
  , flagOOM :: Bool
  , flagNS :: Bool
  , flagSystemd :: Bool
  , flagLxc :: Bool
  , flagLooseTasks :: Bool
  }

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
