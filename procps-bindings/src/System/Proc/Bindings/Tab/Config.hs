module System.Proc.Bindings.Tab.Config
  ( TableConfig (..)

    -- * Table filters
  , Filter (..)

    -- * Binary flags
  , Flags
  , flagsAsInt

    -- ** Constructing flags
  , noFlags
  , fillMem
  , fillCom
  , fillEnv
  , fillUser
  , fillGroup
  , fillStatus
  , fillStat
  , fillArg
  , fillCGroup
  , fillSupGroup
  , fillOOM
  , fillNS
  , fillSystemd
  , fillLxc
  , fillLooseTasks

    -- ** Accessors
  , flagMem
  , flagCom
  , flagEnv
  , flagUser
  , flagGroup
  , flagStatus
  , flagStat
  , flagArg
  , flagCGroup
  , flagSupGroup
  , flagOOM
  , flagNS
  , flagSystemd
  , flagLxc
  , flagLooseTasks

    -- * Util
  , branchTableConfig
  )
where

import Foreign
import Foreign.C.Types
import System.Posix.Types
import System.Proc.Bindings.Flags.C

{- | This type represents our preference of what processes we want information about,
and what information we want about them. It'll be translated into a list of arguments
to be passed to @openproc@ ('System.Proc.Bindings.Tab.openProcTab') or
@readproctab@ ('System.Proc.Bindings.openAllProcs').
-}
data TableConfig = TableConfig
  { binaryFlags :: Flags
  , tableFilter :: Filter
  }

branchTableConfig ::
  (CInt -> IO a) ->
  (CInt -> Ptr CPid -> IO a) ->
  (CInt -> Ptr CUid -> CInt -> IO a) ->
  (TableConfig -> IO a)
branchTableConfig simple onPids onUids (TableConfig flags tabFilter) = case tabFilter of
  NoFilter -> simple flagsInt
  ByPids pids ->
    withArray0 0 pids $ \pidPtr -> onPids flagsInt pidPtr
  ByUids uids ->
    withArrayLen uids $ \nuid uidPtr -> onUids flagsInt uidPtr (fromIntegral nuid)
  where
    flagsInt = flagsAsInt flags

------------------------------------------------------------
-- Table filters

{- | Which processes do we want to read information about?

This will determine which extra arguments we pass to @openproc@ or @readproctab@.
-}
data Filter
  = -- | Read all processes
    NoFilter
  | -- | Filter process by user IDs
    ByUids [CUid]
  | -- | Filter process by process IDs
    ByPids [CPid]

------------------------------------------------------------
-- Binary flags

-- | Only read 'System.Proc.Bindings.taskId's.
noFlags :: Flags
noFlags = mempty

-- | Read @\/proc\/#\/statm@.
fillMem :: Flags
fillMem = noFlags {flagMem = True}

-- | Read @\/proc\/#\/cmdline@.
fillCom :: Flags
fillCom = noFlags {flagCom = True}

-- | Read @\/proc\/#\/environ@.
fillEnv :: Flags
fillEnv = noFlags {flagEnv = True}

-- | Resolve user id number -> user name
fillUser :: Flags
fillUser = noFlags {flagUser = True}

-- | Resolve group id number -> group name
fillGroup :: Flags
fillGroup = noFlags {flagGroup = True}

-- | Read @\/proc\/#\/status@.
fillStatus :: Flags
fillStatus = noFlags {flagStatus = True}

-- | Read @\/proc\/#\/stat@.
fillStat :: Flags
fillStat = noFlags {flagStat = True}

-- | Read @\/proc\/#\/cmdline@. Seems to be treated identically to 'fillCom'.
fillArg :: Flags
fillArg = noFlags {flagArg = True}

-- | Read @\/proc\/#\/cgroup@.
fillCGroup :: Flags
fillCGroup = noFlags {flagCGroup = True}

-- | Resolve supplementary group id -> group name
fillSupGroup :: Flags
fillSupGroup = noFlags {flagSupGroup = True}

-- | Read @\/proc\/#\/oom_{score,adj}@.
fillOOM :: Flags
fillOOM = noFlags {flagOOM = True}

-- | Read @\/proc\/#\/ns/@.
fillNS :: Flags
fillNS = noFlags {flagNS = True}

-- | Read systemd information.
fillSystemd :: Flags
fillSystemd = noFlags {flagSystemd = True}

-- | Read LXC name, if possible.
fillLxc :: Flags
fillLxc = noFlags {flagLxc = True}

-- | Treat threads as if they were processes.
fillLooseTasks :: Flags
fillLooseTasks = noFlags {flagLooseTasks = True}
