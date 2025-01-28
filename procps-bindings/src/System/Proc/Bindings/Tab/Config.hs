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
  , editCGroupAsSingleVector
  , editCmdlineAsSingleVector
  , editEnvironAsSingleVector

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
  , flagEditCGroupSingleVector
  , flagEditCmdlineSingleVector
  , flagEditEnvironSingleVector

    -- * Util
  , branchTableConfig
  , openTableConfig
  )
where

import Control.Exception
import Foreign
import Foreign.C.Types
import GHC.Stack
import System.Posix.Types
import System.Proc.Bindings.C.Utils
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
  deriving (Show, Eq)

{- | Case analysis on 'TableConfig'. The three continuation arguments correspond to the three
constructors of 'Filter'.

Handles memory management and makes sure we pass the correct argument to
@openproc@ or @readproctab@.
-}
branchTableConfig ::
  HasCallStack =>
  (CInt -> IO a) ->
  (CInt -> Ptr CPid -> IO a) ->
  (CInt -> Ptr CUid -> CInt -> IO a) ->
  (TableConfig -> IO a)
branchTableConfig simple onPids onUids (TableConfig flags tabFilter) = do
  xprintf "branchTableConfig"
  case tabFilter of
    NoFilter -> simple flagsInt
    ByPids pids ->
      withArray0 0 pids $ \pidPtr -> do
        xprintf $ "PID pointer: " <> show pidPtr
        onPids flagsInt pidPtr
    ByUids uids ->
      withArrayLen uids $ \nuid uidPtr -> do
        xprintf $ "UID pointer: " <> show uidPtr
        onUids flagsInt uidPtr (fromIntegral nuid)
  where
    flagsInt = flagsAsInt flags

{- | Same as 'branchTableConfig', but without the bracketing.

The @IO ()@ return value is a cleanup function to free any memory that was allocated for
@openproc@, e.g an array of @uid_t@s or @pid_t@s.
-}
openTableConfig ::
  (CInt -> IO a) ->
  (CInt -> Ptr CPid -> IO a) ->
  (CInt -> Ptr CUid -> CInt -> IO a) ->
  (TableConfig -> IO (a, IO ()))
openTableConfig simple onPids onUids (TableConfig flags tabFilter) = do
  case tabFilter of
    NoFilter -> do
      a <- simple flagsInt
      pure (a, pure ())
    ByPids pids -> do
      pidPtr <- newArray0 0 pids
      a <- onPids flagsInt pidPtr `onException` free pidPtr
      pure (a, free pidPtr)
    ByUids uids -> do
      uidPtr <- newArray uids
      let nuid = fromIntegral $ length uids
      a <- onUids flagsInt uidPtr nuid `onException` free uidPtr
      pure (a, free uidPtr)
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
  deriving (Show, Eq)

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

-- | Edit @cgroup@ as a single vector.
editCGroupAsSingleVector :: Flags
editCGroupAsSingleVector = noFlags {flagEditCGroupSingleVector = True}

-- | Edit @cmdline@ as a single vector.
editCmdlineAsSingleVector :: Flags
editCmdlineAsSingleVector = noFlags {flagEditCmdlineSingleVector = True}

-- | Edit @environ@ as a single vector.
editEnvironAsSingleVector :: Flags
editEnvironAsSingleVector = noFlags {flagEditEnvironSingleVector = True}
