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

------------------------------------------------------------
-- Binary flags

noFlags :: Flags
noFlags = mempty

fillMem :: Flags
fillMem = noFlags {flagMem = True}

fillCom :: Flags
fillCom = noFlags {flagCom = True}

fillEnv :: Flags
fillEnv = noFlags {flagEnv = True}

fillUser :: Flags
fillUser = noFlags {flagUser = True}

fillGroup :: Flags
fillGroup = noFlags {flagGroup = True}

fillStatus :: Flags
fillStatus = noFlags {flagStatus = True}

fillStat :: Flags
fillStat = noFlags {flagStat = True}

fillArg :: Flags
fillArg = noFlags {flagArg = True}

fillCGroup :: Flags
fillCGroup = noFlags {flagCGroup = True}

fillSupGroup :: Flags
fillSupGroup = noFlags {flagSupGroup = True}

fillOOM :: Flags
fillOOM = noFlags {flagOOM = True}

fillNS :: Flags
fillNS = noFlags {flagNS = True}

fillSystemd :: Flags
fillSystemd = noFlags {flagSystemd = True}

fillLxc :: Flags
fillLxc = noFlags {flagLxc = True}

fillLooseTasks :: Flags
fillLooseTasks = noFlags {flagLooseTasks = True}

------------------------------------------------------------
-- Table filters

data Filter
  = NoFilter
  | ByUids [CUid]
  | ByPids [CPid]

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
