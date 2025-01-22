module System.Proc.Flags
  ( TableConfig (..)

    -- * Table filters
  , Filter (..)

    -- * Binary flags
  , Flags

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
  , flagSystemD
  , flagLooseTasks
  )
where

import System.Posix.Types

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
  , flagSystemD :: Bool
  , flagLooseTasks :: Bool
  }

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
fillSystemd = noFlags {flagSystemD = True}

fillLooseTasks :: Flags
fillLooseTasks = noFlags {flagLooseTasks = True}

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
      , flagSystemD = flagSystemD f1 || flagSystemD f2
      , flagLooseTasks = flagLooseTasks f1 || flagLooseTasks f2
      }

instance Monoid Flags where
  mempty = noFlags

noFlags :: Flags
noFlags =
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
    , flagSystemD = False
    , flagLooseTasks = False
    }

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
