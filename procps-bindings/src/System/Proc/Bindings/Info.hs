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
