module System.Proc.Bindings.Error
  ( ProcError (..)
  )
where

import Control.Exception

-- | An error that we catch while calling the underlying C library.
data ProcError
  = -- | We tried to do some operation using a 'Foreign.Ptr.nullPtr'
    NullPtrError
  deriving (Show)

instance Exception ProcError
