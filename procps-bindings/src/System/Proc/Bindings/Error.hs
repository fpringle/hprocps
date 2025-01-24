module System.Proc.Bindings.Error
  ( ProcError (..)
  )
where

import Control.Exception

data ProcError
  = NullPtrError
  deriving (Show)

instance Exception ProcError
