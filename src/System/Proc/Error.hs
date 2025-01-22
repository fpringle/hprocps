module System.Proc.Error
  ( ProcError (..)
  )
where

import Control.Exception

data ProcError
  = NullPtrError
  deriving (Show)

instance Exception ProcError
