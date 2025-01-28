module System.Proc.Bindings.Error
  ( ProcError (..)
  , nullPtrError
  )
where

import Control.Exception
import Data.Function
import Data.List.NonEmpty hiding (length)
import GHC.Stack

-- | An error that we catch while calling the underlying C library.
newtype ProcError
  = -- | We tried to do some operation using a 'Foreign.Ptr.nullPtr'
    NullPtrError CallStack

instance Show ProcError where
  show = \case
    NullPtrError cs ->
      "NullPtrError\n" <> prettyCallStack cs

instance Exception ProcError

-- | Create a 'NullPtrError' with the current 'CallStack'.
nullPtrError :: HasCallStack => ProcError
nullPtrError =
  NullPtrError
    . fromCallSiteList
    . fmap addNumbers
    . groupBy ((==) `on` fst)
    . getCallStack
    $ popCallStack callStack
  where
    addNumbers ((name, loc) :| []) = (name, loc)
    addNumbers ne@((name, loc) :| _) = (name <> "*" <> show (length ne), loc)
