{-# LANGUAGE CPP #-}

module System.Proc.Bindings.C.Utils where

import Data.List
import Data.Maybe
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack
import System.Proc.Bindings.Error

eitherPeek :: HasCallStack => (Ptr a -> IO b) -> Ptr a -> IO (Either ProcError b)
eitherPeek f ptr
  | ptr == nullPtr = pure $ Left nullPtrError
  | otherwise = Right <$> f ptr

readStringMaybe :: HasCallStack => CString -> IO (Maybe String)
readStringMaybe = maybePeek peekCString
{-# INLINE readStringMaybe #-}

mapMaybeM :: HasCallStack => (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = go
  where
    go [] = pure []
    go (a : as) =
      f a >>= \case
        Nothing -> go as
        Just b -> (b :) <$> go as

readStringList :: HasCallStack => Ptr CString -> IO [String]
readStringList ptr
  | ptr == nullPtr = pure []
  | otherwise = peekArray0 nullPtr ptr >>= mapMaybeM readStringMaybe

readNullTermArray :: HasCallStack => (Storable a, Num a, Eq a) => Ptr a -> IO [a]
readNullTermArray ptr = fromMaybe [] <$> readNullTermArrayMaybe ptr

readNullTermArrayMaybe :: HasCallStack => (Storable a, Num a, Eq a) => Ptr a -> IO (Maybe [a])
readNullTermArrayMaybe = maybePeek $ peekArray0 0

fmtCallStack :: CallStack -> String
fmtCallStack = intercalate " > " . reverse . fmap fst . getCallStack

xprintf :: HasCallStack => String -> IO ()
#ifdef DEBUG
xprintf s = putStrLn $ "[H] " <> s <> "      - " <> fmtCallStack (popCallStack callStack)
#else
xprintf _ = pure ()
#endif

xreturn :: (HasCallStack, Show a) => a -> IO a
xreturn a = do
  xprintf $ "  return " <> show a
  pure a
