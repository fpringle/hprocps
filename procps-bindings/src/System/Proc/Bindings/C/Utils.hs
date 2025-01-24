module System.Proc.Bindings.C.Utils where

import Data.Maybe
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import System.Proc.Bindings.Error

eitherPeek :: (Ptr a -> IO b) -> Ptr a -> IO (Either ProcError b)
eitherPeek f ptr
  | ptr == nullPtr = pure $ Left NullPtrError
  | otherwise = Right <$> f ptr

readStringMaybe :: CString -> IO (Maybe String)
readStringMaybe = maybePeek peekCString
{-# INLINE readStringMaybe #-}

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = go
  where
    go [] = pure []
    go (a : as) =
      f a >>= \case
        Nothing -> go as
        Just b -> (b :) <$> go as

readStringList :: Ptr CString -> IO [String]
readStringList ptr
  | ptr == nullPtr = pure []
  | otherwise = peekArray0 nullPtr ptr >>= mapMaybeM readStringMaybe

readNullTermArray :: (Storable a, Num a, Eq a) => Ptr a -> IO [a]
readNullTermArray ptr = fromMaybe [] <$> readNullTermArrayMaybe ptr

readNullTermArrayMaybe :: (Storable a, Num a, Eq a) => Ptr a -> IO (Maybe [a])
readNullTermArrayMaybe = maybePeek $ peekArray0 0
