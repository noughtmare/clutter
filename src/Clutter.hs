{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}

-- TODO: resize

module Clutter (HashTable, HashTableIO, new, insert, lookup, modify, toList) where

import Control.Monad.Primitive (PrimMonad (PrimState), RealWorld)
import Data.Bits (Bits (unsafeShiftL))
import Data.Hashable (Hashable (hash))
import Data.Primitive.Array
  ( MutableArray,
    newArray,
    readArray,
    sizeofMutableArray,
    writeArray,
  )
import GHC.Exts (Any, RealWorld, isTrue#, reallyUnsafePtrEquality#)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (lookup)

newtype HashTable s k v = MkHashTable (MutableArray s Any)

type HashTableIO = HashTable RealWorld

sentinel :: Any
sentinel = unsafeCoerce (0 :: Int)
{-# NOINLINE sentinel #-}

eq :: a -> a -> Bool
eq !x !y = isTrue# (reallyUnsafePtrEquality# x y)

new :: PrimMonad m => Int -> m (HashTable (PrimState m) k v)
new n = seq sentinel $ MkHashTable <$> newArray (unsafeShiftL n 1) sentinel
{-# INLINE new #-}

insert :: (PrimMonad m, Hashable k) => HashTable (PrimState m) k v -> k -> v -> m ()
insert (MkHashTable t) k v = do
  let i = unsafeShiftL (hash k) 1 `mod` n
  slot <- readArray t i
  if eq slot sentinel
    then do
      writeArray t i $! unsafeCoerce k
      writeArray t (i + 1) $! unsafeCoerce v
    else go ((i + 2) `rem` n)
  where
    n = sizeofMutableArray t
    go !i = do
      slot <- readArray t i
      if eq slot sentinel
        then do
          writeArray t i $! unsafeCoerce k
          writeArray t (i + 1) $! unsafeCoerce v
        else go ((i + 2) `rem` n)
{-# INLINE insert #-}

lookup :: (PrimMonad m, Hashable k, Eq k) => HashTable (PrimState m) k v -> k -> m (Maybe v)
lookup (MkHashTable t) k = do
  let i = unsafeShiftL (hash k) 1 `mod` n
  slot <- readArray t i
  if
      | eq slot sentinel -> pure Nothing
      | k == unsafeCoerce slot -> Just . unsafeCoerce <$> readArray t (i + 1)
      | otherwise -> go ((i + 2) `rem` n)
  where
    n = sizeofMutableArray t
    go !i = do
      slot <- readArray t i
      if
          | eq slot sentinel -> pure Nothing
          | k == unsafeCoerce slot -> Just . unsafeCoerce <$> readArray t (i + 1)
          | otherwise -> go ((i + 2) `rem` n)
{-# INLINE lookup #-}

modify :: (PrimMonad m, Hashable k, Eq k) => HashTable (PrimState m) k v -> v -> (v -> v) -> k -> m ()
modify (MkHashTable t) !z f k = do
  let i = unsafeShiftL (hash k) 1 `mod` n
  slot <- readArray t i
  if
      -- slot is empty
      | eq slot sentinel -> do
          writeArray t i $! unsafeCoerce k
          writeArray t (i + 1) $! unsafeCoerce z
      -- slot is filled
      | k == unsafeCoerce slot -> do
          v <- unsafeCoerce <$> readArray t (i + 1)
          writeArray t (i + 1) $! unsafeCoerce (f v)
      -- wrong slot
      | otherwise -> go ((i + 2) `rem` n)
  where
    n = sizeofMutableArray t
    go !i = do
      slot <- readArray t i
      if
          -- slot is empty
          | eq slot sentinel -> do
              writeArray t i $! unsafeCoerce k
              writeArray t (i + 1) $! unsafeCoerce z
          -- slot is filled
          | k == unsafeCoerce slot -> do
              v <- unsafeCoerce <$> readArray t (i + 1)
              writeArray t (i + 1) $! unsafeCoerce (f v)
          -- wrong slot
          | otherwise -> go ((i + 2) `rem` n)
{-# INLINE modify #-}

toList :: (PrimMonad m) => HashTable (PrimState m) k v -> m [(k, v)]
toList (MkHashTable t) =
  let n = sizeofMutableArray t
      go s !i
        | i == n = pure s
        | otherwise = do
            slot <- readArray t i
            if eq slot sentinel
              then go s (i + 2)
              else do
                let k = unsafeCoerce slot
                v <- unsafeCoerce <$> readArray t (i + 1)
                go ((k, v) : s) (i + 2)
   in go [] 0
