{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

-- TODO: resize

module IntCounter (IntCounter, new, count, toList) where

import Control.Monad.Primitive (PrimMonad (PrimState), RealWorld, touch)
import Data.Bits (Bits (unsafeShiftL, unsafeShiftR))
import Data.Compact (Compact, compact, compactAdd, compactAddWithSharing, compactSize, getCompact)
import Data.Foldable (traverse_)
import Data.Hashable (Hashable (hash))
import Data.List (sortOn)
import Data.Primitive (sizeOf)
import Data.Primitive.ByteArray
import Debug.Trace (traceIO)
import Foreign.Ptr (Ptr, nullPtr, ptrToIntPtr)
import GHC.Exts hiding (toList)
import GHC.IO (IO (IO))
import System.IO
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (lookup)

newtype IntCounter = MkCounter (MutableByteArray RealWorld)

new :: Int -> IO IntCounter
new n = do
  a <- newByteArray (n * 2 * sizeOf (0 :: Int))
  setByteArray a 0 (n * 2) (-1 :: Int)
  pure (MkCounter a)
{-# INLINE new #-}

count :: IntCounter -> Int -> IO ()
count (MkCounter t) k = do
  let i = unsafeShiftL k 1 `mod` n
  slot <- readByteArray t i
  if
      -- slot is empty
      | slot == (-1 :: Int) -> do
        writeByteArray t i k
        writeByteArray t (i + 1) (1 :: Int)
      -- slot is filled
      | slot == k -> do
        v <- readByteArray t (i + 1)
        writeByteArray t (i + 1) (v + 1 :: Int)
      -- wrong slot
      | otherwise -> do
        go ((i + 2) `rem` n)
  where
    n = sizeofMutableByteArray t `quot` sizeOf (0 :: Int)
    go :: Int -> IO ()
    go !i = do
      slot <- readByteArray t i
      if
          -- slot is empty
          | slot == (-1 :: Int) -> do
            writeByteArray t i k
            writeByteArray t (i + 1) (1 :: Int)
          -- slot is filled
          | slot == k -> do
            v <- readByteArray t (i + 1)
            writeByteArray t (i + 1) (v + 1 :: Int)
          -- wrong slot
          | otherwise -> do
            go ((i + 2) `rem` n)
{-# INLINE count #-}

toList :: IntCounter -> IO [(Int, Int)]
toList (MkCounter t) =
  let n = sizeofMutableByteArray t `quot` sizeOf (0 :: Int)
      go :: [(Int, Int)] -> Int -> IO [(Int, Int)]
      go s !i
        | i == n = pure s
        | otherwise = do
          hFlush stdout
          slot <- readByteArray t i
          if slot == (-1 :: Int)
            then do
              go s (i + 2)
            else do
              v <- readByteArray t (i + 1)
              go ((slot, v) : s) (i + 2)
   in go [] 0
