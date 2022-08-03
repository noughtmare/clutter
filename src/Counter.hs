{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: resize

module Counter where -- (Counter, new, count, toList) where

import Control.Monad.Primitive (PrimMonad (PrimState), RealWorld, touch)
import Data.Bits (Bits (unsafeShiftL, unsafeShiftR))
import Data.Compact (Compact, compact, compactAdd, getCompact, compactAddWithSharing, compactSize)
import Data.Foldable (traverse_)
import Data.Hashable (Hashable (hash))
import Data.Primitive (sizeOf)
import Data.Primitive.ByteArray
import Debug.Trace (traceIO)
import Foreign.Ptr (Ptr, nullPtr, ptrToIntPtr)
import GHC.Exts hiding (toList)
import GHC.IO (IO (IO))
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (lookup)
import Data.List (sortOn)
import System.IO

data Counter k = MkCounter !(Compact ()) !(MutableByteArray RealWorld)

anyToPtr :: a -> IO (Ptr a)
anyToPtr !x = IO (\s -> case anyToAddr# x s of (# s, a #) -> (# s, Ptr a #))

ptrToAny :: Ptr a -> a
ptrToAny (Ptr a) = case addrToAny# a of (# x #) -> x

new :: forall k. Int -> IO (Counter k)
new n = do
  c <- compact ()
  a <- newByteArray (n * 2 * sizeOf (nullPtr :: Ptr k))
  setByteArray a 0 (n * 2) (nullPtr :: Ptr k)
  pure (MkCounter c a)
{-# INLINE new #-}

count :: forall k. (Hashable k, Eq k) => Counter k -> k -> IO ()
count (MkCounter c t) k = do
  let i = unsafeShiftL (hash k) 1 `mod` n
  slot <- readByteArray t i
  if
      -- slot is empty
      | slot == (nullPtr :: Ptr k) -> do
        p <- anyToPtr . getCompact =<< compactAdd c k
        writeByteArray t i p
        writeByteArray t (i + 1) (1 :: Int)
      -- slot is filled
      | ptrToAny slot == k -> do
        v <- readByteArray t (i + 1)
        writeByteArray t (i + 1) (v + 1 :: Int)
      -- wrong slot
      | otherwise -> do
        go ((i + 2) `rem` n)
  where
    n = sizeofMutableByteArray t `quot` sizeOf (nullPtr :: Ptr k)
    go :: Int -> IO ()
    go !i = do
      slot <- readByteArray t i
      if
          -- slot is empty
          | slot == nullPtr -> do
            p <- anyToPtr . getCompact =<< compactAdd c k
            writeByteArray t i p
            writeByteArray t (i + 1) (1 :: Int)
          -- slot is filled
          | ptrToAny slot == k -> do
            v <- readByteArray t (i + 1)
            writeByteArray t (i + 1) (v + 1 :: Int)
          -- wrong slot
          | otherwise -> do
            go ((i + 2) `rem` n)
{-# INLINE count #-}

toList :: forall k. Show k => Counter k -> IO [(k, Int)]
toList (MkCounter c t) =
  let n = sizeofMutableByteArray t `quot` sizeOf (nullPtr :: Ptr k)
      go :: [(k, Int)] -> Int -> IO [(k, Int)]
      go s !i
        | i == n = pure s
        | otherwise = do
          hFlush stdout
          slot <- readByteArray t i
          if slot == nullPtr
            then do
              go s (i + 2)
            else do
              let !k = ptrToAny slot
              !v <- readByteArray t (i + 1)
              go ((k, v) : s) (i + 2)
   in go [] 0 -- <* touch c
