{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wall #-}

module TextCounter (TextCounter, new, count, toList, debugPopulation) where

import Control.Monad.ST (runST)
import qualified Counter as L
import Data.Bifunctor (first)
import Data.Bits (Bits (unsafeShiftL, (.&.)))
import Data.Foldable (for_)
import qualified Data.Text.Array as A
import Data.Text.Internal (Text (..))
import qualified Data.Text.Short as Short
import Data.Word (Word8)
import GHC.Exts (Int (I#), Word (W#), indexWord8ArrayAsWord#)
import qualified IntCounter as S

data TextCounter = MkTextCounter !S.IntCounter !(L.Counter Short.ShortText)

new :: Int -> Int -> IO TextCounter
new n m = do
  s <- S.new n
  l <- L.new m
  pure (MkTextCounter s l)

encode :: Text -> Int
encode txt@(Text a@(A.ByteArray ba) (I# j) n) =
  fromIntegral (W# (indexWord8ArrayAsWord# ba j) .&. (unsafeShiftL 1 (8 * n) - 1))

decode :: Int -> Text
decode n = myPack $ takeWhile (/= 0) $ go n
  where
    go n = let (q, r) = quotRem n 0x100 in fromIntegral r : go q

myPack :: [Word8] -> Text
myPack xs = runST $ do
  let n = length xs
  ma <- A.new n
  for_ (zip [0 ..] xs) $ uncurry (A.unsafeWrite ma)
  a <- A.unsafeFreeze ma
  pure (Text a 0 n)

count :: TextCounter -> Text -> IO ()
count (MkTextCounter s l) t@(Text a@(A.ByteArray ba) i n)
  --   | traceShow (i, n) False = undefined
  | n <= 8 = S.count s (encode t)
  | otherwise = L.count l (Short.fromText t)
{-# INLINE count #-}

toList :: TextCounter -> IO [(Text, Int)]
toList (MkTextCounter s l) = do
  xs <- map (first decode) <$> S.toList s
  ys <- map (first Short.toText) <$> L.toList l
  pure (xs ++ ys)

debugPopulation :: TextCounter -> IO (Int, Int)
debugPopulation (MkTextCounter s l) = do
  xs <- S.toList s
  ys <- L.toList l
  return (length xs, length ys)
