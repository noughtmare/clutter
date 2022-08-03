{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
module TextCounter (TextCounter, new, count, toList) where

import qualified Counter as L
import qualified IntCounter as S
import Data.Text.Internal ( Text(..) )
import qualified Data.Text.Array as A
import Data.Bits ( Bits(unsafeShiftL) )
import GHC.Exts (sizeofByteArray#, Int (I#))
import Data.Bifunctor (first)
import Data.Word ( Word8 )
import Control.Monad.ST ( runST )
import Data.Foldable (for_)
import qualified Data.Text.Short as Short

data TextCounter = MkTextCounter !(L.Counter Short.ShortText) !S.IntCounter

new :: Int -> IO TextCounter
new n = do
  s <- S.new n
  l <- L.new n
  pure (MkTextCounter l s)

encode :: Text -> Int
encode (Text a@(A.ByteArray ba) j n) = go 0 0 where
  go !s !i
    | i == n = s
    | otherwise = go (fromIntegral (A.unsafeIndex a (j + i)) + s `unsafeShiftL` 8) (i + 1)

decode :: Int -> Text
decode n = myPack $ takeWhile (/= 0) $ go n where
  go n = let (q, r) = quotRem n 0x100 in fromIntegral r : go q

myPack :: [Word8] -> Text
myPack xs = runST $ do
  let n = length xs
  ma <- A.new n
  for_ (zip [0..] xs) $ uncurry (A.unsafeWrite ma)
  a <- A.unsafeFreeze ma
  pure (Text a 0 n)

count :: TextCounter -> Text -> IO ()
count (MkTextCounter large small) t@(Text a@(A.ByteArray ba) i n)
  | n <= 8 = S.count small (encode t)
  | otherwise = L.count large (Short.fromText t)

toList :: TextCounter -> IO [(Text,Int)]
toList (MkTextCounter l s) = do
  xs <- map (first Short.toText) <$> L.toList l
  ys <- map (first decode) <$> S.toList s
  pure (xs ++ ys)
