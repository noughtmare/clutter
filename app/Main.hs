{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
import Counter
import Data.Foldable (traverse_)
import Data.Ord (Down(Down))
import Data.List (sortOn)
import Main.Utf8
import GHC.IO (IO (IO))
import GHC.Exts (touch#)
import Data.Char

touch :: a -> IO ()
touch x = IO (\s -> (# touch# x s, () #))

main = withUtf8 $ do
  t <- new 60000 :: IO (Counter String)
  traverse_ (count t . map toLower) . words =<< getContents
  print . sortOn (Down . snd) =<< toList t
  touch t
