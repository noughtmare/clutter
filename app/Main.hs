import Counter
import Data.Foldable (traverse_)
import Data.Ord (Down(Down))
import Data.List (sortOn)
import Main.Utf8
import GHC.IO (IO (IO))
import Data.Char

main = withUtf8 $ do
  t <- new 60000
  traverse_ (count t . map toLower) . words =<< getContents
  xs <- toList t
  print $ sortOn (Down . snd) xs
