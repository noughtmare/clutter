import TextCounter
import Data.Foldable (traverse_)
import Data.Ord (Down(Down))
import Data.List (sortOn)
import Main.Utf8
import GHC.IO (IO (IO))
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T

main = withUtf8 $ do
  t <- new 60000 60000
  traverse_ (count t . T.toLower) . T.words =<< T.getContents
  xs <- toList t
  print $ sortOn (Down . snd) xs
