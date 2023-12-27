import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.Maybe
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed qualified as V
import Knapsack

ints2 :: BS.ByteString -> ((Int, Int), BS.ByteString)
ints2 !bs0 =
  let (!a1, !bs1) = fromJust $ BS.readInt (BS.dropWhile isSpace bs0)
      (!a2, !bs2) = fromJust $ BS.readInt (BS.dropWhile isSpace bs1)
   in ((a1, a2), bs2)

readInput :: IO ((Int, Int), U.Vector (Int, Int))
readInput = do
  !bs <- BS.readFile "bench/cases/knapsack-1_05.in"
  let ((!n, !maxW), !bs') = ints2 bs
  let vws = U.unfoldrExactN n ints2 bs'
  return ((n, maxW), vws)

-- | 実行時間の測定用
main :: IO ()
main = do
  ((!_nItems, !w), !input) <- readInput

  -- unboxed な vector を使った解法
  -- let !res = denseU w input

  -- boxed な vector を使った解法
  let !res = denseV w input

  print res

