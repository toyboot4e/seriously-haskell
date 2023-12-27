#!/usr/bin/env stack
{- stack script --resolver lts-21.7 --package array --package bytestring --package containers --package deepseq --package extra --package hashable --package unordered-containers --package heaps --package utility-ht --package vector --package vector-algorithms --package primitive --package QuickCheck --package random --package transformers --ghc-options "-D DEBUG" -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable (forM_)
import Numeric.Extra (intToDouble)
import System.IO

-- USAGE:
--   cabal bench --benchmark-options='--csv a.csv +RTS -T'
--   cat a.csv | stack plot.hs

-- | Benchmark result parsed from `tasty-bench= output.
data Stat = Stat
  { nameS :: String,
    meanPsS :: Int,
    stdevPsS :: Int,
    allocatedS :: Int,
    copiedS :: Int,
    peakMemoryBytesS :: Int
  }
  deriving (Show)

-- | Parses a line of stat string into a `Stat` object.
-- # Example input
-- Name,Mean (ps),2*Stdev (ps),Allocated,Copied,Peak Memory
-- All.knapsack.dense-unboxed-vector,4266859137,224226858,78708412,7907,20971520
-- All.knapsack.dense-boxed-vector,1106044079100,35367373128,849150161,2054579171,2125463552
parseStat :: String -> Stat
parseStat statLine =
  let parsed = words $ map (\case ',' -> ' '; c -> c) statLine
      [nameS, meanPsS_, stdevPsS_, allocatedS_, copiedS_, peakMemoryBytesS_] = parsed
   in Stat
        { nameS,
          meanPsS = read meanPsS_,
          stdevPsS = read stdevPsS_,
          allocatedS = read allocatedS_,
          copiedS = read copiedS_,
          peakMemoryBytesS = read peakMemoryBytesS_
        }

log1000 :: Int -> Int
log1000 = inner 0
  where
  inner n r
    | r < 1000 = n
    | otherwise = inner (n + 1) (r `div` 1000)

-- | Shows pico seconds in a human-readable format.
showPs :: Int -> String
showPs x = padL (show up) ++ "." ++ padR (show down) ++ " [" ++ unit ++ "]" ++ unitPad
  where
    n1000 = log1000 x
    -- up.down
    up = x `div` (1000 ^ n1000) `mod` 1000
    down = x `div` (1000 ^ max 0 (n1000 - 1)) `mod` 1000
    -- space/zero padding
    padL s = replicate (3 - length s) ' ' ++ s
    padR s = s ++ replicate (3 - length s) '0'
    unit = ["ps", "ns", "μs", "ms", "s"] !! n1000
    unitPad
      | n1000 == 4 = " "
      | otherwise = ""

showMem :: Stat -> String
showMem Stat { .. } = "(" ++ showBytes allocatedS ++ " / " ++ showBytes copiedS ++ ")"

showBytes :: Int -> String
showBytes x = padL (show up) ++ "." ++ show (down `div` 100) ++ " [" ++ unit ++ "]" ++ unitPad
  where
    n1000 = log1000 x
    -- up.down
    up = x `div` (1000 ^ n1000) `mod` 1000
    down = x `div` (1000 ^ max 0 (n1000 - 1)) `mod` 1000
    padL s = replicate (3 - length s) ' ' ++ s
    unit = ["B", "KB", "MB", "GB"] !! n1000
    unitPad
     | n1000 == 0 = " "
     | otherwise = ""

-- | Shows a stat in one line.
showSpeedStat :: Int -> Int -> Stat -> String
showSpeedStat maxNameLen maxBlockCount stat@Stat { .. } =
  nameS ++ namePadding ++ ": " ++ bar ++ barPadding ++ " " ++ showPs meanPsS ++ " " ++ showMem stat
  where
    namePadding = replicate (maxNameLen - length nameS) ' '
    nBlocks = countBlocks stat
    bar = replicate nBlocks blockChar
    barPadding = replicate (maxBlockCount - nBlocks) ' '

countBlocks :: Stat -> Int
countBlocks Stat { .. } = round $ meanLog10 * intToDouble blockScale
  where
    meanLog10 :: Double
    -- μs
    meanLog10 = logBase 10.0 (intToDouble meanPsS / (1000 * 1000))

scaleUnits :: [String]
scaleUnits = ["1μs", "10μs", "100μs", "1ms", "10ms", "100ms", "1s"]

scaleDots :: String
scaleDots = concat . replicate (length scaleUnits) $ '.' : replicate (blockScale - 1) ' '

scaleDotNames :: String
scaleDotNames = concatMap f scaleUnits
  where
    f s = s ++ replicate (blockScale - length s) ' '

-- | Configuration
blockChar :: Char
blockChar = '▇'

-- | Configuration
blockScale :: Int
blockScale = 12

main :: IO ()
main = do
  ([csvHeader], statLines) <- splitAt 1 . lines <$> getContents

  let stats = map parseStat statLines

  let maxNameLen = maximum $ map (length . nameS) stats
  let maxBlockCount = maximum $ map countBlocks stats

  forM_ stats $ \stat -> do
    putStrLn $ showSpeedStat maxNameLen maxBlockCount stat

  let pad = replicate (maxNameLen + 2) ' '
  putStrLn $ pad ++ scaleDots
  putStrLn $ pad ++ scaleDotNames

