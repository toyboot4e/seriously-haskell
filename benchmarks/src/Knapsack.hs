-- | ナップサック問題の様々な解法
--
-- https://atcoder.jp/contests/dp/tasks/dp_d

module Knapsack
  ( denseU,
    denseV,
    sparseList,
    sparseListForced,
    sparseIM,
    sparseIMForced,
    sparseU,
    sparseMonoList,
    sparseMonoU,
  )
where

import Control.DeepSeq
import Data.Bool (bool)
import Data.IntMap.Strict qualified as IM
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U

-- | @U.Vector@ を密な DP 配列とする解法
denseU :: Int -> U.Vector (Int, Int) -> Int
denseU maxW = U.maximum . U.foldl' step s0
  where
    s0 = U.generate (maxW + 1) $ bool minBound 0 . (== 0)
    step sofar (!dw, !dv) = U.imap f sofar
      where
        f :: Int -> Int -> Int
        f w0 v0 = max v0 $ maybe 0 (dv +) (sofar U.!? (w0 - dw))

-- | @V.Vector@ を密な DP 配列とする解法 (TLE)
denseV :: Int -> U.Vector (Int, Int) -> Int
denseV maxW = V.maximum . U.foldl' step s0
  where
    s0 = V.generate (maxW + 1) $ bool minBound 0 . (== 0)
    step sofar (!dw, !dv) = V.imap f sofar
      where
        f :: Int -> Int -> Int
        f w0 v0 = max v0 $ maybe 0 (dv +) (sofar V.!? (w0 - dw))

-- | @[Int]@ を疎な DP 配列とする解法 (TLE)
sparseList :: Int -> U.Vector (Int, Int) -> Int
sparseList maxW = maximum . map snd . U.foldl' step s0
  where
    s0 = [(0, 0)] :: [(Int, Int)]
    step wvs (!dw, !dv) =
      merge wvs $ filter ((<= maxW) . fst) $ map (\(!w, !v) -> (w + dw, v + dv)) wvs

    merge :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    merge xs [] = xs
    merge [] xs = xs
    merge xxs@(!x : xs) yys@(!y : ys) = case compare (fst x) (fst y) of
      LT -> x : merge xs yys
      GT -> y : merge xxs ys
      EQ -> (fst x, max (snd x) (snd y)) : merge xs ys

-- | @[Int]@ を疎な DP 配列としつつ、 @force@ にかける解法 (AC)
sparseListForced :: Int -> U.Vector (Int, Int) -> Int
sparseListForced maxW = maximum . map snd . U.foldl' step s0
  where
    s0 = [(0, 0)] :: [(Int, Int)]
    step wvs (!dw, !dv) =
      force . merge wvs $ filter ((<= maxW) . fst) $ map (\(!w, !v) -> (w + dw, v + dv)) wvs

    merge :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    merge xs [] = xs
    merge [] xs = xs
    merge xxs@(!x : xs) yys@(!y : ys) = case compare (fst x) (fst y) of
      LT -> x : merge xs yys
      GT -> y : merge xxs ys
      EQ -> (fst x, max (snd x) (snd y)) : merge xs ys

-- | @IM.IntMap Int@ を疎な DP 配列とする解法 (TLE)
sparseIM :: Int -> U.Vector (Int, Int) -> Int
sparseIM maxW = maximum . U.foldl' step s0
  where
    s0 = IM.singleton (0 :: Int) (0 :: Int)
    step wvs (!dw, !dv) =
      IM.unionWith max wvs $ IM.fromList . filter ((<= maxW) . fst) . map (\(!w, !v) -> (w + dw, v + dv)) $ IM.assocs wvs

-- | @IM.IntMap Int@ を疎な DP 配列としつつ @force@ にかける解法 (効果無し、 TLE)
sparseIMForced :: Int -> U.Vector (Int, Int) -> Int
sparseIMForced maxW = maximum . U.foldl' step s0
  where
    s0 = IM.singleton (0 :: Int) (0 :: Int)
    step wvs (!dw, !dv) =
      force . IM.unionWith max wvs $ IM.fromList . filter ((<= maxW) . fst) . map (\(!w, !v) -> (w + dw, v + dv)) $ IM.assocs wvs

-- | @U.Vector@ を疎な DP 配列としつつ、 @force@ にかける解法 (AC)
sparseU :: Int -> U.Vector (Int, Int) -> Int
sparseU maxW = U.maximum . U.map snd . U.foldl' step s0
  where
    s0 = U.singleton (0 :: Int, 0 :: Int)
    step wvs (!dw, !dv) =
      U.unfoldr merge . (,) wvs $ U.filter ((<= maxW) . fst) $ U.map (\(!w, !v) -> (w + dw, v + dv)) wvs

    merge :: (U.Vector (Int, Int), U.Vector (Int, Int)) -> Maybe ((Int, Int), (U.Vector (Int, Int), U.Vector (Int, Int)))
    merge (!xs, !ys) = case (U.uncons xs, U.uncons ys) of
      (Nothing, Nothing) -> Nothing
      (Just (!x, !xs'), Nothing) -> Just (x, (xs', U.empty))
      (Nothing, Just (!y, !ys')) -> Just (y, (U.empty, ys'))
      (Just (!x, !xs'), Just (!y, !ys')) -> case compare (fst x) (fst y) of
        LT -> Just (x, (xs', ys))
        GT -> Just (y, (xs, ys'))
        EQ -> Just ((fst x, max (snd x) (snd y)), (xs', ys'))

-- | @[Int]@ を疎な DP 配列としつつ、 @(w, v)@ の単調増加列とする方法 (爆速で AC)
sparseMonoList :: Int -> U.Vector (Int, Int) -> Int
sparseMonoList maxW = maximum . map snd . U.foldl' step s0
  where
    s0 = [(0, 0)] :: [(Int, Int)]
    step wvs (!dw, !dv) =
      merge (-1 :: Int) wvs $ filter ((<= maxW) . fst) $ map (\(!w, !v) -> (w + dw, v + dv)) wvs

    merge :: Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    merge _ xs [] = xs
    merge _ [] ys = ys
    merge maxV xxs@(x : xs) yys@(y : ys)
      | snd x <= maxV = merge maxV xs yys
      | snd y <= maxV = merge maxV xxs ys
      | otherwise = case compare (fst x) (fst y) of
          LT -> x : merge (max maxV (snd x)) xs yys
          GT -> y : merge (max maxV (snd y)) xxs ys
          EQ ->
            if maxV' == maxV
              then merge maxV' xs ys
              else (fst x, maxV') : merge maxV' xs ys
            where
              !maxV' = max (snd x) (snd y) `max` maxV

-- | @U.Vector Int@ を疎な DP 配列としつつ、 @(w, v)@ の単調増加列とする方法 (@sparseMonoList@ と同程度の速度で AC)
sparseMonoU :: Int -> U.Vector (Int, Int) -> Int
sparseMonoU maxW = U.maximum . U.map snd . U.foldl' step s0
  where
    s0 = U.singleton (0 :: Int, 0 :: Int)
    step wvs (!dw, !dv) =
      U.unfoldr merge . (,,) (-1 :: Int) wvs $ U.filter ((<= maxW) . fst) $ U.map (\(!w, !v) -> (w + dw, v + dv)) wvs

    merge :: (Int, U.Vector (Int, Int), U.Vector (Int, Int)) -> Maybe ((Int, Int), (Int, U.Vector (Int, Int), U.Vector (Int, Int)))
    merge (!maxV, !xs, !ys) = case (U.uncons xs, U.uncons ys) of
      (Nothing, Nothing) -> Nothing
      (Just (!x, !xs'), Nothing) | snd x <= maxV -> merge (maxV, xs', U.empty)
      (Just (!x, !xs'), Nothing) -> Just (x, (snd x, xs', U.empty))
      (Nothing, Just (!y, !ys')) | snd y <= maxV -> merge (maxV, U.empty, ys')
      (Nothing, Just (!y, !ys')) -> Just (y, (snd y, U.empty, ys'))
      (Just (!x, !xs'), Just (!y, !ys')) -> case compare (fst x) (fst y) of
        LT ->
          if snd x <= maxV
            then merge (maxV, xs', ys)
            else Just (x, (snd x, xs', ys))
        GT ->
          if snd y <= maxV
            then merge (maxV, xs, ys')
            else Just (y, (snd y, xs, ys'))
        EQ ->
          if maxV' == maxV
            then merge (maxV, xs', ys')
            else Just ((fst x, maxV'), (maxV', xs', ys'))
          where
            !maxV' = max (snd x) (snd y) `max` maxV
