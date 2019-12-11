{-# LANGUAGE LambdaCase #-}

module Four where

import           Data.List                      ( tails )
import           Data.Char                      ( digitToInt )
import           Safe                           ( headMay
                                                , lastMay
                                                )

sameAdjacent :: [Int] -> Bool
sameAdjacent =
  any
      (\case
        [l, r] -> l == r
        _      -> False
      )
    . slidingWindow 2

adjacentOnlyTwice :: [Int] -> Bool
adjacentOnlyTwice =
  (\windows ->
      hasInnerDuplicates windows
        || firstDuplicate windows
        || lastDuplicate windows
    )
    . slidingWindow 4
 where
  hasInnerDuplicates = any
    (\case
      [l, m1, m2, r] -> m1 == m2 && m1 /= l && m1 /= r
      _              -> False
    )
  firstDuplicate =
    all
        (\case
          [l, r, n, _] -> l == r && r /= n
          _            -> False
        )
      . headMay
  lastDuplicate =
    all
        (\case
          [_, n, l, r] -> l == r && r /= n
          _            -> False
        )
      . lastMay

increasing :: [Int] -> Bool
increasing = fst . foldl
  (\(isInc, lastNum) nextNum -> (isInc && (nextNum >= lastNum), nextNum))
  (True, 0)

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow size list =
  let windows = map (take size) (tails list)
  in  take (length windows - size) windows


checkIt :: [[Int] -> Bool] -> [[Int]]
checkIt conds = filter (\num -> and ((\f -> f num) <$> conds))
                       (asList <$> [(231832 :: Int) .. 767346])
  where asList = map digitToInt . show


allMatchingNums :: [[Int]]
allMatchingNums = checkIt [sameAdjacent, increasing]

allMatchingNums2 :: [[Int]]
allMatchingNums2 = checkIt [adjacentOnlyTwice, increasing]
