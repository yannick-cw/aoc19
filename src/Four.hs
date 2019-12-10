{-# LANGUAGE LambdaCase #-}

module Four where

import           Data.List                      ( tails )
import           Data.Char                      ( digitToInt )

sameAdjacent :: [Int] -> Bool
sameAdjacent =
  any
      (\case
        [l, r] -> l == r
        _      -> False
      )
    . slidingWindow

increasing :: [Int] -> Bool
increasing = fst . foldl
  (\(isInc, lastNum) nextNum -> (isInc && (nextNum >= lastNum), nextNum))
  (True, 0)

slidingWindow :: [a] -> [[a]]
slidingWindow = init . init . map (take 2) . tails


allMatchingNums :: [[Int]]
allMatchingNums = filter (\num -> sameAdjacent num && increasing num)
                         (asList <$> [(231832 :: Int) .. 767346])
  where asList = map digitToInt . show
