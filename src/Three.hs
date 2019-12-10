{-# LANGUAGE LambdaCase #-}

module Three where

import           Data.List                      ( sort
                                                , tails
                                                )

data D = U Int | D Int | L Int | R Int deriving (Show)
data Line = Line (Int, Int) (Int, Int)

parseLine :: [String] -> Either String [D]
parseLine = traverse
  (\case
    ('U' : num) -> Right $ U $ read num
    ('D' : num) -> Right $ D $ read num
    ('L' : num) -> Right $ L $ read num
    ('R' : num) -> Right $ R $ read num
    other       -> Left $ "Invalid command: " ++ other
  )

intersections :: [[String]] -> Either String [Int]
intersections [fstLine, sndLine] =
  (\ln1 ln2 ->
      let fstCoords          = init (lineToCoords ln1)
          sndCoords          = init (lineToCoords ln2)
          intersectingCoords = (fstCoords `intersectLines` sndCoords)
      in  sort $ (\(l, r) -> abs l + abs r) <$> intersectingCoords
    )
    <$> parseLine fstLine
    <*> parseLine sndLine
 where
  lineToCoords = foldl
    (\coords command ->
      let (x, y) = head coords
      in  case command of
            (U n) -> (x, y + n) : coords
            (D n) -> (x, y - n) : coords
            (L n) -> (x - n, y) : coords
            (R n) -> (x + n, y) : coords
    )
    [(0, 0)]
intersections _ = Left "Input Broken"

intersectLines :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
intersectLines line1 line2 = do
  l1 <- toLines line1
  l2 <- toLines line2
  l1 `intersect` l2 ++ l2 `intersect` l1
 where
  toLines =
    map
        (\case
          [(x1, y1), (x2, y2)] -> Line (x1, y1) (x2, y2)
          _                    -> Line (0, 0) (0, 0) -- impossible
        )
      . (init . init . map (take 2) . tails) -- creates sliding window [1,2,3,4] -> [[1,2],[2,3],[3,4]]



intersect :: Line -> Line -> [(Int, Int)]
intersect (Line (x1, y1) (x2, y2)) (Line (x1', y1') (x2', y2'))
  | x1 == x2 && y1' == y2'
  = let intersection = (x1, y1')
    in  [ intersection | isIn y1' y1 y2 && isIn x1 x1' x2' ]
intersect _ _ = []

isIn :: Int -> Int -> Int -> Bool
isIn p bound1 bound2 =
  (p <= bound1 && p >= bound2) || (p >= bound1 && p <= bound2)

