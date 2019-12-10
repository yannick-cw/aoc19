{-# LANGUAGE LambdaCase #-}

module Three where

import           Data.List                      ( sort
                                                , tails
                                                )

data D = U Int | D Int | L Int | R Int deriving (Show)
data Coord = Coord { xP :: Int, yP :: Int , steps :: Int } deriving (Show, Eq)
instance Ord Coord where
  c1 <= c2 = steps c1 <= steps c2

data Line = Line Coord Coord

parseLine :: [String] -> Either String [D]
parseLine = traverse
  (\case
    ('U' : num) -> Right $ U $ read num
    ('D' : num) -> Right $ D $ read num
    ('L' : num) -> Right $ L $ read num
    ('R' : num) -> Right $ R $ read num
    other       -> Left $ "Invalid command: " ++ other
  )

nearestIntersection :: [[String]] -> Either String [Int]
nearestIntersection lns =
  sort . map (\(Coord x y _) -> abs x + abs y) <$> intersections lns

nearestStepsIntersection :: [[String]] -> Either String [Coord]
nearestStepsIntersection lns = sort <$> intersections lns

intersections :: [[String]] -> Either String [Coord]
intersections [fstLine, sndLine] =
  (\ln1 ln2 ->
      let fstCoords = drop 1 (reverse (lineToCoords ln1))
          sndCoords = drop 1 (reverse (lineToCoords ln2))
      in  fstCoords `intersectLines` sndCoords
    )
    <$> parseLine fstLine
    <*> parseLine sndLine
 where
  lineToCoords = foldl
    (\coords command ->
      let (Coord x y s) = head coords
      in  case command of
            (U n) -> Coord x (y + n) (s + n) : coords
            (D n) -> Coord x (y - n) (s + n) : coords
            (L n) -> Coord (x - n) y (s + n) : coords
            (R n) -> Coord (x + n) y (s + n) : coords
    )
    [Coord 0 0 0]
intersections _ = Left "Input Broken"

intersectLines :: [Coord] -> [Coord] -> [Coord]
intersectLines line1 line2 = do
  l1 <- toLines line1
  l2 <- toLines line2
  l1 `intersect` l2 ++ l2 `intersect` l1
 where
  toLines =
    map
        (\case
          [c1, c2] -> Line c1 c2
          _        -> Line (Coord 0 0 0) (Coord 0 0 0) -- impossible
        )
      . (init . init . map (take 2) . tails) -- creates sliding window [1,2,3,4] -> [[1,2],[2,3],[3,4]]



intersect :: Line -> Line -> [Coord]
intersect (Line (Coord x1 y1 s1) (Coord x2 y2 _)) (Line (Coord x1' y1' s2) (Coord x2' y2' _))
  | x1 == x2 && y1' == y2'
  = let stepY        = abs (y1 - y1')
        stepX        = abs (x1' - x1)
        intersection = Coord x1 y1' (s1 + s2 + (stepY + stepX))
    in  [ intersection | isIn y1' y1 y2 && isIn x1 x1' x2' ]
intersect _ _ = []

isIn :: Int -> Int -> Int -> Bool
isIn p bound1 bound2 =
  (p <= bound1 && p >= bound2) || (p >= bound1 && p <= bound2)

