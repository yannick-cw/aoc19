module One where

requiredFuel :: [Int] -> Int
requiredFuel = sum . (totalFuelForMass =<<)

totalFuelForMass :: Int -> [Int]
totalFuelForMass = takeWhile (> 0) . iterate fuelForWeight . fuelForWeight
  where fuelForWeight a = a `div` 3 - 2

