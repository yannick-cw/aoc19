module ThreeSpec where

import           Test.Hspec
import           Three

spec :: Spec
spec = describe "day 3" $ do
  it "small example"
    $ intersections [["R8", "U5", "L5", "D3"], ["U7", "R6", "D4", "L4"]]
    `shouldBe` Right [6, 11]
  it "example2"
    $          (head <$> intersections
                 [ ["R75", "D30", "R83", "U83", "L12", "D49", "R71", "U7", "L72"]
                 , ["U62", "R66", "U55", "R34", "D71", "R55", "D58", "R83"]
                 ]
               )
    `shouldBe` Right 159
  it "example3"
    $          (head <$> intersections
                 [ [ "R98"
                   , "U47"
                   , "R26"
                   , "D63"
                   , "R33"
                   , "U87"
                   , "L62"
                   , "D20"
                   , "R33"
                   , "U53"
                   , "R51"
                   ]
                 , ["U98", "R91", "D20", "R16", "D67", "R40", "U7", "R15", "U6", "R7"]
                 ]
               )
    `shouldBe` Right 135
