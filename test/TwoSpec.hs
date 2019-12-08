module TwoSpec where

import           Test.Hspec
import           Two

spec :: Spec
spec = describe "day 2" $ do
  it "adds" $ compute [1, 0, 0, 0, 99] `shouldBe` Right [2, 0, 0, 0, 99]
  it "multiples" $ compute [2, 3, 0, 3, 99] `shouldBe` Right [2, 3, 0, 6, 99]
  it "multiples different position"
    $          compute [2, 4, 4, 5, 99, 0]
    `shouldBe` Right [2, 4, 4, 5, 99, 9801]
  it "multiples another position"
    $          compute [1, 1, 1, 4, 99, 5, 6, 0, 99]
    `shouldBe` Right [30, 1, 1, 4, 2, 5, 6, 0, 99]
  it "compute the example"
    $          compute [1,9,10,3,2,3,11,0,99,30,40,50]
    `shouldBe` Right [3500,9,10,70, 2,3,11,0, 99, 30,40,50]
