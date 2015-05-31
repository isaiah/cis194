module Ch06Spec where

import Test.Hspec
import Ch06

spec :: Spec
spec =
  describe "fibs1" $
    it "returns all the fibonacci numbers" $
      take 15 fibs1 `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]
