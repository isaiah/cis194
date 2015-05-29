module Ch03Spec where

import Test.Hspec
import Ch03

spec :: Spec
spec = do
  describe "skips" $ do
    it "outputs the every nth element in the nth element" $ do
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
      skips [1] `shouldBe` [[1]]
      skips [True, False] `shouldBe` [[True, False], [False]]
      skips ([]::[Int]) `shouldBe` []

  describe "localMaxima" $ do
    it "finds all the local maxima in the input list and returns them in order" $ do
      localMaxima [2,9,5,6,1] `shouldBe` [9,6]
      localMaxima [2,3,4,1,5] `shouldBe` [4]
      localMaxima [1..5] `shouldBe` []

  describe "histogram" $ do
    it "generates string representation of the histogram" $ do
      histogram [3,5] `shouldBe` " * * \n==========\n0123456789\n"
