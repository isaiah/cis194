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
