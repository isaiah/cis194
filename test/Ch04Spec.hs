module Ch04Spec where

import Test.Hspec
import Ch04

spec :: Spec
spec = do
  describe "fun1'" $ do
    it "do the same as fun1" $ do
      let a = [3..10]
      fun1' a `shouldBe` fun1 a

  describe "xor" $ do
    it "returns True only if there are an odd number of True" $ do
      xor [False, True, False] `shouldBe` True
      xor [False, True, False, False, True] `shouldBe` False
