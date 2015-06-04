module Ch10Spec where

import Ch10
import Test.Hspec

spec :: Spec
spec = do
  describe "abParser" $ do
    it "parses string starts with 'ab'" $ do
      runParser abParser "abcd" `shouldBe` Just (('a', 'b'), "cd")
      runParser abParser_ "abcd" `shouldBe` Just ((), "cd")
  describe "intPair" $ do
    it "parses a pair of integers separated by space" $ do
      runParser intPair "12 34" `shouldBe` Just ([12, 34], "")

  describe "intOrUppercase" $ do
    it "parses a integer value or a uppercase character" $ do
      runParser intOrUppercase "342abcd" `shouldBe` Just ((), "abcd")
      runParser intOrUppercase "XYZ" `shouldBe` Just ((), "YZ")
      runParser intOrUppercase "foo" `shouldBe` Nothing
