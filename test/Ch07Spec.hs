module Ch07Spec where

import Test.Hspec
import Ch07

spec :: Spec
spec = do
  describe "indexJ" $
    it "select the nth element" $ do
      indexJ 2 yeah `shouldBe` Just 'a'
      indexJ 5 yeah `shouldBe` Nothing
  describe "dropJ" $
    it "drops first n elements" $ do
      jlToList (dropJ 1 yeah) `shouldBe` "eah"
      jlToList (dropJ 4 yeah) `shouldBe` []
  describe "takeJ" $
    it "takes the first n elements" $ do
      jlToList (takeJ 1 yeah) `shouldBe` "y"
      jlToList (takeJ 3 yeah) `shouldBe` "yea"
