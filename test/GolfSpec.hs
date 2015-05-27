module GolfSpec where

import Test.Hspec
import Golf

spec :: Spec
spec = do
  describe "skips" $ do
    it "outputs the every nth element in the nth element" $ do
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
      skips [1] `shouldBe` [[1]]
