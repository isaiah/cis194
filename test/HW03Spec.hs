module HW03Spec where

import Test.Hspec
import HW03

spec :: Spec
spec =
  describe "evalE" $
    it "evaluates the expression based the variables appear in the expression" $ do
      evalE empty (Val 5) `shouldBe` 5
      evalE empty (Op (Val 1) Eql (Val 2)) `shouldBe` 0
