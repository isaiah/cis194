import Ch01
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "toDigits" $ do
    it "converts integer to digits" $ do
      let int = 1234
          expected = [4,3,2,1]
      toDigitsRev int `shouldBe` expected
      toDigits int `shouldBe` (reverse expected)
    it "returns [] for 0 and nagative numbers" $ do
      toDigits 0 `shouldBe` []
      toDigits (-17) `shouldBe` []

  describe "doubleEveryOther" $ do
    it "doubles every number _beginning from the right_" $ do
      let ints = [8, 7, 6, 5]
      doubleEveryOther [8, 7, 6, 5] `shouldBe` [16, 7, 12, 5]
      doubleEveryOther [1, 2, 3] `shouldBe` [1, 4, 3]
  describe "sumDigits" $ do
    it "calculates the sum of all digits" $ do
      let digits = [16, 7, 12, 5]
          expected = 22
      sumDigits digits `shouldBe` expected

  describe "validate" $ do
    it "validates credit card number" $ do
      let ccValid = 4012888888881881
          ccInvalid = 4012888888881882
      validate ccValid `shouldBe` True
      validate ccInvalid `shouldBe` False
