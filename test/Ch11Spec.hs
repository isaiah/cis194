module Ch11Spec where

import           AParser
import           Ch11
import           Data.Char
import           Test.Hspec

spec :: Spec
spec = do
  describe "zeroOrMore" $ do
    it "parses a sequence with parser" $ do
      runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Just ("", "abcdeFGh")
  describe "oneOrMore" $ do
    it "parses at least one" $ do
      runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Nothing
      runParser (oneOrMore (satisfy isUpper)) "ABcdeFGh" `shouldBe` Just ("AB", "cdeFGh")

  describe "ident" $ do
    it "passes identifiers" $ do
      runParser ident "foobar baz" `shouldBe` Just ("foobar", " baz")
      runParser ident "foo33fA" `shouldBe` Just ("foo33fA", "")
      runParser ident "2bad" `shouldBe` Nothing
      runParser ident "" `shouldBe` Nothing

  describe "parseSExpr" $ do
    it "parses S-Exp" $ do
      runParser parseSExpr "foo3" `shouldBe` Just (A (I "foo3"), "")
      runParser parseSExpr "123" `shouldBe` Just (A (N 123), "")
