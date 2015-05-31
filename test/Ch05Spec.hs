module Ch05Spec where

import Ch05
import ExprT
import Parser
import StackVM (stackVM, StackVal(..))

import Test.Hspec

spec :: Spec
spec = do
  describe "eval" $
    it "evals expression to integer value" $ do
      let expr = Mul (Add (Lit 2) (Lit 3)) (Lit 4)
      eval expr `shouldBe` 20
  describe "evalStr" $
    it "evals string representation of expressions" $ do
      let str = "(2 + 3) * 4"
      evalStr str `shouldBe` Just 20
  describe "Expr typeclass" $
    it "evals customized types" $ do
      let testExp :: Expr a => Maybe a
          testExp = parseExp lit add mul "(2 + 3) * 4"

          testInteger = testExp :: Maybe Integer
          testBool = testExp :: Maybe Bool
          testMM = testExp :: Maybe MinMax
          testSat = testExp :: Maybe Mod7

      testInteger `shouldBe` Just 20
      testBool `shouldBe` Just True
      testMM `shouldBe` Just (MinMax 3)
      testSat `shouldBe` Just (Mod7 6)

  describe "compiler" $
    it "compiles to stack program" $
      stackVM <$> compile "(2+3)*4" `shouldBe` Just (Right (IVal 20))
