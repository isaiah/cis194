module Ch05Spec where

import Ch05 hiding (Lit, Add, Mul)
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

  describe "HasVars" $
    it "could store and interpret variables" $ do
      let env = [("x", 6)]
          expr1 = add (lit 3) (var "x")
          expr2 = add (lit 3) (var "y")
          expr3 = mul (var "x") (add (var "y") (var "x"))
      withVars env expr1 `shouldBe` Just 9
      withVars env expr2 `shouldBe` Nothing
      withVars [("x", 6), ("y", 3)] expr3 `shouldBe` Just 54
