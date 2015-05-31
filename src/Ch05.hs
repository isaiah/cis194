{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Ch05 where

import           ExprT   (ExprT)
import qualified ExprT   as ET
import           Parser
import           StackVM

-- Exercise 1
eval :: ExprT -> Integer
eval expr =
  case expr of
    ET.Lit v -> v
    ET.Add a b -> eval a + eval b
    ET.Mul a b -> eval a * eval b

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr str =
  case parseExp ET.Lit ET.Add ET.Mul str of
    Just expr ->
      Just $ eval expr
    Nothing -> Nothing
-- Exercise 3
class Expr a where
  mul :: Expr a => a -> a -> a
  add :: Expr a => a -> a -> a
  lit :: Expr a => Integer -> a

instance Expr ExprT where
  mul = ET.Mul
  add = ET.Add
  lit = ET.Lit

reify :: ExprT -> ExprT
reify = id


-- Exercise 4
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  mul = (*)
  add = (+)
  lit = id

instance Expr Bool where
  lit a = a > 0
  mul = (&&)
  add = (||)

instance Expr MinMax where
  mul (MinMax a) (MinMax b) = MinMax $ min a b
  add (MinMax a) (MinMax b) = MinMax $ max a b
  lit = MinMax

instance Expr Mod7 where
  lit = Mod7
  mul (Mod7 a) (Mod7 b) = Mod7 $ a * b `mod` 7
  add (Mod7 a) (Mod7 b) = Mod7 $ a + b `mod` 7

-- Exercise 5
instance Expr Program where
  lit a = [PushI a]
  mul a b = a ++ b ++ [Mul]
  add a b = a ++ b ++ [Add]

compile ::  String -> Maybe Program
compile =
  parseExp lit add mul

-- Exercise 6
