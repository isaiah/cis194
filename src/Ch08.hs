{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ch08 where

import           Data.Tree (Tree (..))
import           Employee

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e@Emp{empFun} (GL gl f) =
  GL (e:gl) (empFun + f)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL l1 f1) (GL l2 f2) =
    GL (l1 ++ l2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f1 >= f2 = gl1
  | otherwise = gl2

-- Exercise 2
treeFold :: Monoid b => (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a []) = f a []
treeFold f (Node a subForest) = f a $ map (treeFold f) subForest

combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs e@Emp{..} (x:xs) = undefined

-- Exercise 3
-- The first Guest list is the one with the employee, the the second is the one without
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp@Emp { .. } [] = (GL [emp] empFun, mempty)
nextLevel emp xs =
  (glCons emp wo, w)
  where
  w = mconcat (map fst xs)
  wo = mconcat (map snd xs)

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun employees =
  uncurry moreFun $ treeFold nextLevel employees


main :: IO ()
main = do
  company <- readFile "data/company.txt"
  let guestList = maxFun $ read company
  print . fun $ guestList
  putStrLn $ unlines (map empName (employees guestList))
  where
    fun (GL _ f) = f
    employees (GL e _) = e
