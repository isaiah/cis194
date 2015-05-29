module Ch04 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-- Exercise 1: Wholemeal programming
fun1' :: [Integer] -> Integer
fun1' = product . filter even . map (\x -> x - 2)

-- DAMN this problem :D
fun2' :: Integer -> Integer
fun2' = undefined

-- Exercise 2: Folding with trees
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
              deriving (Show, Eq)
foldTree :: [a] -> Tree a
foldTree l =
  go l Leaf
  where
    go :: [a] -> Tree a -> Tree a
    go [] tree = tree
    go (x:xs) tree =
      case tree of
        Leaf -> Node 1 Leaf x Leaf
        Node 
