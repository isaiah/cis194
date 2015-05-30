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

height :: Tree a -> Integer
height Leaf = 0
height (Node _ l _ r) = 1 + max (height l) (height r)

foldTree :: [a] -> Tree a
foldTree =
  foldr insert Leaf
  where
    insert :: a -> Tree a -> Tree a
    insert x Leaf = Node 0 Leaf x Leaf
    insert x (Node _ l v r)
          | height l <= height r =
            let left = insert x l in
                Node (height left) left v r
          | otherwise =
                Node (height l) l v (insert x r)

xor :: [Bool] -> Bool
xor =
  foldl go False
  where
    go True True = False
    go _ _ = True

map' :: (a -> b) -> [a] -> [b]
map' f =
  foldr go []
  where
    go a l =
      f a : l
