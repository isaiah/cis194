module Ch06 where

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [fib x | x <- [0..]]

-- Exercise 2

-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a s) =
  a : streamToList s

instance Show a => Show (Stream a) where
  show s =
    go s 0
    where
      go :: (Show a) => Stream a -> Integer -> String
      go (Cons x stream) depth
        | depth > 20 = "..."
        | otherwise = show x ++ ", " ++ go stream (depth + 1)

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) =
  Cons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a =
  Cons a $ streamFromSeed f (f a)

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = Cons 0 (interleaveStreams
                  (interleaveStreams  (streamRepeat 1)
                                      (streamFromSeed (+1) 2))
                  (streamRepeat 0))
        where
            interleaveStreams (Cons a1 s1) (Cons a2 s2) =
                Cons a1 (Cons a2 (interleaveStreams s1 s2))
