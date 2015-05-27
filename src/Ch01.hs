module Ch01 (
  toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
  , hanoi
  ) where

toDigitsRev :: Integer -> [Integer]
toDigitsRev ints
  | ints <= 0 = []
  | otherwise = ints `mod` 10 : toDigitsRev (ints `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther arr =
  reverse $ double' $ reverse arr
  where
    double' ints =
      case ints of
        x:y:t -> x : 2 * y : double' t
        [] -> []
        a -> a

sumDigits :: [Integer] -> Integer
sumDigits ints =
  sum $ concatMap toDigits ints

validate :: Integer -> Bool
validate int =
  (sumDigits . doubleEveryOther . toDigits) int `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c =
  go n a b c []
  where
    go n x y temp ret
      | n <= 0 = ret
      | otherwise = hanoi (n-1) x temp y ++ ((x, y) : hanoi (n - 1) temp y x)
