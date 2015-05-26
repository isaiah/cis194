module Ch01 (
  toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
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
