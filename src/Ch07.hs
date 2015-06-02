{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

module Ch07 where

import           Buffer
import           Data.Char
import           JoinList
import           Sized

-- Exercise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl jl' = Append (mappend (tag jl) (tag jl')) jl jl'

tag :: (Monoid m) => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- Exercise 2
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

tagSize :: (Sized b, Monoid b) => JoinList b a -> Int
tagSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i (Append _ l1 l2)
  | i >= tagSize l1 = indexJ (i - tagSize l1) l2
  | otherwise = indexJ i l1

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i jl | i <= 0 = jl
dropJ i (Single _ _) | i > 0 = Empty
dropJ i (Append _ l1 l2)
  | i >= tagSize l1 = dropJ (i - tagSize l1) l2
  | otherwise = dropJ i l1 +++ l2

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i _ | i <= 0 = Empty
takeJ i jl | i >= tagSize jl = jl
takeJ i (Append _ l1 l2)
  | i >= tagSize l1 = l1 +++ takeJ (i - tagSize l1) l2
  | otherwise = takeJ i l1

-- Handy JoinList for demo purpose
yeah :: JoinList Size Char
yeah = ms 'y' +++ ms 'e' +++ ms 'a' +++ ms 'h'
  where
    s = Size 1
    ms = Single s

-- Exercise 3
newtype Score = Score Int
                deriving (Show, Eq, Ord, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score a =
  case toLower a of
    'a' -> Score 1
    'b' -> Score 3
    'c' -> Score 3
    'd' -> Score 2
    'e' -> Score 1
    'f' -> Score 4
    'g' -> Score 2
    'h' -> Score 4
    'i' -> Score 1
    'j' -> Score 8
    'k' -> Score 5
    'l' -> Score 1
    'm' -> Score 3
    'n' -> Score 1
    'o' -> Score 1
    'p' -> Score 3
    'q' -> Score 10
    'r' -> Score 1
    's' -> Score 1
    't' -> Score 1
    'u' -> Score 1
    'v' -> Score 4
    'w' -> Score 4
    'x' -> Score 8
    'y' -> Score 4
    'z' -> Score 10
    _ -> Score 0

scoreString :: String -> Score
scoreString = foldr (mappend . score) (Score 0)

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

-- Exercise 4
instance Buffer (JoinList (Score, Size) String) where
  toString :: JoinList (Score, Size) String -> String
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ l1 l2) =
    toString l1 ++ toString l2

  fromString :: String -> JoinList (Score, Size) String
  fromString s = Single (scoreString s, Size 1) s

  line :: Int -> JoinList (Score, Size) String -> Maybe String
  line = indexJ

  replaceLine :: Int -> String -> JoinList (Score, Size) String -> JoinList (Score, Size) String
  replaceLine _ string Empty = fromString string
  replaceLine 0 string (Single _ _) = fromString string
  replaceLine i string (Append _ l1 l2)
    | i >= tagSize l1 = l1 +++ replaceLine (i - tagSize l1) string l1 +++ l2
    | otherwise = replaceLine i string l1 +++ l2
  numLines Empty = 0
  numLines (Single _ _) =  1
  numLines (Append (Score _, Size x) _ _) = x
  value Empty = 0
  value (Single (Score x, Size _) _) = x
  value (Append (Score x, Size _) _ _) = x
