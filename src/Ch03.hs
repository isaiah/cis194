module Ch03 where

import Data.List (sortBy, group, sort, intersperse)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- Exercise 1 Hopscotch
splitBy :: Int -> [a] -> [[a]]
splitBy _ [] = []
splitBy index list
  | index <= 0 || length list < index = [list]
  | otherwise =
    let (h', t') = splitAt index list in
        h' : splitBy index t'

skips :: [a] -> [[a]]
skips [] = []
skips l =
  l : go l 2
  where
    go l' depth
      | depth > length l = []
      | otherwise =
        let (h:t) = l' in
            (map head $ splitBy depth t) : go t (depth + 1)

-- Exercise 2 Local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima l@(a:b:c:_) =
    if b > a && b > c
    then b : localMaxima (tail l)
    else localMaxima (tail l)
localMaxima _ = []

-- Exercise 3 Histogram
-- XXX not finished
histogram :: [Int] -> String
histogram l =
  printHistoGram finalOcc ++ foot
  where
    occ = map (\l'@(x:_) -> (x, length l')) . group . sort $ l
    defaultOcc = zip [0..9] $ repeat 0
    finalOcc = IntMap.map (\v -> replicate v '*') $
        IntMap.union (IntMap.fromList occ) (IntMap.fromList defaultOcc)
    foot = "\n==========\n0123456789\n"
    printHistoGram dotMap =
      concat . (intersperse "\n") $ map (\(_,a) -> a) (IntMap.toAscList dotMap)


