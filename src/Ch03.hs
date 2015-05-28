module Ch03 where

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
