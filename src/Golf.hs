module Golf where

skips :: [a] -> [[a]]
skips ll =
  go ll 0
  where
    go l depth ret
      | depth == length l = []
      | otherwise = map head $ splitBy depth l : go l (depth + 1)
    splitBy index list =
      let (h', t') = splitAt index list in
        h' : splitBy index t'
