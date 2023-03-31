agrupa' :: Int -> [a] -> [[a]]
agrupa' n = takeWhile (not . null)
           . map (take n)
           . iterate (drop n)
  where
    takeWhile' _ [] = []
    takeWhile' p (x:xs)
      | p x = x : takeWhile' p xs
      | otherwise = []
