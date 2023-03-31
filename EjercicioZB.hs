enteros' :: [Int]
enteros' = iterate siguiente 0
    where siguiente x  | x >= 0 = -x-1| otherwise = -x