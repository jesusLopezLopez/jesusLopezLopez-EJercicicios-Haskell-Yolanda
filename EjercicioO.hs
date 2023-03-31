siguiente n | even n = n `div` 2
            | otherwise = 3*n+1
collatz' :: Integer -> [Integer]
collatz' n = (takeWhile (/=1) (iterate siguiente n)) ++ [1]
