collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n = n : collatz (siguiente n)
siguiente n | even n = n `div` 2
            | otherwise = 3*n+1
