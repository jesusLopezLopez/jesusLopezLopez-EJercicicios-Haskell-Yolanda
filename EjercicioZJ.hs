divisoresEn :: Int -> [Int] -> Bool
divisoresEn 1 _                     = True
divisoresEn x []                    = False
divisoresEn x (y:ys) | mod x y == 0 = divisoresEn (div x y) (y:ys)
                     | otherwise    = divisoresEn x ys

hamming' :: [Int]
hamming' = [x | x <- [1..], divisoresEn x [2,3,5]]

cantidadHammingMenores :: Int -> Int
cantidadHammingMenores x = length (takeWhile (<x) hamming')