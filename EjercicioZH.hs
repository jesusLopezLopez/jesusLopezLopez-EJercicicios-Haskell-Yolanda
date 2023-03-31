divisoresEn :: Int -> [Int] -> Bool
divisoresEn 1 _                     = True
divisoresEn x []                    = False
divisoresEn x (y:ys) | mod x y == 0 = divisoresEn (div x y) (y:ys)
                     | otherwise    = divisoresEn x ys