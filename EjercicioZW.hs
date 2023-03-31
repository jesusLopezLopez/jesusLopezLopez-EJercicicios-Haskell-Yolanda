paresOrdenados :: [a] -> [(a,a)]
paresOrdenados [] = []
paresOrdenados (x:xs) = [(x,y) | y <- xs] ++ paresOrdenados xs

sumaDeDos :: Int -> [Int] -> Maybe (Int,Int)
sumaDeDos _ [] = Nothing
sumaDeDos _ [_] = Nothing
sumaDeDos y (x:xs) | y-x `elem` xs = Just (x,y-x)
                   | otherwise = sumaDeDos y xs