paresOrdenados :: [a] -> [(a,a)]
paresOrdenados [] = []
paresOrdenados (x:xs) = [(x,y) | y <- xs] ++ paresOrdenados xs

sumaDeDos' :: Int -> [Int] -> Maybe (Int,Int)
sumaDeDos' x xs
    | null ys = Nothing
    | otherwise = Just (head ys)
    where ys = [(a,b) | (a,b) <- paresOrdenados xs , a+b == x]