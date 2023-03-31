enteros :: [Int]
enteros = 0 : concat [[-x,x] | x <- [1..]]

posicion :: Int -> Int
posicion x = length (takeWhile (/=x) enteros)