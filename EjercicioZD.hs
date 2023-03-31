enteros :: [Int]
enteros = 0 : concat [[-x,x] | x <- [1..]]

posicion1 :: Int -> Int
posicion1 x = aux enteros 0
    where aux (y:ys) n | x == y = n| otherwise = aux ys (n+1)