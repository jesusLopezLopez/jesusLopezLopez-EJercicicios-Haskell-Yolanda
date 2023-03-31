enteros :: [Int]
enteros = 0 : concat [[-x,x] | x <- [1..]]