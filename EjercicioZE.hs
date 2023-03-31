enteros :: [Int]
enteros = 0 : concat [[-x,x] | x <- [1..]]

posicion2 :: Int -> Int
posicion2 x = head [n | (n,y) <- zip [0..] enteros, y == x]