triangulares :: [Integer]
triangulares = 1 : [x + y | (x, y) <- zip [2 ..] triangulares]

triangulares' :: [Integer]
triangulares' = scanl (+) 1 [2 ..]

divisores :: Integer -> [Integer]
divisores x = [y | y <- [1 .. x], mod x y == 0]

nDivisores :: Integer -> Int
nDivisores x = length (divisores x)