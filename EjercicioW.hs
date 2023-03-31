esMuyCompuesto :: Int -> Bool
esMuyCompuesto x =
    and [numeroDivisores y < n | y <- [1..x-1]]
    where n = numeroDivisores x

muyCompuesto :: Int -> Int
muyCompuesto n =
    [x | x <- [1..], esMuyCompuesto x] !! n

numeroDivisores :: Int -> Int
numeroDivisores = length . divisores

divisores :: Int -> [Int]
divisores x = [y | y <- [1..x], mod x y == 0]