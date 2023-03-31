primos :: Integral a => [a]
primos = criba [2..]
  where criba [] = []
        criba (n:ns) = n : criba (elimina n ns)
        elimina n xs = [x | x <- xs, x `mod` n /= 0]
        
primo :: Integral a => a -> Bool
primo x = x == head (dropWhile (<x) primos)


primoPermutable :: Int -> Bool
primoPermutable x = and [primo y | y <- permutacionesN x]

permutacionesN :: Int -> [Int]
permutacionesN x = [read ys | ys <- permutaciones (show x)]

intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : [y:zs | zs <- intercala x ys]

permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones (x:xs) =
    concat [intercala x ys | ys <- permutaciones xs]