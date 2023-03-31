primos :: Integral a => [a]
primos = criba [2..]
  where criba [] = []
        criba (n:ns) = n : criba (elimina n ns)
        elimina n xs = [x | x <- xs, x `mod` n /= 0]
        
primo :: Integral a => a -> Bool
primo x = x == head (dropWhile (<x) primos)
primoTruncable :: Int -> Bool

primoTruncable x
    | x < 10 = primo x
    | otherwise = primo x && primoTruncable (x `div` 10)