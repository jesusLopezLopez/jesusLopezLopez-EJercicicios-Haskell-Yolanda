primos :: Integral a => [a]
primos = criba [2..]
     where criba [] = []
           criba (n:ns) = n : criba (elimina n ns)
           elimina n xs = [x | x <- xs, x `mod` n /= 0]
sumaDeDosPrimos :: Int -> [(Int,Int)]
sumaDeDosPrimos n =
    [(x,n-x) | x <- primosN, x < n-x, elem (n-x) primosN]
    where primosN = takeWhile (<=n) primos
