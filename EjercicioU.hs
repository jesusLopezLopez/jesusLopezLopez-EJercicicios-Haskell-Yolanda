primos :: Integral a => [a]
primos = criba [2..]
     where criba [] = []
           criba (n:ns) = n : criba (elimina n ns)
           elimina n xs = [x | x <- xs, x `mod` n /= 0]
esProductoDeDosPrimos :: Int -> Bool
esProductoDeDosPrimos n =
    [x | x <- primosN,
    mod n x == 0,
    div n x /= x,
    elem (div n x) primosN] /= []
    where primosN = takeWhile (<=n) primos