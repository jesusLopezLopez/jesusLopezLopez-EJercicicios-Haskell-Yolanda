primos :: Integral a => [a]
primos = criba [2 ..]
  where
    criba [] = []
    criba (n : ns) = n : criba (elimina n ns)
    elimina n xs = [x | x <- xs, x `mod` n /= 0]

sumaPrimoMenores :: Int -> Int
sumaPrimoMenores n = sumaMenores n primos 0
  where
    sumaMenores n (x : xs) a
      | n <= x = a
      | otherwise = sumaMenores n xs (a + x)