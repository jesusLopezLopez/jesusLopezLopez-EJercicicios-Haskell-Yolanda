primosEquivalentes :: Int -> [[Integer]]
primosEquivalentes n = aux primos
  where
    aux (x : xs)
      | relacionados equivalentes ys = ys : aux xs
      | otherwise = aux xs
      where
        ys = take n (x : xs)

    primos = criba [2 ..]

    criba (n : ns) = n : criba (elimina n ns)
    elimina n xs = [x | x <- xs, x `mod` n /= 0]

digitosC :: Integer -> [Integer]
digitosC n = [read [x] | x <- show n]

media :: [Integer] -> Float
media xs = (fromIntegral (sum xs)) / (fromIntegral (length xs))

equivalentes :: Integer -> Integer -> Bool
equivalentes x y = media (digitosC x) == media (digitosC y)

relacionados :: (a -> a -> Bool) -> [a] -> Bool
relacionados r (x : y : zs) = (r x y) && relacionados r (y : zs)
relacionados _ _ = True