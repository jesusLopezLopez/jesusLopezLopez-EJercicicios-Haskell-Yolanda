ecoR :: [a] -> [a]
ecoR xs = aux 1 xs
  where
    aux n [] = []
    aux n (x:xs) = replicate n x ++ aux (n+1) xs
