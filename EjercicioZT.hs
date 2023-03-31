paresOrdenados3 :: [a] -> [(a,a)]
paresOrdenados3 [] = []
paresOrdenados3 (x:xs) = zip (repeat x) xs ++ paresOrdenados3 xs