paresOrdenados :: [a] -> [(a,a)]
paresOrdenados [] = []
paresOrdenados (x:xs) = [(x,y) | y <- xs] ++ paresOrdenados xs