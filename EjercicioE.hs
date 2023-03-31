ecoC :: [a] -> [a]
ecoC xs = concat [replicate i x | (i,x) <- zip [1..] xs]
