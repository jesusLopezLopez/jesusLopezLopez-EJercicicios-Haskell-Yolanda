multiplosRestringidos :: Int -> (Int -> Bool) -> [Int]
multiplosRestringidos n p = [y | y <- [n,2*n..], all p (digitos y)]
digitos :: Int -> [Int]
digitos n = [read [x] | x <- show n]
