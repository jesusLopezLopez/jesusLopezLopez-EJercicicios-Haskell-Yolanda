subSucGolomb :: Int -> [Int]
subSucGolomb 1 = [1] ++ subSucGolomb 2
subSucGolomb 2 = [2,2] ++ subSucGolomb 3
subSucGolomb x = (replicate (golomb x) x) ++ subSucGolomb (x+1)

sucGolomb :: [Int]
sucGolomb = subSucGolomb 1

golomb :: Int -> Int
golomb n = sucGolomb !! (n-1)

