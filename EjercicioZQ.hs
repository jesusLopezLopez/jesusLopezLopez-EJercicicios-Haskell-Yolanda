perteneceRango :: Int -> (Int -> Int) -> Bool
perteneceRango y f = y `elem` takeWhile (<=y) (imagenes f)
    where imagenes f = [f x | x <- [0..]]