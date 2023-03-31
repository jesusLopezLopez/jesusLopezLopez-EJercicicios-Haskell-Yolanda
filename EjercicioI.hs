itera :: (a -> a) -> a -> [a]
itera f x = x : itera f (f x)
