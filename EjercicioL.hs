import Test.QuickCheck
prop_AgrupaLongitud :: Int -> [Int] -> Property
prop_AgrupaLongitud n xs =
  n > 0 && not (null gs) ==>
    and [length g == n | g <- init gs] &&
    0 < length (last gs) && length (last gs) <= n
  where gs = agrupa n xs

prop_AgrupaCombina :: Int -> [Int] -> Property
prop_AgrupaCombina n xs =
  n > 0 ==> concat (agrupa n xs) == xs

agrupa :: Int -> [a] -> [[a]]
agrupa n [] = []
agrupa n xs = take n xs : agrupa n (drop n xs)
