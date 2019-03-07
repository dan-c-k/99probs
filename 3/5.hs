primeFactors :: Int -> [Int]
primeFactors n = helper n 2 where
    helper n m 
        | n == m = [m]
        | r == 0 = m:helper q m
        | otherwise = helper n (m + 1) where
            (q,r) = divMod n m
