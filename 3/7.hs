primeFactors :: Int -> [Int]
primeFactors n = helper n 2 where
    helper n m 
        | n == m = [m]
        | r == 0 = m:helper q m
        | otherwise = helper n (m + 1) where
            (q,r) = divMod n m

primeFactorsMult :: Int -> [(Int,Int)]
primeFactorsMult n = foldr helper [] $ primeFactors n where 
    helper x [] = [(x,1)]
    helper x l@((f,m):xs)
        | x == f = ((f,m+1)):xs
        | otherwise = ((x,1)):l

totientFast :: Int -> Int
totientFast n = product [ (p-1)*(p^(m-1)) | (p,m) <- (primeFactorsMult n)]