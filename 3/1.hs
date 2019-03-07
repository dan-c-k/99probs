isPrime :: Int -> Bool
isPrime n = helper 2 n where
  helper m n
    | n == m = True
    | (rem n m) == 0 = False
    | otherwise = helper (m+1) n