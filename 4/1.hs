isPrime :: Int -> Bool
isPrime n = helper 2 n where
  helper m n
    | n == m = True
    | (rem n m) == 0 = False
    | otherwise = helper (m+1) n

goldbach :: Int -> (Int,Int)
goldbach n = head [ (x,y) | x <- [2..n], y <- [2..n], isPrime x, isPrime y, x+y == n]

goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList n m = map goldbach [x | x <- [n..m], even x]