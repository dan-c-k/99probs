myGCD :: Int -> Int -> Int
myGCD a b 
    | r == 0 = b
    | otherwise = myGCD b r where
        r = (rem a b)