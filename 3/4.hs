coprime :: Int -> Int -> Bool
coprime a b = 1 == gcd a b

totient :: Int -> Int
totient a = length [ x | x <- [1..a], coprime x a]
