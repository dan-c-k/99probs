slice :: [a] -> Int -> Int -> [a]
slice l n m = (take (m-n) (drop n l))