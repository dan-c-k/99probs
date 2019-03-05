dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:xs

repli :: [a] -> Int -> [a]
repli [] _ = []
repli lox@(x:xs) n = (map (\y -> x) [1..n]) ++ (repli xs n)