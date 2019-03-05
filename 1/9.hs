rotate :: [a] -> Int -> [a]
rotate l@(x:xs) n 
  | n < 0 = rotate (reverse l) (n * (-1))
  | n == 0 = l
  | otherwise = rotate (xs ++ [x]) (n-1)