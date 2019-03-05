split :: [a] -> Int -> [[a]]
split l n 
  | length l < n = [l]
  | otherwise = [(take n l),(drop n l)]