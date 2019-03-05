compress :: (Eq a) => [a] -> [a]
compress lst = helper (head lst) lst
  where 
    helper prev [] = [prev]
    helper prev (x:xs) 
      | prev == x = helper prev xs
      | otherwise = [prev] ++ helper x xs 