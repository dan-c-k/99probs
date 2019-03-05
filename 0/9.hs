pack :: (Eq a) => [a] -> [[a]]
pack lst = helper [(head lst)] lst 
  where
    helper prev [] = [prev]
    helper (y:ys) (x:xs) 
      | y == x = helper (x:y:ys) xs
      | otherwise = [y:ys] ++ helper [x] xs 
