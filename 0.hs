removeAt :: [a] -> Int -> (a,[a])
removeAt l n = helper l n [] where
    y = length l
    helper l n acc 
      | y < n = (,l)
      | y == n = ((last l),(init l))
      | otherwise = .... NOT DONE