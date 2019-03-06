removeAt :: [a] -> Int -> (a,[a])
removeAt l n = (last taken,(init taken) ++ (drop n l)) where
    taken = take n l