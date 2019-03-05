dropEvery :: [a] -> Int -> [a]
dropEvery lst n = concat (map init (helper lst)) where
    helper [] = []
    helper lst = (take n lst):(helper (drop n lst ))