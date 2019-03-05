dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:xs