insertAt :: a -> [a] -> Int -> [a]
insertAt x l 1 = x:l
insertAt x [] n = [x]
insertAt x (x1:xs) n = x1:(insertAt x xs (n-1))