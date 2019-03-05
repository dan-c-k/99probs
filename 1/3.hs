data Encoding a = Multiple Int a | Single a deriving(Eq,Show)

encodeDirect :: (Eq a) => [a] -> [Encoding a]
encodeDirect lst = foldr helper [] lst where
    helper curr [] = [(Single curr)]
    helper curr ((Single a):xs) 
      | curr == a = (Multiple 2 a): xs
      | otherwise = (Single curr):(Single a):xs
    helper curr ((Multiple n a):xs)
      | curr == a = (Multiple (n+1) a): xs
      | otherwise = (Single curr):(Multiple n a):xs