data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

construct :: [Int] -> Tree Int
construct [] = Empty
construct l = foldr helper Empty (reverse l) where
    helper x (Empty) = Branch x Empty Empty
    helper x (Branch n b1 b2) 
        | x < n = Branch n (helper x b1) b2
        | x > n = Branch n b1 (helper x b2)
        | otherwise = (Branch n b1 b2)

