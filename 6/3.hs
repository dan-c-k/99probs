data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

completeBinaryTree :: Int -> Tree Int
completeBinaryTree 0 = Empty
completeBinaryTree 1 = Branch 1 Empty Empty
completeBinaryTree n = Branch n (completeBinaryTree q') (completeBinaryTree q) where
    q = quot (n-1) 2
    q' = n - q - 1