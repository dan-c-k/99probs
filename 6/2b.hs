data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)


atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch a _ _) 1 = [a]
atLevel (Branch a b1 b2) n = atLevel b1 (n-1) ++ atLevel b2 (n-1)