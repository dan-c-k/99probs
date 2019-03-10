data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)


internals :: Tree a -> [a]
internals  Empty = []
internals (Branch a Empty Empty) = []
internals (Branch a b1 b2) = a:(internals b1 ++ internals b2)