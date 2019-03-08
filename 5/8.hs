data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf :: a -> Tree a             
leaf x = Branch x Empty Empty

cbalTree :: Int -> [Tree Int]
cbalTree 0 = [Empty]
cbalTree 1 = [leaf 1]
cbalTree n = [Branch n x y| x <- cbalTree q, y <- cbalTree q'] ++ [Branch n x y| x <- cbalTree q', y <- cbalTree q]  where  
    q = quot (n-1) 2
    q' = n - q - 1

mirror2 :: (Eq a) => Tree a -> Tree a -> Bool
mirror2 (Empty) (Empty) = True
mirror2 (Empty) _ = False
mirror2 _ (Empty) = False
mirror2 (Branch x b1a b2a) (Branch y b1b b2b) = (mirror2 b1a b2b) && (mirror2 b1b b2a)

symmetric :: (Eq a) => Tree a -> Bool
symmetric (Empty) = True
symmetric (Branch a b1 b2) = mirror2 b1 b2

symCbalTrees :: Int -> [Tree Int]
symCbalTrees n = [t | t <- cbalTree n, symmetric t]