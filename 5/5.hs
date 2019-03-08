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