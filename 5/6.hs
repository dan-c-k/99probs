data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

mirror2 :: (Eq a) => Tree a -> Tree a -> Bool
mirror2 (Empty) (Empty) = True
mirror2 (Empty) _ = False
mirror2 _ (Empty) = False
mirror2 (Branch x b1a b2a) (Branch y b1b b2b) = (mirror2 b1a b2b) && (mirror2 b1b b2a)

symmetric :: (Eq a) => Tree a -> Bool
symmetric (Empty) = True
symmetric (Branch a b1 b2) = mirror2 b1 b2