data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

maxDepth :: Tree a -> Int
maxDepth Empty = 0
maxDepth (Branch a Empty Empty) = 1
maxDepth (Branch a b1 b2) = 1 + max (maxDepth b1) (maxDepth b2)

layout :: Tree a -> [(a,(Int,Int))]
layout t = helper t md ((2^md)-1) 1 where
    md = (maxDepth t) - 1
    helper Empty _ _ _ = []
    helper (Branch a Empty Empty) _ val cd = [(a,(val,cd))]
    helper (Branch a b1 b2) md val cd =
        (a,(val,cd)):((helper b1 md (val-nextVal) (cd+1))++(helper b2 md (val+nextVal) (cd+1))) where
            nextVal = 2^(md-cd) 
        
