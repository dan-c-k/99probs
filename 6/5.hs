data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )

countDescendants :: Tree a -> Int
countDescendants Empty = 0
countDescendants (Branch a b1 b2) = 1 + countDescendants b1 + countDescendants b2

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
        
