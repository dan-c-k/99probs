data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )

countDescendants :: Tree a -> Int
countDescendants Empty = 0
countDescendants (Branch a b1 b2) = 1 + countDescendants b1 + countDescendants b2

layout :: Tree a -> [(a,(Int,Int))]
layout t = helper t 0 1 where
    helper Empty pld depth = []
    helper (Branch a Empty Empty) pld depth  = [(a,(pld,depth))]
    helper (Branch a b1 b2) pld depth =  (a,(des,depth)):((helper b1 pld (depth + 1))++(helper b2 des (depth + 1))) where
        des = countDescendants b1 + 1