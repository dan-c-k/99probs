import Data.List (sortBy)

sortTuple :: (Int,[a]) -> (Int,[a]) -> Ordering
sortTuple (n1,as1) (n2,as2)
  | n1 < n2 = LT 
  | n1 == n2 = EQ
  | otherwise = GT

lsort :: [[a]] -> [[a]]
lsort l = map (\(n,as) -> as) $ sortBy sortTuple $ map (\x -> (length x,x)) l