import Data.List

combinations :: [a] -> Int -> [[a]]
combinations [] n = []
combinations l 1 = foldr (\x -> [x]) [] l
combinations l n = map (\x -> combinations (delete x l) (n-1)) l

-- this is not done
-- this is going to have order matter

-- say i've chosen 1 then its the combinations of the remaining minus 1

-- 1 2 3

-- 12

-- 23

-- 13

-- ----
-- 1234 choose 2

-- a combination is just the presence or absence of an item
-- the combination of 1 2  corresponds to [T T F]
--                                        [T F T]
--                                        []