data Encoding a = Multiple Int a | Single a deriving(Eq,Show)

decodeModified :: (Eq a) => [Encoding a] -> [a]
decodeModified lst = foldr helper [] lst where
    helper (Single a) acc  = a: acc
    helper (Multiple n a) acc = (map (\x -> a) [1..n]) ++ acc