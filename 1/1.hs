pack :: (Eq a) => [a] -> [[a]]
pack lst = helper [(head lst)] lst 
  where
    helper prev [] = [prev]
    helper (y:ys) (x:xs) 
      | y == x = helper (x:y:ys) xs
      | otherwise = [y:ys] ++ helper [x] xs 

data Encoding a = Multiple a Int | Single a deriving(Eq,Show)

encodeModified :: (Eq a) => [a] -> [Encoding a]
encodeModified lst = map helper (pack lst) where
    helper (x:[]) = Single x
    helper lox@(x:xs) = Multiple x $ length lox