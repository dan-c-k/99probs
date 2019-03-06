import System.Random
import Data.List

rnd_select :: (Eq a) => [a] -> Int -> IO [a]
rnd_select l 0 = return []
rnd_select l n = 
    do r <- getStdRandom (randomR (0,(length l)-1))
       as <- rnd_select (delete (l!!r) l) (n-1)
       return ((l!!r):as)

rnd_permu ::(Eq a) => [a] -> IO [a]
rnd_permu l = rnd_select l $ length l