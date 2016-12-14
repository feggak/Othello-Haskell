module OthelloUtils where

-- | updates the given list with the new value at the given index
(!!=) :: [a] -> (Int, a) -> [a]
list !!= (i,e) | i >= length list || (i < 0) = list
               | otherwise = take i list ++ [e] ++ drop (i+1) list
