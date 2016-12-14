module OthelloUtils where

import OthelloTypes

-- | updates the given list with the new value at the given index
-- | from the Sudoku lab
(!!=) :: [a] -> (Int, a) -> [a]
list !!= (i,e) | i >= length list || (i < 0) = list
               | otherwise = take i list ++ [e] ++ drop (i+1) list
