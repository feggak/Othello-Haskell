module OthelloUtils where

import OthelloTypes

-- | converts maybe disk to char
toChar :: Maybe Disk -> Char
toChar (Just Black) = '◯'
toChar (Just White) = '●'
toChar Nothing = '⋅'

-- | returns opposite color of given disk
otherDisk :: Disk -> Maybe Disk
otherDisk White = Just Black
otherDisk Black = Just White

-- | updates the given list with the new value at the given index
-- | from the Sudoku lab
(!!=) :: [a] -> (Int, a) -> [a]
list !!= (i,e) | i >= length list || (i < 0) = list
               | otherwise = take i list ++ [e] ++ drop (i+1) list
