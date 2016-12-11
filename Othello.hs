module Othello where

import Data.Maybe

type Pos = (Int, Int)
type Board = [[Maybe Disk]]
data Disk = White | Black deriving (Show, Eq)

-- | creates a blank board
blankBoard :: Int -> Board
blankBoard n | even n && n > 2 = replicate n (replicate n Nothing)
             | otherwise = error "Pick an even number greater than 2!"

-- | creates a board with disks in start positions
startBoard :: Int -> Board
startBoard n = placeDisk (placeDisk (placeDisk (placeDisk (blankBoard n)
                  (Just White) (a, a)) (Just Black) (a, a - 1)) (Just Black)
                  (a - 1, a)) (Just White) (a - 1, a - 1)
                  where
                    a = quot n 2

-- | prints the board to the console
printBoard :: Board -> IO ()
printBoard board = putStrLn (unlines (map (map toChar) board))

-- | converts maybe disk to char
toChar :: Maybe Disk -> Char
toChar (Just Black) = 'B'
toChar (Just White) = 'W'
toChar Nothing = '.'

-- | places/updates a disk in a board at a given position
placeDisk :: Board -> Maybe Disk -> Pos -> Board
placeDisk board d (x,y) = board !!= (x,row)
  where row = (board !! x) !!= (y,d)

-- | updates the given list with the new value at the given index
(!!=) :: [a] -> (Int, a) -> [a]
list !!= (i,e) | i >= length list || (i < 0) = list
                | otherwise = take i list ++ [e] ++ drop (i+1) list

play :: Board -> Maybe Disk -> Pos -> Board
play b d p | isCandidate b d p = flipDisks (placeDisk b d p) d (cellsToFlip b d p)
           | otherwise = b

flipDisks :: Board -> Maybe Disk -> [Pos] -> Board
flipDisks b d (x:[]) = placeDisk b d x
flipDisks b d (x:xs) = flipDisks (placeDisk b d x) d xs

-- || returns a list of positions of disks to flip
cellsToFlip :: Board -> Maybe Disk -> Pos -> [Pos]
cellsToFlip b d (x,y) = cellsToFlip' b d (x,y) (filterNeighbours b list d)
                     where
                       list = neighbours b [(i,j) | i <- [x+1, x, x-1], j <- [y+1, y, y-1]]

cellsToFlip' :: Board -> Maybe Disk -> Pos -> [Pos] -> [Pos]
cellsToFlip' b d (x,y) [] = []
cellsToFlip' b d (x,y) (z:[]) = getRow b d z (fst z - x, snd z - y)
cellsToFlip' b d (x,y) (z:zs) = getRow b d z (fst z - x, snd z - y) ++ cellsToFlip' b d (x,y) zs

--cellsToFlip' b d (x,y) (z:[]) | isOkRow b d z (fst z - x, snd z - y) = getRow b d z (fst z - x, snd z - y)
--                              | otherwise = []
--cellsToFlip' b d (x,y) (z:zs) | isOkRow b d z (fst z - x, snd z - y) = getRow b d z (fst z - x, snd z - y) ++ cellsToFlip' b d (x,y) zs
--                              | otherwise = []

getRow :: Board -> Maybe Disk -> Pos -> Pos -> [Pos]
getRow b d (x,y) (e,f) | d == getDisk b (x,y) = []
                       | otherwise = [(x,y)] ++ getRow b d (x+e, y+f) (e,f)

--isOkRow :: Board -> Maybe Disk -> Pos -> Pos -> Bool
--isOkRow b d (x,y) (e,f) | x+e<x || y+f<x || x+e>length b || y+f>length b = False
--                        | getDisk b (x,y) == Nothing = False
--                        | getDisk b (x,y) /= d = isOkRow b d (x+e, y+f) (e,f)
--                        | otherwise = getDisk b (x,y) == d

getDisk :: Board -> Pos -> Maybe Disk
getDisk b (x,y) = (b!!x)!!y

neighbours :: Board -> [Pos] -> [Pos]
neighbours b ((x,y):[]) | x < 0 || y < 0 || x > (length b)-1 || y > (length b)-1 = []
                        | otherwise = [(x,y)]
neighbours b ((x,y):xs) | x < 0 || y < 0 || x > (length b)-1 || y > (length b)-1 = [] ++ neighbours b xs
                        | otherwise = [(x,y)] ++ neighbours b xs

filterNeighbours :: Board -> [Pos] -> Maybe Disk -> [Pos]
filterNeighbours b (x:[]) d | getDisk b x == d || isNothing(getDisk b x) = []
                            | otherwise = [x]
filterNeighbours b (x:xs) d | getDisk b x == d || isNothing(getDisk b x) = [] ++ filterNeighbours b xs d
                            | otherwise = [x] ++ filterNeighbours b xs d

isCandidate :: Board -> Maybe Disk -> Pos -> Bool
isCandidate b d (x,y) = isLegal b (x,y) && isNothing (getDisk b (x,y)) && (cellsToFlip b d (x,y) /= [])

isLegal :: Board -> Pos -> Bool
isLegal board (x,y) = x >= 0 && y >= 0 && x < length board && y < length board
