module Othello where

import Data.Maybe

type Pos = (Int, Int)
type Board = [[Maybe Disk]]
data Disk = White | Black deriving (Show, Eq)

otherDisk :: Maybe Disk -> Maybe Disk
otherDisk (Just White) = Just Black
otherDisk (Just Black) = Just White

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
play b d p | isCandidate b d p =
             flipDisks (placeDisk b d p) d (cellsToFlip b d p)
           | otherwise = b

flipDisks :: Board -> Maybe Disk -> [Pos] -> Board
flipDisks b d (x:[]) = placeDisk b d x
flipDisks b d (x:xs) = flipDisks (placeDisk b d x) d xs

-- || returns a list of positions of disks to flip
cellsToFlip :: Board -> Maybe Disk -> Pos -> [Pos]
cellsToFlip b d pos = cellsToFlip' b d pos (neighbours b d pos)

cellsToFlip' :: Board -> Maybe Disk -> Pos -> [Pos] -> [Pos]
cellsToFlip' b d (x,y) [] = []
cellsToFlip' b d (x,y) (z:[]) = getRow b d z (fst z - x, snd z - y)
cellsToFlip' b d (x,y) (z:zs) = getRow b d z (fst z - x, snd z - y)
                                ++ cellsToFlip' b d (x,y) zs

getRow :: Board -> Maybe Disk -> Pos -> Pos -> [Pos]
getRow b d (x,y) (e,f) | not (isOkRow b d (x,y) (e,f)) = []
                       | d == getDisk b (x,y) = []
                       | otherwise = (x,y) : getRow b d (x+e, y+f) (e,f)

isOkRow :: Board -> Maybe Disk -> Pos -> Pos -> Bool
isOkRow b d (x,y) (e,f) | not (isLegal b (x+e,y+f)) = False
                        | isNothing(getDisk b (x,y)) = False
                        | d == getDisk b (x,y) = True
                        | otherwise = isOkRow b d (x+e, y+f) (e,f)

getDisk :: Board -> Pos -> Maybe Disk
getDisk b (x,y) = (b!!x)!!y

neighbours :: Board -> Maybe Disk -> Pos -> [Pos]
neighbours b d (x,y) = neighbours' b d
                       [(i,j) | i <- [x+1, x, x-1], j <- [y+1, y, y-1]]

neighbours' :: Board -> Maybe Disk -> [Pos] -> [Pos]
neighbours' b d (x:[]) | isLegal b x &&
                         getDisk b x == otherDisk d = [x]
                       | otherwise = []
neighbours' b d (x:xs) | isLegal b x &&
                         getDisk b x == otherDisk d = x : neighbours' b d xs
                       | otherwise = [] ++ neighbours' b d xs

isCandidate :: Board -> Maybe Disk -> Pos -> Bool
isCandidate b d (x,y) = isLegal b (x,y) &&
                        isNothing (getDisk b (x,y)) &&
                        cellsToFlip b d (x,y) /= []

isLegal :: Board -> Pos -> Bool
isLegal board (x,y) = x >= 0 && y >= 0 && x < length board && y < length board

canPlay :: Board -> Disk -> Bool
canPlay board d = undefined

blanks :: Board -> [Pos]
blanks b = zip (yPos (rowBlanks b))
               (whereBlank (xValuePos b))

-- | returns a list of all the blanks y-positions
yPos :: [Int] -> [Int]
yPos list = concat ([(replicate (list!!i) i) | i <- [0..8]])

-- | returns a list of the number of blanks from all the rows
rowBlanks :: [[Maybe Disk]] -> [Int]
rowBlanks (x:[]) = [length (whereBlank (zip x [0..8]))]
rowBlanks (x:xs) = [length (whereBlank (zip x [0..8]))] ++ rowBlanks xs

-- | creates a total list of pairs containing a Maybe Int and the x position
xValuePos :: [[Maybe Disk]] -> [(Maybe Disk, Int)]
xValuePos (x:[]) = zip x [0..8]
xValuePos (x:xs) = zip x [0..8] ++ xValuePos xs

-- | takes a list of pairs (Maybe Int and pos) and returns a list of the pos
-- | of the Maybe Ints that are empty (Nothing)
whereBlank :: [(Maybe Disk, Int)] -> [Int]
whereBlank (x:[]) = if (fst x) == Nothing then [(snd x)]
                    else []
whereBlank (x:xs) = if (fst x) == Nothing then [(snd x)] ++ whereBlank xs
                    else whereBlank xs

winner :: Board -> Maybe Disk
winner board = undefined
