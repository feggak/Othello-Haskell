module Othello where

import OthelloUtils
import Data.Maybe

type Pos = (Int, Int)
type Board = [[(Maybe Disk, Pos)]]
data Disk = White | Black deriving (Show, Eq)

-- | creates a blank board
blankBoard :: Int -> Board
blankBoard n | even n && n > 2 =
                [[(Nothing, (i,j)) | j <- [0..n-1]] | i <- [0..n-1]]
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
printBoard b = putStrLn (unlines (map (map toChar) board))
  where
    board = map (map fst) b

-- | converts maybe disk to char
toChar :: Maybe Disk -> Char
toChar (Just Black) = '◯'
toChar (Just White) = '●'
toChar Nothing = '⋅'

-- | returns the disk att the given position
getDisk :: Board -> Pos -> Maybe Disk
getDisk b (x,y) = fst((b!!x)!!y)

-- | returns opposite color of given disk
otherDisk :: Maybe Disk -> Maybe Disk
otherDisk (Just White) = Just Black
otherDisk (Just Black) = Just White

-- | places a disk and flips all affected disks, runs one round of the game
-- | if the move is not valid, Nothing is returned
play :: Board -> Maybe Disk -> Pos -> Maybe Board
play b d p | isCandidate b d p =
             Just (flipDisks (placeDisk b d p) d (cellsToFlip b d p))
           | otherwise = Nothing

-- | places/updates a disk in a board at a given position
placeDisk :: Board -> Maybe Disk -> Pos -> Board
placeDisk b d (x,y) = b !!= (x,row)
  where row = (b !! x) !!= (y,(d,(x,y)))

-- | flips the disks to given color in positions given
flipDisks :: Board -> Maybe Disk -> [Pos] -> Board
flipDisks b d (x:[]) = placeDisk b d x
flipDisks b d (x:xs) = flipDisks (placeDisk b d x) d xs

-- | returns a list of positions of disks to flip
cellsToFlip :: Board -> Maybe Disk -> Pos -> [Pos]
cellsToFlip b d pos = cellsToFlip' b d pos (neighbours b d pos)

cellsToFlip' :: Board -> Maybe Disk -> Pos -> [Pos] -> [Pos]
cellsToFlip' b d (x,y) [] = []
cellsToFlip' b d (x,y) (z:[]) = getRow b d z (fst z - x, snd z - y)
cellsToFlip' b d (x,y) (z:zs) = getRow b d z (fst z - x, snd z - y)
                                ++ cellsToFlip' b d (x,y) zs

-- | takes a position, a color and a direction (as simple pos)
-- | returns the row of disks to flip in that direction
getRow :: Board -> Maybe Disk -> Pos -> Pos -> [Pos]
getRow b d (x,y) (e,f) | not (isOkRow b d (x,y) (e,f)) = []
                       | d == getDisk b (x,y) = []
                       | otherwise = (x,y) : getRow b d (x+e, y+f) (e,f)

-- | checks if there is a valid row in given direction
isOkRow :: Board -> Maybe Disk -> Pos -> Pos -> Bool
isOkRow b d (x,y) (e,f) | not (isLegal b (x,y)) = False
                        | isNothing(getDisk b (x,y)) = False
                        | d == getDisk b (x,y) = True
                        | otherwise = isOkRow b d (x+e, y+f) (e,f)

-- | returns all disks of opposite color in a circle around
-- | given position that is not outside the board
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

-- | checks if the given position is a valid move for given color
isCandidate :: Board -> Maybe Disk -> Pos -> Bool
isCandidate b d (x,y) = isLegal b (x,y) &&
                        isNothing (getDisk b (x,y)) &&
                        cellsToFlip b d (x,y) /= []

-- | checks if the given position is inside board bounds
isLegal :: Board -> Pos -> Bool
isLegal board (x,y) = x >= 0 && y >= 0 && x < length board && y < length board

-- | returns all blank positions on the board
blanks :: Board -> [Pos]
blanks (x:[]) = blanks' x
blanks (x:xs) = concat (blanks' x : [blanks xs])

blanks' :: [(Maybe Disk, Pos)] -> [Pos]
blanks' (x:[]) | isNothing(fst x) = [snd x]
               | otherwise = []
blanks' (x:xs) | isNothing(fst x) = snd x : blanks' xs
               | otherwise = [] ++ blanks' xs

-- | checks if there is a valid move to be done for given color
canPlay :: Board -> Maybe Disk -> Bool
canPlay b d = any (isCandidate b d) (blanks b)

-- | returns the color with most disks on the board
winner :: Board -> Maybe Disk
winner board | winner' board (Just White) > winner' board (Just Black) = Just White
winner board | winner' board (Just White) < winner' board (Just Black) = Just Black
winner board = Nothing

winner' :: Board -> Maybe Disk -> Int
winner' (x:[]) d = length (filter ((==d).fst) x)
winner' (x:xs) d = length (filter ((==d).fst) x) + winner' xs d
