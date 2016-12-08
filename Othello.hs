module Othello where
import Data.Maybe

type Pos = (Int, Int)
type Board = [[Maybe Disk]]
data Disk = White | Black deriving (Show, Eq)

-- | creates a blank board
blankBoard :: Int -> Board
blankBoard n = replicate n (replicate n Nothing)

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
placeDisk board d (x, y) = board !!= (x, row)
  where row = (board !! x) !!= (y, d)

-- | updates the given list with the new value at the given index
(!!=) :: [a] -> (Int, a) -> [a]
list !!= (i, e) | (i >= length list) || (i < 0) = list
                | otherwise = take i list ++ [e] ++ drop (i+1) list
