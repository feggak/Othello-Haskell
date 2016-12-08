module Othello where
import Data.Maybe

type Pos = (Int, Int)
type Board = [[Maybe Disk]]
data Disk = White | Black deriving (Show, Eq)

blankBoard :: Int -> Board
blankBoard n = replicate n (replicate n Nothing)

printBoard :: Board -> IO ()
printBoard board = putStrLn (unlines (map (map diskToChar) board))

-- | converts maybe disk to char
diskToChar :: Maybe Disk -> Char
diskToChar (Just White) = 'W'
diskToChar (Just Black) = 'B'
diskToChar Nothing = '.'

-- | places a disk on a position on a board
placeDisk :: Board -> Disk -> Pos -> Board
placeDisk board d pos = undefined

-- | updates the given list with the new value at the given index
(!!=) :: [a] -> (Int, a) -> [a]
list !!= (i, e) | (i >= length list) || (i < 0) = list
                | otherwise = take i list ++ [e] ++ drop (i+1) list
