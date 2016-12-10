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
placeDisk board d (x, y) = board !!= (x, row)
  where row = (board !! x) !!= (y, d)

-- | updates the given list with the new value at the given index
(!!=) :: [a] -> (Int, a) -> [a]
list !!= (i, e) | (i >= length list) || (i < 0) = list
                | otherwise = take i list ++ [e] ++ drop (i+1) list

play :: Board -> Maybe Disk -> Pos -> Board
play b d p | fst p < 0 || snd p < 0 || fst p > (length b)-1 || snd p > (length b)-1 = b
           | isCandidate b d p = flipDisks (placeDisk b d p) d (nextTo b d p)
           | otherwise = b

flipDisks :: Board -> Maybe Disk -> [Pos] -> Board
flipDisks b d (x:[]) = placeDisk b d x
flipDisks b d (x:xs) = flipDisks (placeDisk b d x) d xs

-- || returns a list of positions of disks to flip
getCellsToFlip :: Board -> Maybe Disk -> Pos -> [Pos] -> [Pos]
getCellsToFlip b d (x,y) [] = []
getCellsToFlip b d (x,y) (z:[]) = getRow b d z (fst z - x,snd z - y)
getCellsToFlip b d (x,y) (z:zs) = getRow b d z (fst z -x, snd z - y) ++ getCellsToFlip b d (x,y) zs
--where
--  a = x - fst z
--  b = y - snd z

getRow :: Board -> Maybe Disk -> Pos -> Pos -> [Pos]
getRow b d (x,y) (e,f) | d == getDisk b (x,y) = []
                       | otherwise = [(x, y)] ++ getRow b d (x+e, y+f) (e,f)

getDisk :: Board -> Pos -> Maybe Disk
getDisk b (x,y) = (b!!x)!!y

nextTo :: Board -> Maybe Disk -> Pos -> [Pos]
nextTo b d (x,y) = getCellsToFlip b d (x,y) (nextTo' [((b!!(fst i))!!(snd i),i) | i <- list] d)
                    where
                      list = neighbours b [(i,j) | i <- [x+1, x, x-1], j <- [y+1, y, y-1]]

neighbours :: Board -> [Pos] -> [Pos]
neighbours b (x:[]) | fst x <= 0 || snd x <= 0 || fst x >= (length b) -1 || snd x >= (length b) -1 = []
                    | otherwise = [x]
neighbours b (x:xs) | fst x <= 0 || snd x <= 0 || fst x >= (length b) -1 || snd x >= (length b) -1 = [] ++ neighbours b xs
                    | otherwise = [x] ++ neighbours b xs

nextTo' :: [(Maybe Disk, Pos)] -> Maybe Disk -> [Pos]
nextTo' (x:[]) d | fst x == d || fst x == Nothing = []
                 | otherwise = [snd x]
nextTo' (x:xs) d | fst x == d || fst x == Nothing = nextTo' xs d
                 | otherwise = [snd x] ++ nextTo' xs d

isCandidate :: Board -> Maybe Disk -> Pos -> Bool
isCandidate b d (x,y) = (b!!x)!!y == Nothing && not ((nextTo b d (x,y)) == [])
