module Othello where
import Data.Maybe

type Pos = (Int, Int)
data Disk = Black | White
            deriving ( Show, Eq )

data Othello = Othello { board :: [[Maybe Disk]]}
             deriving ( Show, Eq )

-- | creates a blank Othello board
blankBoard :: Int -> Othello
blankBoard size = Othello (replicate size (replicate size Nothing))

startBoard :: Int -> Othello
startBoard size = placeDisk (blankBoard size) (Just Black) (a, a)
                  where
                    a = quot size 2 

printOthello :: Othello -> IO ()
printOthello oth = putStrLn (unlines (map (map (toChar)) (board oth)))

toChar :: Maybe Disk -> Char
toChar (Just Black) = 'B'
toChar (Just White) = 'W'
toChar Nothing = '.'

placeDisk :: Othello -> Maybe Disk -> Pos -> Othello
placeDisk oth disk (x,y) = Othello ((board oth) !!=
  (x,(update (board oth) disk (x,y))))

-- | updates the given row at the given position with the new value
update :: [[Maybe Disk]] -> Maybe Disk -> Pos -> [Maybe Disk]
update oth disk (x,y) = (oth!!(x)) !!= ((x), disk)

-- | updates the given list with the new value at the given index
(!!=) :: [a] -> (Int, a) -> [a]
list !!= (i, e) | (i >= (length list)) || (i < 0) = list
                | otherwise = take i list ++ [e] ++ drop (i+1) list
