module OthelloTypes where

import Test.QuickCheck
import Data.List.Split
import Data.Maybe

type Pos = (Int, Int)
newtype Board = Board { mtrx :: [[(Maybe Disk, Pos)]]} deriving (Show, Eq)
data Disk = White | Black deriving (Show, Eq)

-- | generates an arbitrary disk in an Othello
disk :: Gen (Maybe Disk)
disk = frequency [(7, return Nothing),
                  (3, elements [Just n | n <- [Black, White]])]

-- | an instance for generating Arbitrary Disk
instance Arbitrary Disk where
  arbitrary = frequency [(5, return Black), (5, return White)]

-- | an instance for generating Arbitrary Board
-- | parts from Sudoku lab
instance Arbitrary Board where
  arbitrary =
    do size <- elements [2,4..16]
       let pos = [(i,j) | i <- [0..size-1], j <- [0..size-1]]
       board <- sequence [ sequence [ disk | j <- [0..size-1] ] | i <- [0..size-1] ]
       return (Board (chunksOf (length board) (zip (concat board) pos)))
