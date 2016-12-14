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
instance Arbitrary Board where
  arbitrary =
    do board <- sequence [ sequence [ disk | j <- [0..7] ] | i <- [0..7] ]
       return (Board (chunksOf (length board) (zip (concat board) pos)))
       where
         pos = [(i,j) | i <- [0..7], j <- [0..7]]
