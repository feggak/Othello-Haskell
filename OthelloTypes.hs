module OthelloTypes where

import Data.Maybe

type Pos = (Int, Int)
data Board = Board { mtrx :: [[(Maybe Disk, Pos)]]} deriving (Show, Eq)
data Disk = White | Black deriving (Show, Eq)
