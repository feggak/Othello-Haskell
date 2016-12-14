module OthelloTests where

import Othello
import Test.QuickCheck

prop_flipDisk :: Board -> Maybe Disk -> [Pos] -> Property
prop_flipDisk board d [] = True ==> True
prop_flipDisk board d pos = prop_flipDisk' (flipDisks board d pos) d pos

prop_flipDisk' :: Board -> Maybe Disk -> [Pos] -> Property
prop_flipDisk' b d (x:[]) = isLegal b x ==>
                            getDisk b x == d
prop_flipDisk' b d (x:xs) = isLegal b x ==>
                            if getDisk b x == d then
                              prop_flipDisk' b d xs
                            else isLegal b x ==> False
