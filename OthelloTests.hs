module OthelloTests where

import Othello
import OthelloTypes

import Data.Maybe
import Test.QuickCheck

-- | checks if the generated blank board of varying size is actually all blanks
prop_blankBoard_is_blank :: Int -> Bool
prop_blankBoard_is_blank n | even n && n > 2 = (all (\x -> ((all (\y -> (fst y == Nothing)) x)))) (mtrx (blankBoard n))
                           | otherwise = True

-- | checks if properties of generated boards are valid
prop_board :: Board -> Bool
prop_board b = (all (\x -> (length x) == length (mtrx b) ) (mtrx b) ) &&
               (all (\x -> ((all (\y -> (
               fst y == (Just Black) ||
               fst y == (Just White) ||
               fst y == Nothing)) x)))) (mtrx b)

-- | checks if placeDisk actually placed given disk at given position
prop_placeDisk :: Board -> Maybe Disk -> Pos -> Bool
prop_placeDisk b d pos | isLegal b pos = getDisk (placeDisk b d pos) pos == d
                       | otherwise = True

-- | checks if the given positions to flip are actaully flipped
-- | counts Nothing as True because the position is invalid
prop_flipDisk :: Board -> Maybe Disk -> [Pos] -> Bool
prop_flipDisk b d [] = True
prop_flipDisk b d pos = prop_flipDisk' (flipDisks b d pos) d pos

prop_flipDisk' :: Board -> Maybe Disk -> [Pos] -> Bool
prop_flipDisk' b d (x:[]) = getDisk b x == d || getDisk b x == Nothing
prop_flipDisk' b d (x:xs) | getDisk b x == d || getDisk b x == Nothing
                            = prop_flipDisk' b d xs
                          | otherwise = False

prop_blanks :: Board -> Bool
prop_blanks b = all (\x -> (getDisk b x) == Nothing) (blanks (mtrx b))
