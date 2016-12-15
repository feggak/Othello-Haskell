module Run where

import Othello
import OthelloTypes
import OthelloUtils

import Data.Maybe
import System.Console.ANSI hiding (White, Black)

-- | first functiion to be run, here you choose the size of the game board
run :: IO ()
run = do
  setTitle "Othello Game"
  putStr "Welcome to "
  setSGR [SetConsoleIntensity BoldIntensity]
  putStr "Othello"
  setSGR [Reset]
  putStrLn "!"
  putStr "Pick a board size: "
  input <- getLine
  let size = read input :: Int
  if even size then
    loop (startBoard size) White Black
  else do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn "Pick an even number greater than 2!"
    setSGR [Reset]
    run

-- | main game loop, asks for input and alternates turns for players
loop :: Board -> Disk -> Disk -> IO ()
loop board d1 d2
  | canPlay board d1 = do
    putStrLn ""
    printBoard board
    printPlayer d1
    putStr " horizontal pos: "
    posX <- getLine
    printPlayer d1
    putStr " vertical pos: "
    posY <- getLine
    let newBoard = play board d1 (
                     (read posY :: Int) - 1,
                     (read posX :: Int) - 1
                   )
    case newBoard of
      Nothing -> do
        putStrLn ""
        setSGR [SetColor Foreground Vivid Yellow]
        putStrLn "Invalid move, try again!"
        setSGR [Reset]
        loop board d1 d2
      Just b -> loop b d2 d1
  | canPlay board d2 = do
    setSGR [SetColor Foreground Vivid Red]
    putStr (show d1)
    putStrLn " can't make a move!"
    setSGR [Reset]
    loop board d2 d1
  | otherwise = do
    putStrLn ""
    printBoard board
    setSGR [SetColor Foreground Vivid Green]
    putStrLn "Game is over!"
    setSGR [Reset]
    if isNothing(winner board) then putStr "It's a tie!"
    else do
      putStr "Winner is "
      setSGR [SetConsoleIntensity BoldIntensity]
      print (fromJust(winner board))
      setSGR [Reset]

-- | prints the board to the console
printBoard :: Board -> IO ()
printBoard b = putStrLn (unlines (map (map toChar) board))
  where
    board = map (map fst) (mtrx b)

-- | prints the player to the console
printPlayer :: Disk -> IO ()
printPlayer d =
  if d == White then do
    setSGR [SetSwapForegroundBackground True]
    putStr (show d)
    setSGR [Reset]
  else putStr (show d)
