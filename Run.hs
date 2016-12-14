module Run where

import Othello
import Data.Maybe
import System.Console.ANSI hiding (White, Black)

run :: IO ()
run = do
  setTitle "Othello Game"
  clearScreen
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

loop :: Board -> Disk -> Disk -> IO ()
loop board d1 d2
  | canPlay board (Just d1) = do
    putStrLn ""
    printBoard board
    if d1 == White then do
      setSGR [SetSwapForegroundBackground True]
      putStr (show d1)
      setSGR [Reset]
    else putStr (show d1)
    putStr " horizontal pos: "
    posX <- getLine
    if d1 == White then do
      setSGR [SetSwapForegroundBackground True]
      putStr (show d1)
      setSGR [Reset]
    else putStr (show d1)
    putStr " vertical pos: "
    posY <- getLine
    let newBoard = play board (Just d1) (
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
  | canPlay board (Just d2) = do
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
    if (isNothing(winner board)) then do
      putStr "It's a tie!"
    else do
      putStr "Winner is "
      setSGR [SetConsoleIntensity BoldIntensity]
      print (fromJust(winner board))
      setSGR [Reset]
