module Run where

import Othello
import System.Console.ANSI hiding (White, Black)

run :: IO ()
run = do
  putStrLn "Welcome to Othello!"
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
    putStr (show d1)
    putStr " horizontal position: "
    posX <- getLine
    putStr (show d1)
    putStr " vertical position: "
    posY <- getLine
    let newBoard = play board (Just d1)
                   ((read posY :: Int) - 1, (read posX :: Int) - 1)
    case newBoard of
      Nothing -> do
        setSGR [SetColor Foreground Vivid Yellow]
        putStrLn "Invalid move, try again!"
        setSGR [Reset]
        loop board d1 d2
      Just b -> loop b d2 d1
  | canPlay board (Just d2) =
    if canPlay board (Just d2) then do
      setSGR [SetColor Foreground Vivid Red]
      putStr (show d1)
      putStrLn " can't make a move!"
      setSGR [Reset]
      loop board d2 d1
    else do
      printBoard board
      setSGR [SetColor Foreground Vivid Green]
      putStr "Game is over! Winner is "
      setSGR [SetColor Foreground Vivid Blue]
      print (winner board)
      setSGR [Reset]
