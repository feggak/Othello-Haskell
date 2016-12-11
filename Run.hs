module Run where

import Othello

run :: IO ()
run = do
  putStrLn "Welcome to Othello!"
  putStr "Pick a board size: "
  size <- getLine
  loop (startBoard (read size :: Int)) White Black

loop :: Board -> Disk -> Disk -> IO ()
loop board d1 d2 =
  if canPlay board (Just d1) then do
    printBoard board
    putStr (show d1)
    putStr " horizontal position: "
    posX <- getLine
    putStr (show d1)
    putStr " vertical position: "
    posY <- getLine
    let newBoard = play board (Just d1)
                   ((read posY :: Int) - 1, (read posX :: Int) - 1)
    loop newBoard d2 d1
  else do
    if canPlay board (Just d2) then do
      putStr (show d1)
      putStrLn " can't make a move!"
      loop board d2 d1
    else do
      printBoard board
      putStr "Game is over! Winner is "
      putStrLn (show (winner board))
