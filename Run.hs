module Run where

import Othello

run :: IO ()
run = do
  putStrLn "Welcome to Othello!"
  putStr "Pick a board size: "
  input <- getLine
  let size = (read input :: Int)
  if even size then do
    loop (startBoard size) White Black
  else do
    putStrLn "Pick an even number greater than 2!"
    run

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
    case newBoard of
      Nothing -> do
        putStrLn "Invalid move, try again!"
        loop board d1 d2
      Just b -> loop b d2 d1
  else do
    if canPlay board (Just d2) then do
      putStr (show d1)
      putStrLn " can't make a move!"
      loop board d2 d1
    else do
      printBoard board
      putStr "Game is over! Winner is "
      putStrLn (show (winner board))
