module Run where

import Othello

run :: IO ()
run = do
  putStrLn "Welcome to Othello!"
  loop (startBoard 4) White Black

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
  else
    loop newBoard d2 d1
