module Run where

import Othello

run :: IO ()
run = do
  putStrLn "Welcome to Othello!"
  loop (startBoard 8) White Black

loop :: Board -> Disk -> Disk -> IO ()
loop board d1 d2 =
  if True then do
    printBoard board
    putStr "New horizontal position: "
    posX <- getLine
    putStr "New vertical position: "
    posY <- getLine
    let newBoard = play board (Just d1)
                   ((read posY :: Int) - 1, (read posX :: Int) - 1)
    loop newBoard d2 d1
  else
    putStr "Game over"
