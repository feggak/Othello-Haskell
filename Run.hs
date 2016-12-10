module Run where

import Othello

run :: IO ()
run = do
  putStrLn "Welcome to Othello!"
  loop (startBoard 8)

loop :: Board -> IO ()
loop board =
  if True then do
    printBoard board
    putStr "New horizontal position: "
    posX <- getLine
    putStr "New vertical position: "
    posY <- getLine
    let newBoard = placeDisk board (Just White)
                   ((read posY :: Int) - 1, (read posX :: Int) - 1)
    loop newBoard
  else
    loop (blankBoard 8)
