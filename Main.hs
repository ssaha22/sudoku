import BoardGenerator
import Boards
import Data.Char (isDigit)
import Data.Maybe (isNothing)
import Game
import Solver
import System.Exit (exitSuccess)

play :: IO b
play = do
  putStrLn "Welcome to Haskell Sudoku!"
  difficulty <- getDifficulty
  boardPair <- generateBoard difficulty
  runGame boardPair

playWithBoard board = do
  if solvable board
    then runGame (board, head (solve board))
    else putStrLn "Invalid starting board."

runGame :: (Board, Board) -> IO b
runGame (board, solution) = do
  if board == solution
    then do
      putStrLn (boardToString board)
      putStrLn "Congratulations! You completed the game!"
      exitSuccess
    else do
      putStrLn "Current board:"
      putStrLn (boardToString board)
      putStr "Enter 'h' for a hint, 'q' to quit, or 'c' to continue playing: "
      input <- getLine
      if input == "h"
        then do
          let (updatedBoard, action) = applyHint board solution
          putStrLn (actionToString action)
          runGame (updatedBoard, solution)
        else
          if input == "q"
            then exitSuccess
            else do
              action <- getAction
              if isValidAction board action
                then do
                  putStrLn (actionToString action)
                  runGame (performValidAction board action, solution)
                else do
                  putStrLn "Invalid move"
                  runGame (board, solution)

getDifficulty :: IO String
getDifficulty = do
  putStr "Enter the difficulty you would like to play ('easy', 'medium', or 'hard'): "
  difficulty <- getLine
  if elem difficulty ["easy", "medium", "hard"]
    then return difficulty
    else do
      putStrLn "Invalid input"
      getDifficulty

getAction :: IO Action
getAction = do
  row <- getNumInput "Enter the row where you would like to place a number: "
  col <- getNumInput "Enter the column where you would like to place a number: "
  num <- getNumInput "Enter the number you would like to place: "
  return ((row, col), num)

getNumInput :: String -> IO Int
getNumInput msg = do
  putStr msg
  input <- getLine
  if all isDigit input
    then return (read input :: Int)
    else do
      putStrLn "Invalid input"
      getNumInput msg
