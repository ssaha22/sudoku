import BoardGenerator
import Boards
import Data.Char (isDigit)
import Data.Maybe (isNothing)
import Game
import Solver
import System.Exit (exitSuccess)

-- To run the game:
-- ghci
-- :l Main
-- play

-- runs the game with a generated board of a particular difficulty
play :: IO b
play = do
  putStrLn "Welcome to Haskell Sudoku!"
  difficulty <- getDifficulty
  boardPair <- generateBoard difficulty
  runGame boardPair

-- runs the game with the given board
playWithBoard :: Board -> IO ()
playWithBoard board = do
  if solvable board
    then runGame (board, head (solve board))
    else putStrLn "Invalid starting board."

-- runs the game given a starting board and its solution
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
                  runGame (performAction board action, solution)
                else do
                  putStrLn "Invalid move"
                  runGame (board, solution)

-- gets user input for a difficulty of easy, medium, or hard
getDifficulty :: IO String
getDifficulty = do
  putStr "Enter the difficulty you would like to play ('easy', 'medium', or 'hard'): "
  difficulty <- getLine
  if elem difficulty ["easy", "medium", "hard"]
    then return difficulty
    else do
      putStrLn "Invalid input"
      getDifficulty

-- gets user input for a ((row, col), num) action
getAction :: IO Action
getAction = do
  row <- getNumInput "Enter the row where you would like to place a number (0-8): "
  col <- getNumInput "Enter the column where you would like to place a number (0-8): "
  num <- getNumInput "Enter the number you would like to place (1-9): "
  return ((row, col), num)

-- prints the message and gets an Int user input
getNumInput :: String -> IO Int
getNumInput msg = do
  putStr msg
  input <- getLine
  if all isDigit input
    then return (read input :: Int)
    else do
      putStrLn "Invalid input"
      getNumInput msg
