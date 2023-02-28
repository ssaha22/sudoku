module Solver where

import Data.Maybe (isNothing)
import Game

-- produces all possible solutions to a given sudoku board
solve :: Board -> [Board]
solve board = do
  let emptyPositions = getEmptyPositions board
  if null emptyPositions
    then [board]
    else solve' board emptyPositions (getValidActions board (head emptyPositions)) []

-- performs backtracking to recursively find all solutions for the given board
-- positions is a list of the empty positions on the board
-- actions is a list of valid actions for the first empty position (head positions)
-- solutions is the list of solutions found so far
solve' :: Board -> [Position] -> [Action] -> [Board] -> [Board]
solve' board positions actions solutions
  | null actions = solutions
  | null (tail positions) = performAction board (head actions) : solutions
  | otherwise = do
      let newBoard = performAction board (head actions)
      let newPositions = tail positions
      let newActions = getValidActions newBoard (head newPositions)
      let result = solve' newBoard newPositions newActions solutions
      result ++ solve' board positions (tail actions) solutions

-- returns True if a board has only one valid solution, False otherwise
solvable :: Board -> Bool
solvable board = length (solve board) == 1

-- returns all empty positions on the board
getEmptyPositions :: Board -> [Position]
getEmptyPositions board = [(row, col) | row <- [0 .. 8], col <- [0 .. 8], board !! row !! col == 0]

-- returns all the valid actions for the given board and position
getValidActions :: Board -> Position -> [Action]
getValidActions board pos = [(pos, num) | num <- [1 .. 9], isValidAction board (pos, num)]
