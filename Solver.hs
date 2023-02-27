module Solver where

import Data.Maybe (isNothing)
import Game

solve :: Board -> [Board]
solve board = do
  let emptyPositions = getEmptyPositions board
  if null emptyPositions
    then [board]
    else solve' board emptyPositions (getValidActions board (head emptyPositions)) []

solve' :: Board -> [Position] -> [Action] -> [Board] -> [Board]
solve' board positions actions solutions
  | null actions = solutions
  | null (tail positions) = performValidAction board (head actions) : solutions
  | otherwise = do
      let newBoard = performValidAction board (head actions)
      let newPositions = tail positions
      let newActions = getValidActions newBoard (head newPositions)
      let result = solve' newBoard newPositions newActions solutions
      result ++ solve' board positions (tail actions) solutions

getEmptyPositions :: Board -> [Position]
getEmptyPositions board = [(row, col) | row <- [0 .. 8], col <- [0 .. 8], board !! row !! col == 0]

getValidActions :: Board -> Position -> [Action]
getValidActions board pos = [(pos, num) | num <- [1 .. 9], isValidAction board (pos, num)]
