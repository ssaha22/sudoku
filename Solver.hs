module Solver where

import Data.Maybe (isNothing)
import Game

solve :: Board -> Maybe Board
solve board = do
  let emptyPositions = getEmptyPositions board
  if null emptyPositions
    then Just board
    else solve' board emptyPositions (getValidActions board (head emptyPositions))

solve' :: Board -> [Position] -> [Action] -> Maybe Board
solve' board positions actions
  | null actions = Nothing
  | null (tail positions) = return (performValidAction board (head actions))
  | otherwise = do
      let newBoard = performValidAction board (head actions)
      let newPositions = tail positions
      let newActions = getValidActions newBoard (head newPositions)
      let result = solve' newBoard newPositions newActions
      case result of
        Just solution -> result
        Nothing -> solve' board positions (tail actions)

getEmptyPositions :: Board -> [Position]
getEmptyPositions board = [(row, col) | row <- [0 .. 8], col <- [0 .. 8], board !! row !! col == 0]

getValidActions :: Board -> Position -> [Action]
getValidActions board pos = [(pos, num) | num <- [1 .. 9], isValidAction board (pos, num)]
