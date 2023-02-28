module BoardGenerator where

import Boards
import Data.List (elemIndex, transpose)
import Data.Maybe (fromJust)
import Game
import Solver
import System.Random (randomRIO)

-- produces a starting and solution board of the given difficulty
generateBoard :: String -> IO (Board, Board)
generateBoard difficulty = do
  let baseBoard
        | difficulty == "easy" = easyBaseBoard
        | difficulty == "medium" = mediumBaseBoard
        | otherwise = hardBaseBoard
  jumbledBoard <- createJumbledBoard baseBoard
  startBoard <- emptyBoard jumbledBoard 50
  return (startBoard, jumbledBoard)

-- jumbles a given board (meant to take an already accurate board like what makeBaseBoard returns)
createJumbledBoard :: Board -> IO Board
createJumbledBoard board = do
  newBoard1 <- jumbleBoard board 1
  newBoard2 <- shuffleRows newBoard1 0 2 0
  newBoard3 <- shuffleRows newBoard2 3 5 3
  newBoard4 <- shuffleRows newBoard3 6 8 6
  newBoard5 <- shuffleColumns newBoard4 0 2 0
  newBoard6 <- shuffleColumns newBoard5 3 5 3
  newBoard7 <- shuffleColumns newBoard6 6 8 6
  newBoard8 <- shuffleGroupThreeRows newBoard7 0 8 0
  shuffleGroupThreeColumns newBoard8 0 8 0

-- removes n numbers off the board
emptyBoard :: Board -> Int -> IO Board
emptyBoard board n =
  if n == 0
    then return board
    else do
      row <- randomInt 0 8
      col <- randomInt 0 8
      if board !! row !! col == 0
        then emptyBoard board n
        else do
          let newBoard = performAction board ((row, col), 0)
          if solvable newBoard
            then emptyBoard newBoard (n - 1)
            else emptyBoard board n

-- returns a random integer in the range lo to hi
randomInt :: Int -> Int -> IO Int
randomInt lo hi = do randomRIO (lo, hi)

-- mixes up an already valid board number by number
-- start board = valid board, counter = 1
jumbleBoard :: Board -> Int -> IO Board
jumbleBoard board counter = do
  n <- randomInt 1 9
  let newBoard = replaceInBoard board 0 counter n
  if counter == 9
    then return newBoard
    else do jumbleBoard newBoard (counter + 1)

-- switches n and num in every row in the board
-- starts counter = 0, firstValue = 0, num = 1, n = randomInt 1 9
replaceInBoard :: Board -> Int -> Int -> Int -> Board
replaceInBoard board rowCounter num n = do
  let cIndex = fromJust (elemIndex num (board !! rowCounter))
  let nIndex = fromJust (elemIndex n (board !! rowCounter))
  let newBoard = performAction board ((rowCounter, cIndex), n)
  let newBoard2 = performAction newBoard ((rowCounter, nIndex), num)
  if rowCounter == 8
    then newBoard2
    else replaceInBoard newBoard2 (rowCounter + 1) num n

-- mixes up rows in the group firstRow to lastRow
-- starts counter = 0, firstRow & lastRow dependent on where you want to start
-- best pairs for valid board (0,2), (3,5), (6,8)
shuffleRows :: Board -> Int -> Int -> Int -> IO Board
shuffleRows board firstRow lastRow counter = do
  n <- randomInt firstRow lastRow
  let counterRow = board !! counter
  let newboard1 = replaceElementInList board counter (board !! n)
  let newboard2 = replaceElementInList newboard1 n counterRow
  if counter == lastRow
    then do
      return newboard2
    else do
      shuffleRows newboard2 firstRow lastRow (counter + 1)

-- mixes up columns in the group firstColumn to lastColumn
-- starts counter = 0, firstColumn & lastColumn dependent on where you want to start
-- best pairs for valid board (0,2), (3,5), (6,8)
shuffleColumns :: Board -> Int -> Int -> Int -> IO Board
shuffleColumns board firstColumn lastColumn counter = do
  transposedNewBoard <- shuffleRows (transpose board) firstColumn lastColumn counter
  return (transpose transposedNewBoard)

-- mix up blocks of 3 rows: block 1 row 0-2, block 2 row 3-5, block 3 row 6-8
-- start counter = 0, firstRow = 0, lastRow = 8
shuffleGroupThreeRows :: Board -> Int -> Int -> Int -> IO Board
shuffleGroupThreeRows board firstRow lastRow counter = do
  n <- randomInt firstRow lastRow
  let num = assignBlockNum n

  let counterRow1 = board !! counter
  let newboard1 = replaceElementInList board counter (board !! num)
  let newboard2 = replaceElementInList newboard1 num counterRow1

  let counter2 = counter + 1
  let counterRow2 = board !! counter2
  let newboard3 = replaceElementInList newboard2 counter2 (board !! (num + 1))
  let newboard4 = replaceElementInList newboard3 (num + 1) counterRow2

  let counter3 = counter2 + 1
  let counterRow3 = board !! counter3
  let newboard5 = replaceElementInList newboard4 counter3 (board !! (num + 2))
  let newboard6 = replaceElementInList newboard5 (num + 2) counterRow3
  if counter3 == 8
    then do
      return newboard6
    else shuffleGroupThreeRows newboard6 (firstRow + 3) lastRow (counter3 + 1)

-- takes a number and returns which block it is in
assignBlockNum :: Int -> Int
assignBlockNum num
  | num >= 0 && num <= 2 = 0
  | num >= 3 && num <= 5 = 3
  | num >= 6 && num <= 8 = 6
  | otherwise = 0

-- mix up blocks of 3 columns: block 1 column 0-2, block 2 colum 3-5, block 3 column 6-8
-- start counter = 0, firstColumn = 0, lastColumn = 8
shuffleGroupThreeColumns :: Board -> Int -> Int -> Int -> IO Board
shuffleGroupThreeColumns board firstColumn lastColumn counter =
  do
    transposedNewBoard <- shuffleGroupThreeRows (transpose board) firstColumn lastColumn counter
    return (transpose transposedNewBoard)
