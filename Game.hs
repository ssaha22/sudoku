module Game where

type Board = [[Int]]

type Position = (Int, Int)

type Action = (Position, Int)

exampleBoard :: Board
exampleBoard =
  [ [0, 5, 8, 2, 4, 0, 9, 1, 0],
    [0, 0, 0, 0, 9, 0, 6, 8, 7],
    [0, 0, 0, 0, 6, 0, 2, 0, 0],
    [8, 0, 5, 0, 0, 0, 4, 0, 0],
    [0, 7, 0, 0, 5, 0, 1, 6, 2],
    [1, 2, 0, 0, 0, 4, 0, 3, 0],
    [0, 9, 6, 0, 8, 1, 3, 0, 5],
    [0, 8, 1, 0, 0, 0, 0, 2, 0],
    [7, 4, 3, 5, 0, 6, 0, 0, 0]
  ]

exampleBoardAlmostFilled :: Board
exampleBoardAlmostFilled =
  [ [0, 5, 8, 2, 4, 0, 9, 1, 3],
    [4, 3, 2, 1, 9, 5, 6, 8, 7],
    [9, 1, 7, 8, 6, 3, 2, 5, 4],
    [8, 6, 5, 3, 1, 2, 4, 7, 9],
    [3, 7, 4, 9, 5, 8, 1, 6, 2],
    [1, 2, 9, 6, 7, 4, 5, 3, 8],
    [2, 9, 6, 7, 8, 1, 3, 4, 5],
    [5, 8, 1, 4, 3, 9, 7, 2, 6],
    [7, 4, 3, 5, 2, 6, 8, 9, 1]
  ]

exampleBoardSolution :: Board
exampleBoardSolution =
  [ [6, 5, 8, 2, 4, 7, 9, 1, 3],
    [4, 3, 2, 1, 9, 5, 6, 8, 7],
    [9, 1, 7, 8, 6, 3, 2, 5, 4],
    [8, 6, 5, 3, 1, 2, 4, 7, 9],
    [3, 7, 4, 9, 5, 8, 1, 6, 2],
    [1, 2, 9, 6, 7, 4, 5, 3, 8],
    [2, 9, 6, 7, 8, 1, 3, 4, 5],
    [5, 8, 1, 4, 3, 9, 7, 2, 6],
    [7, 4, 3, 5, 2, 6, 8, 9, 1]
  ]

unsolvableBoard :: Board
unsolvableBoard =
  [ [5, 1, 6, 8, 4, 9, 7, 3, 2],
    [3, 0, 7, 6, 0, 5, 0, 0, 0],
    [8, 0, 9, 7, 0, 0, 0, 6, 5],
    [1, 3, 5, 0, 6, 0, 9, 0, 7],
    [4, 7, 2, 5, 9, 1, 0, 0, 6],
    [9, 6, 8, 3, 7, 0, 0, 5, 0],
    [2, 5, 3, 1, 8, 6, 0, 7, 4],
    [6, 8, 4, 2, 0, 7, 5, 0, 0],
    [7, 9, 1, 0, 5, 0, 6, 0, 8]
  ]

boardWithTwoSolutions :: Board
boardWithTwoSolutions =
  [ [2, 9, 5, 7, 4, 3, 8, 6, 1],
    [4, 3, 1, 8, 6, 5, 9, 0, 0],
    [8, 7, 6, 1, 9, 2, 5, 4, 3],
    [3, 8, 7, 4, 5, 9, 2, 1, 6],
    [6, 1, 2, 3, 8, 7, 4, 9, 5],
    [5, 4, 9, 2, 1, 6, 7, 3, 8],
    [7, 6, 3, 5, 2, 4, 1, 8, 9],
    [9, 2, 8, 6, 7, 1, 3, 5, 4],
    [1, 5, 4, 9, 3, 8, 6, 0, 0]
  ]

exampleBoardPair :: (Board, Board)
exampleBoardPair = (exampleBoard, exampleBoardSolution)

isValidAction :: Board -> Action -> Bool
isValidAction board ((row, col), num)
  | row < 0 || row > 8 = False
  | col < 0 || col > 8 = False
  | num < 1 || num > 9 = False
  | rowContainsNum board row num = False
  | colContainsNum board col num = False
  | blockContainsNum board ((row, col), num) = False
  | otherwise = True

performValidAction :: Board -> Action -> Board
performValidAction board ((row, col), num) =
  replaceElementInList board row (replaceElementInList (board !! row) col num)

applyHint :: Board -> Board -> (Board, Action)
applyHint board solution =
  head
    [ ( performValidAction
          board
          action,
        action
      )
      | row <- [0 .. 8],
        col <- [0 .. 8],
        board !! row !! col == 0,
        let solutionNum = solution !! row !! col,
        let action = ((row, col), solutionNum),
        isValidAction board action
    ]

replaceElementInList :: [a] -> Int -> a -> [a]
replaceElementInList row idx num = (take idx row) ++ (num : (drop (idx + 1) row))

rowContainsNum :: Board -> Int -> Int -> Bool
rowContainsNum board row num = elem num (board !! row)

colContainsNum :: Board -> Int -> Int -> Bool
colContainsNum board col num = elem num [board !! row !! col | row <- [0 .. 8]]

blockContainsNum :: Board -> Action -> Bool
blockContainsNum board ((row, col), num) =
  any (elem num) (getBlockCols (getBlockRows board row) col)

getBlockRows :: Board -> Int -> Board
getBlockRows board row
  | row >= 0 && row <= 2 = take 3 board
  | row >= 3 && row <= 5 = take 3 (drop 3 board)
  | otherwise = drop 6 board

getBlockCols :: Board -> Int -> Board
getBlockCols board col
  | col >= 0 && col <= 2 = map (take 3) board
  | col >= 3 && col <= 5 = map (take 3 . drop 3) board
  | otherwise = map (drop 6) board

boardToString :: Board -> String
boardToString [] = "-------------------------------------\n"
boardToString (h : t) = boardToString [] ++ rowToString h ++ "\n" ++ boardToString t

rowToString :: [Int] -> String
rowToString [] = "| "
rowToString (h : t) = rowToString [] ++ show h ++ " " ++ rowToString t

actionToString :: Action -> String
actionToString (pos, num) = "Placed " ++ show num ++ " at position " ++ show pos
