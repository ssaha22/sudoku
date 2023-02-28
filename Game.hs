module Game where

-- a valid game board is a 9x9 matrix of Ints
type Board = [[Int]]

-- a position on the game board
type Position = (Int, Int)

-- an action is a position and an Int to be placed on the position
type Action = (Position, Int)

-- checks if an action is valid for the given board
isValidAction :: Board -> Action -> Bool
isValidAction board ((row, col), num)
  | row < 0 || row > 8 = False
  | col < 0 || col > 8 = False
  | num < 1 || num > 9 = False
  | rowContainsNum board row num = False
  | colContainsNum board col num = False
  | blockContainsNum board ((row, col), num) = False
  | otherwise = True

-- places the num in position (row, col) on the board
performAction :: Board -> Action -> Board
performAction board ((row, col), num) =
  replaceElementInList board row (replaceElementInList (board !! row) col num)

-- given a board and its solution, changes the first empty position on the board to the correct number
applyHint :: Board -> Board -> (Board, Action)
applyHint board solution =
  head
    [ ( performAction
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

-- replaces the element at the given index in the list with the new element
replaceElementInList :: [a] -> Int -> a -> [a]
replaceElementInList row idx num = (take idx row) ++ (num : (drop (idx + 1) row))

-- produces True if the given row on the board contains the number
rowContainsNum :: Board -> Int -> Int -> Bool
rowContainsNum board row num = elem num (board !! row)

-- produces True if the given column on the board contains the number
colContainsNum :: Board -> Int -> Int -> Bool
colContainsNum board col num = elem num [board !! row !! col | row <- [0 .. 8]]

-- produces True if the 3x3 block around (row, col) on the board contains the number
blockContainsNum :: Board -> Action -> Bool
blockContainsNum board ((row, col), num) =
  any (elem num) (getBlockCols (getBlockRows board row) col)

-- produces the 3 rows of the 3x9 block in which the given row is contained
getBlockRows :: Board -> Int -> Board
getBlockRows board row
  | row >= 0 && row <= 2 = take 3 board
  | row >= 3 && row <= 5 = take 3 (drop 3 board)
  | otherwise = drop 6 board

-- produces the 3 columns of the 9x3 block in which the given column is contained
getBlockCols :: Board -> Int -> Board
getBlockCols board col
  | col >= 0 && col <= 2 = map (take 3) board
  | col >= 3 && col <= 5 = map (take 3 . drop 3) board
  | otherwise = map (drop 6) board

-- produces a string representation of the board
boardToString :: Board -> String
boardToString [] = "-------------------------------------\n"
boardToString (h : t) = boardToString [] ++ rowToString h ++ "\n" ++ boardToString t

-- produces a string representation of a board's row
rowToString :: [Int] -> String
rowToString [] = "| "
rowToString (h : t) = rowToString [] ++ show h ++ " " ++ rowToString t

-- produces a string representation of an action
actionToString :: Action -> String
actionToString (pos, num) = "Placed " ++ show num ++ " at position " ++ show pos
