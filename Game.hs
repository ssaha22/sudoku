module Game where

type Board = [[Int]]

type Position = (Int, Int)

type Action = (Position, Int)

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
