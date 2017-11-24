-- Matrix operations
import Data.Matrix
-- List operations (elemIndex)
import Data.List
-- For elemIndex
import Data.Maybe
-- For splitting the list into chunks for printing
import Data.List.Split

import System.Random.Shuffle

import Text.Read

-- The board is a list of Ints, from 1-16 (16 being the empty space)
type Board = [Int]

initialBoard = [12, 14, 1, 2, 5, 6, 3, 10, 9, 13, 8, 11, 4, 7, 15, 16]

-- To play the game,
--   explain the rules,
--   play from the initial board
main = do
  putStrLn "\nWelcome to 15 Puzzle!\n"
  putStrLn "Your goal is to get the tiles 1 through 15 into ascending order, and the empty space in the bottom right corner.\n"
  putStrLn "You can do that by moving one tile at a time into the empty space. To move a tile into the space, enter the number on the tile.\n"
  putStrLn "Ready?\n"
  board <- randomBoard
  displayBoard board
  play board

-- On each turn,
--   check if the game is over,
--   get the user's next move,
--   check that the move is valid,
--   update the game board,
--   go to the next turn
play :: Board -> IO()
play board = do
  if (isFinished board) then do
    putStrLn "\nCongratulations, you won!\n"
    return ()
  else do
    move <- getLineInt
    if not (isValidMove move board) then do
      putStrLn "\nInvalid move, please enter a valid move\n"
      play board
    else do
      let newBoard = updateBoard move board
      displayBoard newBoard
      play newBoard

getLineInt :: IO Int
getLineInt = do
  putStrLn "Enter a tile to move: "
  line <- getLine
  case readMaybe line :: Maybe Int of
    Just x -> return x
    Nothing -> putStrLn "\nInvalid move, please enter a valid move\n" >> getLineInt

-- Return True if the move is valid, that is,
--   if the specified tile is next to the empty tile
isValidMove :: Int -> Board -> Bool
isValidMove move board =
  -- get the matrix representation of the board
  if move > 15 then
    False
  else
    let boardAsMatrix = fromList 4 4 board
        indexOfMove = fromJust $ elemIndex move board
        rowOfMove = indexOfMove `div` 4
        columnOfMove = indexOfMove `mod` 4
        indexOfEmpty = fromJust $ elemIndex 16 board
        rowOfEmpty = indexOfEmpty `div` 4
        columnOfEmpty = indexOfEmpty `mod` 4
    in isValidMoveHelper rowOfMove columnOfMove rowOfEmpty columnOfEmpty

-- Return True if (x, y) is row or column adjacent to (i, j)
isValidMoveHelper :: Int -> Int -> Int -> Int -> Bool
isValidMoveHelper x y i j =
  -- (x, y) is (i, j-1), (x, y) is above (i, j)
  (x == i && y == j-1) ||

  -- (x, y) is (i, j+1), (x, y) is below (i, j)
  (x == i && y == j+1) ||

  -- (x, y) is (i-1, j), (x, y) is to the left of (i, j)
  (x == i-1 && y == j) ||

  -- (x, y) is (i+1, j), (x, y) is to the right of (i, j)
  (x == i+1 && y == j)

-- Update the board by swapping the empty space with the chosen tile
updateBoard :: Int -> Board -> Board
updateBoard move board =
  let empty = fromJust $ elemIndex 16 board
  in swap move 16 board

-- Swaps (by element, not index) a and b in the given list (with distinct elements)
swap :: Int -> Int -> Board -> Board
swap a b lst =
  map (\x -> if x == a then b else if x == b then a else x) lst

-- Return True if the board is sorted
isFinished :: Board -> Bool
isFinished board = sorted board

-- Return True if the list is sorted
sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = x <= y && sorted (y:xs)

-- Pretty-prints the game board
displayBoard board =
  putStrLn (boardStr board)

boardStr :: Board -> String
boardStr board =
  -- Put it together
  "+----+----+----+----+\n" ++ (intercalate rowSep $ rows) ++ "\n+----+----+----+----+"
  where
    boardWithChars = map (\x -> show x) board
    boardWithEmpty = map (\x -> if x == "16" then " " else x) boardWithChars
    drawableBoard = chunksOf 4 boardWithEmpty
    -- Place vertical bars between the tiles
    rowStr        = intercalate " | " . map (\x -> if (length x) == 1 then " " ++ x else x)
    -- Generate row separators ("---+----+----+---")
    rowSep        = "\n+----+----+----+----+\n"
    rows = map (\x -> "| " ++ rowStr x ++ " |") drawableBoard


-- Generates a random, solvable board
randomBoard :: IO Board
randomBoard = do
  board <- shuffleM [1..16]
  if solvable board
    then
      return board
    else
      randomBoard

-- A board is solvable if the empty space is in an odd row
-- and the number of inversions is even, or the empty space
-- is in an even row and the number of inversions is odd
solvable :: Board -> Bool
solvable board =
  let indexOfEmpty = fromJust $ elemIndex 16 board
      rowOfEmpty = indexOfEmpty `div` 4
  in (rowOfEmpty `mod` 2) /= ((numInversions board) `mod` 2)

-- Counts the number of inversions in a list
numInversions :: [Int] -> Int
numInversions [x] = 0
numInversions (h:t) =
  let sum = numInversionsHelper h t
  in sum + numInversions t

-- Counts the number of elements in a list that are greater than a given value
numInversionsHelper :: Int -> [Int] -> Int
numInversionsHelper _ [] = 0
numInversionsHelper x (h:t)
  | x > h = 1 + numInversionsHelper x t
  | x <= h = numInversionsHelper x t

-- Swaps two elements in a unique list by index
swapByIndex :: Int -> Int -> Board -> Board
swapByIndex i j list =
  swap (list!!i) (list!!j) list
