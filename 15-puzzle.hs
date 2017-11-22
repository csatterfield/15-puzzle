-- Matrix operations
import Data.Matrix
-- List operations (elemIndex)
import Data.List
-- For elemIndex
import Data.Maybe

-- The board is a list of Ints, from 1-16 (16 being the empty space)
type Board = [Int]

initialBoard = [12, 14, 1, 2, 5, 6, 3, 10, 9, 13, 8, 11, 4, 7, 15, 16]

-- To play the game,
--   explain the rules,
--   play from the initial board
main = do
  putStrLn "\nWelcome to 15 Puzzle!\n"
  putStrLn "\nYour goal is to get the tiles 1 through 15 into ascending order, and the empty space in the bottom right corner.\n"
  putStrLn "\nYou can do that by moving one tile at a time into the empty space. To move a tile into the space, enter the number on the tile.\n"
  putStrLn "\nReady?\n"
  displayBoard initialBoard
  play initialBoard

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
    move <- getLine
    if not (isValidMove (read move :: Int) board) then do
      putStrLn "\nInvalid move, please enter a valid move\n"
      play board
    else do
    let newBoard = updateBoard (read move :: Int) board
    displayBoard newBoard
    play newBoard

-- Return True if the move is valid, that is,
--   if the specified tile is next to the empty tile
isValidMove :: Int -> Board -> Bool
isValidMove move board =
  -- get the matrix representation of the board
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

-- Pretty-prints the board
displayBoard board =
  let boardWithChars = map (\x -> show x) board
      boardWithEmpty = map (\x -> if x == "16" then " " else x) boardWithChars
  in putStrLn $ show (fromList 4 4 boardWithEmpty)
