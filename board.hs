module Board (loadBoard, isValidPosition, printBoard, validateBoard, boardSize, Square(..), letter) where

import qualified Data.Array as A        -- Import the Data.Array module for working with arrays
import qualified Data.Set as Set          -- Import the Data.Set module for working with sets
import System.IO.Error (catchIOError, isDoesNotExistError) -- Import error handling functions
import Data.List (any)                  -- Import the 'any' function for checking if any element in a list satisfies a condition

-- Define constants
boardSize :: Int
boardSize = 15                         -- Define the size of the board (15x15)

center :: Int
center = 7                             -- Define the center index of the board

-- Define the Square data type
data Square = Square {
    letter :: Char,                    -- The letter on the square (' ' for empty)
    validPlacement :: Bool             -- Indicates if a square is a valid placement for a new tile
}

-- Load the board from a file
loadBoard :: FilePath -> IO (A.Array (Int, Int) Square)
loadBoard filename = do
    contents <- readFile filename `catchIOError` (\e -> if isDoesNotExistError e then error "File not found" else ioError e) -- Read the file contents, handling file not found errors
    let linesBoard = lines contents     -- Split the file contents into lines.
    let squares = [((r, c), Square (if (linesBoard !! r) !! c == '_' then ' ' else (linesBoard !! r) !! c) False) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]] -- Create a list of squares from the file data
    let updatedSquares = map (\((r, c), sq) -> ((r, c), sq { validPlacement = isValidPlacement squares r c })) squares -- Update each square's validPlacement
    return $ A.array ((0, 0), (boardSize - 1, boardSize - 1)) updatedSquares -- Create and return an array from the updated squares

-- Helper function to determine valid placement for a square
isValidPlacement :: [((Int, Int), Square)] -> Int -> Int -> Bool
isValidPlacement squares row col =
    if isBoardEmpty squares then row == center && col == center -- If the board is empty, only the center square is a valid placement
    else
        let currentSquare = lookupSquare squares row col -- Get the current square
        in letter currentSquare == ' ' && (row == center && col == center || any (\(dr, dc) -> isValidPosition (row + dr) (col + dc) && letter (lookupSquare squares (row + dr) (col + dc)) /= ' ') [(-1, 0), (1, 0), (0, -1), (0, 1)]) --Check if the empty square is the center or adjacent to a letter

-- Helper function to lookup a square
lookupSquare :: [((Int, Int), Square)] -> Int -> Int -> Square
lookupSquare squares row col = case lookup (row, col) squares of -- Look up a square
    Just sq -> sq
    Nothing -> Square ' ' False -- Return an empty square if not found

-- Helper function to check if board is empty
isBoardEmpty :: [((Int, Int), Square)] -> Bool
isBoardEmpty = all (\sq -> letter (snd sq) == ' ') -- Check if all squares are empty

-- Check if a position is within bounds
isValidPosition :: Int -> Int -> Bool
isValidPosition row col = row >= 0 && row < boardSize && col >= 0 && col < boardSize -- Check if the row and column are within the board bounds

-- Print the board
printBoard :: A.Array (Int, Int) Square -> IO ()
printBoard board = mapM_ printRow [0 .. boardSize - 1] -- Print each row of the board
  where
    printRow r = putStrLn $ unwords [if letter (board A.! (r, c)) == ' ' then "_" else [letter (board A.! (r, c))] | c <- [0 .. boardSize - 1]] -- Print a single row

-- Validate if the board is empty or connected
validateBoard :: A.Array (Int, Int) Square -> Bool
validateBoard board
    | isBoardEmpty (A.assocs board) = True -- If the board is empty, it's valid
    | otherwise = isValidCenter board && isBoardConnected board -- Otherwise, check if the center square is used and all letters are connected
  where
    isValidCenter b = letter (b A.! (center, center)) /= ' ' -- Check if the center square has a letter
    isBoardConnected b = checkConnected b (center, center) Set.empty -- Check if all letters are connected

-- Check if the board is connected using DFS
checkConnected :: A.Array (Int, Int) Square -> (Int, Int) -> Set.Set (Int, Int) -> Bool
checkConnected board start visited =
    Set.size (dfs startingPoint Set.empty) == totalNonEmptySquares board -- Check if the number of visited squares equals the number of non-empty squares
  where
    totalNonEmptySquares b = length [() | square <- A.elems b, letter square /= ' '] -- Count the number of non-empty squares

    -- DFS Traversal
    dfs :: (Int, Int) -> Set.Set (Int, Int) -> Set.Set (Int, Int)
    dfs (r, c) visited
        | not (isValidPosition r c) = visited -- If the position is out of bounds, return the visited set
        | Set.member (r, c) visited = visited -- If the position has already been visited, return the visited set
        | letter (board A.! (r, c)) == ' ' = visited -- If the square is empty, return the visited set
        | otherwise = foldr (\(dr, dc) acc -> dfs (r + dr, c + dc) acc) (Set.insert (r, c) visited) -- Otherwise, recursively call dfs on adjacent squares
            [(-1, 0), (1, 0), (0, -1), (0, 1)] -- Directions: up, down, left, right

    -- Find the starting point for DFS
    startingPoint = findStartingPoint board

    findStartingPoint :: A.Array (Int, Int) Square -> (Int, Int)
    findStartingPoint b = head [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], letter (b A.! (r, c)) /= ' '] -- Find the first non-empty square