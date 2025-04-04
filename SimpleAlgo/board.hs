{-|
Module      : Board
Description : Defines the Scrabble board representation and related functions.

This module defines the data types for the Scrabble board ('Square', 'Bonus', 'Coord')
and provides functions for loading a board state from a file, validating board properties
(like size, connectivity, center usage), checking position validity, and printing the board.
It also includes the definitions for standard Scrabble bonus square locations.
-}
module Board (loadBoard, isValidPosition, validateBoard, boardSize, isBoardEmpty, Square(..), Bonus(..), Coord, printBoard) where

import qualified Data.Array as A
import qualified Data.Set as Set
import System.IO (readFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import Data.List (any, find)
import Data.Maybe (fromMaybe, listToMaybe)

-- | Standard Scrabble board dimension (15x15).
boardSize :: Int
boardSize = 15

-- | Index of the center row/column.
center :: Int
center = 7

-- | Represents the bonus multiplier types.
data Bonus = None | DL | TL | DW | TW deriving (Show, Eq)

-- | Represents a single square on the Scrabble board, containing a letter (or empty)
--   and the potential bonus type associated with the square (active only when empty).
data Square = Square {
    letter :: Char,    -- ^ The letter on the square (' ' for empty).
    bonus :: Bonus     -- ^ The bonus multiplier for this square.
} deriving (Show)

-- | Type alias for board coordinates (Row, Column).
type Coord = (Int, Int)

-- | Type alias for a set of coordinates.
type CoordSet = Set.Set Coord

-- | Set of coordinates for double letter bonus squares.
dlCoords :: CoordSet
dlCoords = Set.fromList [(0,3), (0,11), (2,6), (2,8), (3,0), (3,7), (3,14), (6,2), (6,6), (6,8), (6,12), (7,3), (7,11), (8,2), (8,6), (8,8), (8,12), (11,0), (11,7), (11,14), (12,6), (12,8), (14,3), (14,11)]

-- | Set of coordinates for triple letter bonus squares.
tlCoords :: CoordSet
tlCoords = Set.fromList [(1,5), (1,9), (5,1), (5,5), (5,9), (5,13), (9,1), (9,5), (9,9), (9,13), (13,5), (13,9)]

-- | Set of coordinates for double word bonus squares.
dwCoords :: CoordSet
dwCoords = Set.fromList [(1,1), (2,2), (3,3), (4,4), (1,13), (2,12), (3,11), (4,10), (7,7), (10,4), (11,3), (12,2), (13,1), (10,10), (11,11), (12,12), (13,13)]

-- | Set of coordinates for triple word bonus squares.
twCoords :: CoordSet
twCoords = Set.fromList [(0,0), (0,7), (0,14), (7,0), (7,14), (14,0), (14,7), (14,14)]

-- | Determines the Bonus type for a given board coordinate.
getBonusType :: Coord -- ^ The (row, col) coordinate to check.
             -> Bonus -- ^ The bonus type.
getBonusType coord
    | coord `Set.member` twCoords = TW
    | coord `Set.member` dwCoords = DW
    | coord `Set.member` tlCoords = TL
    | coord `Set.member` dlCoords = DL
    | otherwise                   = None

-- | Loads a board state from a text file.
--   Expects a 15x15 grid of characters.
--   '_' or '.' represent empty squares. Any other character represents a placed letter.
--   Assigns bonus types to squares that are empty based on standard Scrabble layout.
--   Throws an error if the file is not found or has incorrect dimensions.
loadBoard :: FilePath -- ^ Path to the board file.
          -> IO (A.Array Coord Square) -- ^ The loaded board as an Array.
loadBoard filename = do
    contents <- readFile filename `catchIOError` handleReadError
    let linesBoard = lines contents
    if length linesBoard /= boardSize || any (\line -> length line /= boardSize) linesBoard
      then error $ "Invalid board dimensions in " ++ filename ++ ". Expected " ++ show boardSize ++ "x" ++ show boardSize
      else do
        -- Create a list of ((row, col), Square)
        let squares = [ ((r, c), determineSquare r c charFromFile)
                      | r <- [0 .. boardSize - 1]
                      , c <- [0 .. boardSize - 1]
                      , let charFromFile = (linesBoard !! r) !! c
                      ]
        -- Create the Array
        return $ A.array ((0, 0), (boardSize - 1, boardSize - 1)) squares
  where
    -- | Handle file reading errors.
    handleReadError :: IOError -> IO String
    handleReadError e
      | isDoesNotExistError e = error ("Board file not found: " ++ filename)
      | otherwise             = ioError e

    -- | Parses a character from the file into a board character (' ' for empty).
    parseChar :: Char -> Char
    parseChar c | c == '_' || c == '.' = ' '
                | otherwise            = c

    -- | Determines the initial state for a square based on file input.
    determineSquare :: Int -> Int -> Char -> Square
    determineSquare r c charFromFile =
        let parsedLetter = parseChar charFromFile
            coord = (r, c)
        in if parsedLetter /= ' ' then
             -- If square has a letter, its bonus is considered inactive
             Square { letter = parsedLetter, bonus = None }
           else
             -- If square is empty, determine if it has a bonus type
             Square { letter = ' ', bonus = getBonusType coord }


-- | Checks if a given (Row, Column) coordinate is within the bounds of the board.
isValidPosition :: Int -- ^ Row index.
                -> Int -- ^ Column index.
                -> Bool -- ^ True if the position is on the board, False otherwise.
isValidPosition row col =
    row >= 0 && row < boardSize && col >= 0 && col < boardSize

-- | Checks if the board array contains any letters.
isBoardEmpty :: A.Array Coord Square -- ^ The board array.
             -> Bool                 -- ^ True if all squares have letter ' ', False otherwise.
isBoardEmpty board = all (\(_, sq) -> letter sq == ' ') (A.assocs board)

-- | Prints the board.
--   Empty squares are shown as '_'.
printBoard :: A.Array Coord Square -- ^ The board array to print.
           -> IO ()
printBoard board = mapM_ printRow [0 .. boardSize - 1]
  where
    -- | Prints a single row of the board.
    printRow :: Int -> IO ()
    printRow r = putStrLn $ unwords [ displayChar (board A.! (r, c)) 
                                   | c <- [0 .. boardSize - 1] ]
    -- | Gets the character for a square.
    displayChar :: Square -> String
    displayChar sq = if letter sq == ' ' then "_" else [letter sq]

-- | Validates the state of a loaded board according to Scrabble rules:
--   An empty board is valid.
--   A non-empty board is valid only if the center square is occupied and all
--   placed tiles are connected.
validateBoard :: A.Array Coord Square -- ^ The board array to validate.
              -> Bool                 -- ^ True if the board state is valid, False otherwise.
validateBoard board
    | isBoardEmpty board = True -- Empty board is always valid
    | otherwise = isValidCenter board && isBoardConnected board -- Non-empty checks
  where
    -- | Checks if the center square contains a letter.
    isValidCenter :: A.Array Coord Square -> Bool
    isValidCenter b = letter (b A.! (center, center)) /= ' ' 

    -- | Checks if all letters on the board are connected.
    isBoardConnected :: A.Array Coord Square -> Bool
    isBoardConnected b = checkConnected b

-- | Helper for validateBoard. Checks connectivity using Depth First Search.
--   Uses findStartingPoint to start the search.
checkConnected :: A.Array Coord Square -- ^ The board array.
               -> Bool                 -- ^ True if all tiles are connected, False otherwise.
checkConnected board =
   -- Find the first tile on the board to start the DFS from.
   case findStartingPoint board of
     Nothing -> isBoardEmpty board
     -- If a starting tile is found, perform DFS and compare visited count.
     Just startCoord -> Set.size (dfs startCoord Set.empty) == totalNonEmptySquares board
  where
    -- | Counts the total number of squares with letters on the board.
    totalNonEmptySquares :: A.Array Coord Square -> Int
    totalNonEmptySquares b = length [() | square <- A.elems b, letter square /= ' '] 

    -- | Performs Depth First Search starting from a coordinate.
    dfs :: Coord -> Set.Set Coord -> Set.Set Coord
    dfs currentPos@(r, c) visitedSet
        -- Base case: Stop if off-board
        | not (isValidPosition r c) = visitedSet
        -- Base case: Stop if already visited
        | Set.member currentPos visitedSet = visitedSet
        -- Base case: Stop if the square is empty
        | letter (board A.! currentPos) == ' ' = visitedSet 
        -- Recursive step: Insert current, explore neighbors
        | otherwise = foldr (\(dr, dc) currentVisitedSet -> dfs (r + dr, c + dc) currentVisitedSet)
                           (Set.insert currentPos visitedSet)
                           [(-1, 0), (1, 0), (0, -1), (0, 1)] -- Neighbor directions

    -- | Finds the coordinate of the first non-empty square.
    findStartingPoint :: A.Array Coord Square -> Maybe Coord
    findStartingPoint b = fmap fst $ find (\(_, sq) -> letter sq /= ' ') (A.assocs b)
