{-|
Module      : Main
Description : Command-line executable for the Scrabble Solver.

This module provides the main entry point for the Scrabble solver.
It handles parsing command-line arguments (board file, dictionary file, rack string),
loading the initial board state, validating the initial board, invoking the core
solver logic, and printing the results, including the best move found and the
resulting board configuration.
-}
module Main where


import Solver (solveScrabble, Move(..), Direction(..))
import Board (Square(..), loadBoard, printBoard, Bonus(..), letter, bonus, validateBoard)
import System.Environment (getArgs)      
import System.IO                         
import qualified Data.Array as A        

-- | Applies a found move to an existing board state.
--   This function assumes the move placement is valid according to game rules
--   When placing a tile, any bonus on that square is considered consumed.
applyMove :: A.Array (Int, Int) Square -- ^ The board state before the move.
          -> [(Int, Int, Char)]        -- ^ The list of tiles placed this turn: [(row, col, char)]
          -> A.Array (Int, Int) Square -- ^ The new board state after the move.
applyMove board placement = board A.// updates
    where
        -- Create a list of updates for the array operator: [((index), newValue)]
        updates :: [((Int, Int), Square)]
        updates = map (\(r, c, char) ->
                        -- The new Square has the placed letter and no active bonus.
                        ((r, c), Square {letter = char, bonus = None})
                    ) placement

-- | The main entry point for the Scrabble solver.
--   Expects three command-line arguments: board_file dictionary_file rack_string
main :: IO ()
main = do
    args <- getArgs -- Get command line arguments

    -- Validate and parse arguments
    case args of
        -- Correct number of arguments provided
        [boardFile, dictionaryFile, rackString] -> do
            -- Attempt to load the initial board from file
            initialBoard <- loadBoard boardFile

            -- Validate the initial board state
            if not (validateBoard initialBoard) then
                putStrLn $ "Error: Input board file '" ++ boardFile ++ "' represents an invalid Scrabble state (e.g., not connected or center not used)."
            else do
                -- Call the solver function
                result <- solveScrabble initialBoard dictionaryFile rackString

                -- Process the result
                case result of
                    -- A best move was found
                    Just moveData -> do
                        putStrLn $ "\nBest word: " ++ mainWord moveData
                        putStrLn $ "Start Pos: " ++ show (startPos moveData)
                        putStrLn $ "Direction: " ++ show (direction moveData)
                        putStrLn "Tiles Placed:"
                        -- Print each placed tile individually
                        mapM_ printMove (placement moveData)
                        putStrLn $ "Total score: " ++ show (score moveData)
                        putStrLn "\nResulting Board:"
                        -- Calculate the board state after applying the best move
                        let newBoard = applyMove initialBoard (placement moveData)
                        -- Print the final board state
                        printBoard newBoard

                    -- No valid moves were found
                    Nothing -> putStrLn "No valid moves found."

        -- Incorrect number of arguments provided
        _ -> putStrLn "Usage: ./main <board_file.txt> <dictionary_file.txt> <rack_string>"

-- | Prints the details of a single placed tile (row, column, character)
--   in a formatted way for the output.
printMove :: (Int, Int, Char) -- ^ A tuple representing the row, column, and character of a placed tile.
          -> IO ()
printMove (row, col, char) =
    putStrLn $ "  Row: " ++ show row ++ ", Col: " ++ show col ++ ", Char: " ++ [char]