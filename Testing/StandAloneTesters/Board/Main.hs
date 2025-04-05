module Main where

import Board(loadBoard, isValidPosition, printBoard, validateBoard)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            loadedBoard <- loadBoard filename
            let isValid = validateBoard loadedBoard
            putStrLn $ "Is the board valid? " ++ show isValid
            putStrLn "Board:"
            printBoard loadedBoard
        _ -> putStrLn "Usage: ./main <board_file.txt>"