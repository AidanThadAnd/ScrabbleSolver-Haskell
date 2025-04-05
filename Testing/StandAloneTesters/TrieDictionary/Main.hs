module Main where

import System.Environment (getArgs)
import IO (loadDictionary)
import Trie (Trie, emptyTrie, insertWords, searchWord)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            trie <- loadDictionary filename
            putStrLn "Dictionary loaded. Enter words to check (type 'exit' to quit):"
            userLoop trie
        _ -> putStrLn "Usage: ./main <dictionary_file>"

userLoop :: Trie -> IO ()
userLoop trie = do
    putStrLn "Enter word: "
    word <- getLine
    if word == "exit"
        then putStrLn "Exiting..."
        else do
            putStrLn $ if searchWord trie word then "Word exists!" else "Word not found."
            userLoop trie