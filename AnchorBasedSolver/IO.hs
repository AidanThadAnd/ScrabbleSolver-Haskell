{-|
Module      : IO
Description : Handles Input/Output operations, specifically dictionary loading.

This module contains IO functions needed by the solver, focusing
on loading the dictionary file into the Trie structure. 
Includes basic error handling for file operations.
-}
module IO (loadDictionary) where

import Trie (Trie, insertWords, emptyTrie) 
import System.IO (readFile, putStrLn)
import System.IO.Error (catchIOError, isDoesNotExistError)
import Control.Exception (IOException)

-- | Loads a dictionary from a specified file path into a Trie.
--   Reads the file, treating each line as a separate word.
--   Handles errors by printing an error message and returning an empty Trie.
loadDictionary :: FilePath -- ^ The path to the dictionary file.
               -> IO Trie  -- ^ The resulting Trie within the IO monad.
loadDictionary filename =
    -- Attempt to read the file and process it
    catchIOError (do
        -- Read the entire file content as a single String
        contents <- readFile filename
        -- Split the content into a list of lines
        let wordsList = lines contents
        -- Insert the words into a new Trie
        let dictionaryTrie = insertWords wordsList
        return $! dictionaryTrie
      )
      handleLoadError
    where
      -- | Handles IO errors.
      handleLoadError :: IOException -> IO Trie
      handleLoadError e = do
          putStrLn $ "\nError loading dictionary file '" ++ filename ++ "': " ++ show e
          if isDoesNotExistError e then
              putStrLn "Please ensure the dictionary file exists."
          else
              return ()
          putStrLn "Error loading dictionary"
          return emptyTrie