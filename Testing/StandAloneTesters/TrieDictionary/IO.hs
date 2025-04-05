module IO (loadDictionary) where

import Trie (Trie, insertWords)
import System.IO (readFile)

-- Load words from a file into a trie
loadDictionary :: FilePath -> IO Trie
loadDictionary filename = do
    contents <- readFile filename
    let wordsList = lines contents
    return (insertWords wordsList)