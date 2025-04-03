

module Trie (Trie, emptyTrie, insertWords, searchWord) where

import qualified Data.Trie as T
import qualified Data.ByteString.Char8 as BS

-- Type alias for readability
type Trie = T.Trie ()

-- Create an empty trie
emptyTrie :: Trie
emptyTrie = T.empty

-- Insert a list of words into the trie
insertWords :: [String] -> Trie
insertWords = foldr (\word trie -> T.insert (BS.pack word) () trie) emptyTrie

-- Check if a word exists in the trie
searchWord :: Trie -> String -> Bool
searchWord trie word = T.member (BS.pack word) trie

