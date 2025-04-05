{-|
Module      : Trie
Description : A module for storing and searching Scrabble dictionary words.

This module provides a Trie data structure for a Scrabble dictionary 
along with functions for Trie creation, word insertion and word lookup.
-}
module Trie (Trie, emptyTrie, insertWords, searchWord, isEmptyTrie) where

import qualified Data.Trie as T
import qualified Data.ByteString.Char8 as BS

-- | Type alias for the dictionary Trie.
--   It maps bytestring keys to unit values '()'.
--   We store '()' as we only care about the presence or absence of a word,
--   not an associated value.
type Trie = T.Trie ()

-- | Creates a new, empty dictionary Trie.
emptyTrie :: Trie -- ^ An empty Trie.
emptyTrie = T.empty

-- | Checks if the Trie is empty.
isEmptyTrie :: Trie -> Bool; isEmptyTrie = T.null

-- | Inserts a list of words into a Trie.
insertWords :: [String] -- ^ A list of words to insert.
            -> Trie     -- ^ The resulting Trie.
insertWords = foldr insertSingleWord emptyTrie
    where
      -- | Helper function to insert a single word.
      insertSingleWord :: String -> Trie -> Trie
      insertSingleWord word trie =
          -- Convert the String to bytestring before inserting into the trie.
          -- Store '()' as the value to mark the word's presence.
          T.insert (BS.pack word) () trie

-- | Checks if a given word exists in the trie.
searchWord :: Trie   -- ^ The Trie to search.
           -> String -- ^ The word to search for.
           -> Bool   -- ^ True if the word exists, False otherwise.
searchWord trie word =
    -- Convert the search string to bytestring to match the keys in the trie.
    T.member (BS.pack word) trie
