{-# LANGUAGE OverloadedStrings #-}

module Trie where

-- ghc --package-env=. Trie.hs to compile
--
import qualified Data.Trie as T
import qualified Data.ByteString.Char8 as BS

-- Insert words into the trie
insertWords :: [String] -> T.Trie ()
insertWords wordsList = foldr (\word trie -> T.insert (BS.pack word) () trie) T.empty wordsList

-- Search for a word in the trie
searchWord :: T.Trie () -> String -> Bool
searchWord trie word = T.member (BS.pack word) trie

main :: IO ()
main = do
  let wordsList = ["HELLO", "WORLD", "HASKELL"]
      trie = insertWords wordsList

  -- Test cases
  print $ searchWord trie "HELLO"   -- True
  print $ searchWord trie "WORLD"   -- True
  print $ searchWord trie "HASKELL" -- True
  print $ searchWord trie "BYE"     -- False
  print $ searchWord trie "HEL"     -- False
  print $ searchWord trie ""        -- False

