module TrieTest where

import Trie
import System.IO

main :: IO ()
main = do
  root <- createTrieNode ' '
  insertWord root "HELLO"
  insertWord root "WORLD"
  insertWord root "HASKELL"

  -- Test cases
  test1 <- searchWord root "HELLO"
  test2 <- searchWord root "WORLD"
  test3 <- searchWord root "HASKELL"
  test4 <- searchWord root "BYE"
  test5 <- searchWord root "HEL" -- Partial word
  test6 <- searchWord root "" -- empty word

  -- Print test results
  putStrLn $ "Test 1 (HELLO): " ++ show test1
  putStrLn $ "Test 2 (WORLD): " ++ show test2
  putStrLn $ "Test 3 (HASKELL): " ++ show test3
  putStrLn $ "Test 4 (BYE): " ++ show test4
  putStrLn $ "Test 5 (HEL): " ++ show test5
  putStrLn $ "Test 6 (empty word): " ++ show test6