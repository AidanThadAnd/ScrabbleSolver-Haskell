{-# LANGUAGE OverloadedStrings #-}

module Solver (solveScrabble) where

import Board (Square(..), letter, boardSize, loadBoard, printBoard, isValidPosition)
import Trie (Trie, searchWord)
import IO (loadDictionary)
import Data.Array (Array, (!), assocs)
import Data.List (maximumBy, permutations, subsequences, sort)
import Data.Ord (comparing)
import qualified Data.Set as Set
import System.IO (readFile)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)

-- Define the score for each letter
letterScores :: [(Char, Int)]
letterScores = [('A', 1), ('B', 3), ('C', 3), ('D', 2), ('E', 1), ('F', 4), ('G', 2), ('H', 4), ('I', 1), ('J', 8), ('K', 5), ('L', 1), ('M', 3), ('N', 1), ('O', 1), ('P', 3), ('Q', 10), ('R', 1), ('S', 1), ('T', 1), ('U', 1), ('V', 4), ('W', 4), ('X', 8), ('Y', 4), ('Z', 10)]

-- Helper function to get the score of a letter
getLetterScore :: Char -> Int
getLetterScore c = fromMaybe 0 (lookup (toUpper c) letterScores) -- Handle unknown characters

-- Calculate the score of a word on the board
calculateWordScore :: Array (Int, Int) Square -> [(Int, Int, Char)] -> Int
calculateWordScore board placements = sum $ map calculateSquareScore placements
  where
    calculateSquareScore (r, c, ch) =
      let square = board ! (r, c)
      in if letter square == ' ' then getLetterScore ch else getLetterScore (letter square)

-- Generate all possible plays from a rack of letters
generatePlays :: String -> [String]
generatePlays rack = concatMap permutations (subsequences rack)

-- Helper Functions
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- Find all valid plays on the board
findValidPlays :: Array (Int, Int) Square -> Trie -> String -> [(Int, Int, Char)] -> [([(Int, Int, Char)], Int)]
findValidPlays board dictionary rack placements =
  let
    word = map (\(_, _, c) -> c) placements
    isHorizontal = all (\(r, _, _) -> r == fst3 (head placements)) placements
    isVertical = all (\(_, c, _) -> c == snd3 (head placements)) placements

    isValidWord = searchWord dictionary word
    isValidPlacements = all (\(r, c, _) -> isValidPosition r c && (letter (board ! (r, c)) == ' ' || letter (board ! (r, c)) == toUpper (last [x | (_, _, x) <- filter (\(row, col, _) -> row == r && col == c) placements]))) placements

    isContinuous
      | isHorizontal = all (\(r, c, _) -> isValidPosition r c) [(fst3 (head placements), c', ' ') | c' <- [snd3 (head placements)..snd3 (last placements)]]
      | isVertical = all (\(r, c, _) -> isValidPosition r c) [(r', snd3 (head placements), ' ') | r' <- [fst3 (head placements)..fst3 (last placements)]]
      | otherwise = True

    isAdjacent
      | isHorizontal = any (\(r, c, _) -> any (\(dr, dc) -> isValidPosition (r + dr) (c + dc) && letter (board ! (r + dr, c + dc)) /= ' ') [(-1, 0), (1, 0), (0, -1), (0, 1)]) placements
      | isVertical = any (\(r, c, _) -> any (\(dr, dc) -> isValidPosition (r + dr) (c + dc) && letter (board ! (r + dr, c + dc)) /= ' ') [(-1, 0), (1, 0), (0, -1), (0, 1)]) placements
      | otherwise = True
  in
    [(placements, calculateWordScore board placements) | isValidWord && isValidPlacements && isContinuous && (not (null placements) && (letter (board ! (7, 7)) /= ' ' || any (\(r, c, _) -> r == 7 && c == 7) placements)) && isAdjacent]

-- Generate all possible starting positions
generateStartingPositions :: Array (Int, Int) Square -> [(Int, Int)]
generateStartingPositions board = [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], letter (board ! (r, c)) == ' ']

-- Generate all possible plays on the board
findAllPlays :: Array (Int, Int) Square -> Trie -> String -> [([(Int, Int, Char)], Int)]
findAllPlays board dictionary rack =
  let
    startingPositions = generateStartingPositions board
    plays = generatePlays rack
    allPlays = concatMap (\startPos -> concatMap (generatePlacements board dictionary rack startPos) plays) startingPositions
  in
    allPlays

generatePlacements :: Array (Int, Int) Square -> Trie -> String -> (Int, Int) -> String -> [([(Int, Int, Char)], Int)]
generatePlacements board dictionary rack (r, c) play =
  let
    horizontalPlacements = zipWith (\offset ch -> (r, c + offset, ch)) [0..] play
    verticalPlacements = zipWith (\offset ch -> (r + offset, c, ch)) [0..] play
  in
    findValidPlays board dictionary rack horizontalPlacements ++ findValidPlays board dictionary rack verticalPlacements

-- Find the highest scoring play
findHighestScoringPlay :: Array (Int, Int) Square -> Trie -> String -> Maybe ([(Int, Int, Char)], Int)
findHighestScoringPlay board dictionary rack =
  case findAllPlays board dictionary rack of
    [] -> Nothing
    plays -> Just (maximumBy (comparing snd) plays)

-- Solve that shit
solveScrabble :: FilePath -> FilePath -> FilePath -> IO (Maybe ([(Int, Int, Char)], Int))
solveScrabble boardFile dictionaryFile rackFile = do
  board <- loadBoard boardFile
  dictionary <- loadDictionary dictionaryFile
  rack <- readFile rackFile
  let rackString = head (lines rack)
  return (findHighestScoringPlay board dictionary rackString)
