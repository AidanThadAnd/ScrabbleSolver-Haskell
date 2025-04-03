-- Solver.hs
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Solver
Description : Core logic for finding the best Scrabble move.

This module contains the primary functions for solving a Scrabble turn.
It takes a board state, dictionary (Trie), and player rack, then attempts
to find the highest-scoring valid move according to Scrabble rules,
including crossword validation, bonus square scoring and bingo bonuses.
-}
module Solver (solveScrabble, Move(..), Direction(..)) where

import Board (Square(..), letter, boardSize, isValidPosition, Bonus(..), bonus, isBoardEmpty)
import Trie (Trie, searchWord)
import IO (loadDictionary)
import qualified Data.Array as A
import Data.List (maximumBy, minimumBy, permutations, subsequences, sort, find, minimum, maximum, nub)
import Data.Ord (comparing)
import qualified Data.Set as Set
import System.IO (readFile) 
import Data.Char (toUpper)
import Data.Maybe (fromMaybe, mapMaybe, fromJust)
import qualified Data.ByteString.Char8 as BS 

-- | Represents the details of a valid Scrabble move found by the solver.
data Move = Move {
    mainWord  :: String,         -- ^ The word formed.
    startPos  :: (Int, Int),     -- ^ The (row, col) of the first letter of the main word.
    direction :: Direction,      -- ^ The orientation (Horizontal or Vertical) of the main word.
    placement :: Placement,      -- ^ List of tiles placed this turn: `[(row, col, char)]`.
    score     :: Int             -- ^ The total calculated score for this move, including bonuses and crosswords.
} deriving (Show, Eq)

-- | Standard letter scores for Scrabble.
letterScores :: [(Char, Int)]
letterScores = [('A', 1), ('B', 3), ('C', 3), ('D', 2), ('E', 1), ('F', 4), ('G', 2), ('H', 4), ('I', 1), ('J', 8), ('K', 5), ('L', 1), ('M', 3), ('N', 1), ('O', 1), ('P', 3), ('Q', 10), ('R', 1), ('S', 1), ('T', 1), ('U', 1), ('V', 4), ('W', 4), ('X', 8), ('Y', 4), ('Z', 10)]

-- | Gets the Scrabble score for a letter.
--   Returns 0 for characters not in the standard Scrabble set.
getLetterScore :: Char -- ^ The character to score.
               -> Int  -- ^ The score, or 0 if not a standard Scrabble letter.
getLetterScore c = fromMaybe 0 (lookup (toUpper c) letterScores)

-- | The orientation of a word on the board.
data Direction = Horizontal | Vertical deriving (Show, Eq, Ord)

-- | Type alias for a placement: a list of (Row, Column, Character) tuples
--   representing tiles placed from the rack in a single turn.
type Placement = [(Int, Int, Char)]

-- | Calculates the score of a single fully formed word on the board,
--   correctly applying letter and word bonuses based on the tiles placed this turn.
calculateWordScore :: A.Array (Int, Int) Square -- ^ The current board state.
                              -> Placement                 -- ^ List of tiles placed. Used to check if a bonus applies.
                              -> String                    -- ^ The full word string.
                              -> (Int, Int)                -- ^ Start position (row, col) of the word.
                              -> Direction                 -- ^ The direction (Horizontal/Vertical) of the word.
                              -> Int                       -- ^ The calculated score for this word.
calculateWordScore board placementThisTurn fullWord startPos dir =
    let
        wordLength = length fullWord
        coords = take wordLength $ iterate nextPos startPos
          where nextPos = case dir of Horizontal -> \(r,c) -> (r, c+1); Vertical -> \(r,c) -> (r+1, c)

        scoreComponents = zipWith calculatePositionScore coords fullWord
        letterScoresWithBonuses = map fst scoreComponents
        wordMultipliers = map snd scoreComponents
        wordScore = sum letterScoresWithBonuses
        wordMultiplier = product wordMultipliers

        calculatePositionScore (r, c) char =
            let baseLetterScore = getLetterScore char
                sq = board A.! (r, c)
                sqBonus = bonus sq
                wasPlacedThisTurn = any (\(pr, pc, _) -> pr == r && pc == c) placementThisTurn
                (letterBonusValue, wordBonusValue)
                    | wasPlacedThisTurn = case sqBonus of
                        DL -> (baseLetterScore * 2, 1); TL -> (baseLetterScore * 3, 1)
                        DW -> (baseLetterScore, 2);     TW -> (baseLetterScore, 3)
                        None -> (baseLetterScore, 1)
                    | otherwise = (baseLetterScore, 1)
            in (letterBonusValue, wordBonusValue)
    in wordScore * wordMultiplier

-- | Generates all possible permutations of all subsequences of the rack.
generatePlays :: String -> [String]; generatePlays rack = nub $ filter (not . null) $ concatMap permutations (subsequences rack)

-- | Given a placed tile, finds the full perpendicular word ('crossword') it forms.
getCrossWord :: A.Array (Int, Int) Square -> Direction -> (Int, Int, Char) -> Maybe String
getCrossWord board mainDir (r, c, placedChar) =
    let crossDir = case mainDir of Horizontal -> Vertical; Vertical -> Horizontal
        (dr, dc) = case crossDir of Horizontal -> (0, 1); Vertical -> (1, 0)
        scan wordPart pos = let pPos = (fst pos - dr, snd pos - dc) in if not (isValidPosition (fst pPos) (snd pPos)) then wordPart else let sq = board A.! pPos in if letter sq == ' ' then wordPart else scan (letter sq : wordPart) pPos
        prefix = scan "" (r, c)
        scan' wordPart pos = let nPos = (fst pos + dr, snd pos + dc) in if not (isValidPosition (fst nPos) (snd nPos)) then wordPart else let sq = board A.! nPos in if letter sq == ' ' then wordPart else scan' (wordPart ++ [letter sq]) nPos
        suffix = scan' "" (r, c)
        crossWord = prefix ++ [placedChar] ++ suffix
    in if length crossWord > 1 then Just crossWord else Nothing

-- | Validates a single potential placement against all Scrabble rules.
findValidPlays :: A.Array (Int, Int) Square -- ^ The current board state.
               -> Trie                      -- ^ The dictionary Trie.
               -> Placement                 -- ^ The candidate placement.
               -> Maybe Move                -- ^ The Move details if valid, otherwise Nothing.
findValidPlays board dictionary placements =
  if null placements then Nothing else
    let
      placedWord = map (\(_, _, c) -> c) placements

      rows = map (\(r,_,_)->r) placements
      cols = map (\(_,c,_)->c) placements

      -- Determine the orientation of the placed tiles
      mainDirection :: Maybe Direction
      mainDirection
          | length placements == 1 = Just Horizontal
          | all (\r -> r == head rows) rows = Just Horizontal
          | all (\c -> c == head cols) cols = Just Vertical
          | otherwise = Nothing

      -- 1. Check placement validity
      isValidPlacement = all (\(r, c, _) -> isValidPosition r c && letter (board A.! (r, c)) == ' ') placements

      -- 2. Check for continuous placement
      isContinuous = case mainDirection of 
                         Just Horizontal -> sort cols == [minimum cols .. maximum cols]
                         Just Vertical   -> sort rows == [minimum rows .. maximum rows]
                         Nothing         -> length placements == 1

      -- 3. Find the full main word formed and its start position
      fullMainWord :: Maybe (String, (Int,Int))
      fullMainWord = case mainDirection of 
          Nothing -> if length placements == 1 then Just (placedWord, (head rows, head cols)) else Nothing
          Just dir ->
              let firstTile = minimumBy (comparing (\(r, c, _) -> if dir == Horizontal then c else r)) placements
                  lastTile  = maximumBy (comparing (\(r, c, _) -> if dir == Horizontal then c else r)) placements
                  (firstR, firstC, _) = firstTile; (lastR, lastC, _) = lastTile
                  (dr, dc) = case dir of Horizontal -> (0, 1); Vertical -> (1, 0)
                  scanBkw wordPart pos = let pPos=(fst pos-dr,snd pos-dc) in if not (isValidPosition (fst pPos) (snd pPos)) then (pos, wordPart) else let sq = board A.! pPos in if letter sq == ' ' then (pos, wordPart) else scanBkw (letter sq : wordPart) pPos
                  (actualStartPos, prefix) = scanBkw "" (firstR, firstC)
                  scanFwd wordPart pos = let nPos = (fst pos+dr, snd pos+dc) in if not (isValidPosition (fst nPos) (snd nPos)) then wordPart else let sq = board A.! nPos in if letter sq == ' ' then wordPart else scanFwd (wordPart ++ [letter sq]) nPos
                  suffix = scanFwd "" (lastR, lastC)
              in Just (prefix ++ placedWord ++ suffix, actualStartPos)

      -- 4. Check Scrabble placement rules: Adjacency or Center Start
      isAdjacentToExisting = any (\(r, c, _) -> any (\(dr, dc) -> let nr=r+dr; nc=c+dc in isValidPosition nr nc && letter (board A.! (nr, nc)) /= ' ') [(-1,0),(1,0),(0,-1),(0,1)]) placements
      isEmpty = isBoardEmpty board
      coversCenter = any (\(r, c, _) -> r == 7 && c == 7) placements

      -- Combine initial rule checks
      passesInitialChecks =
          isValidPlacement && isContinuous && mainDirection /= Nothing &&
          case fullMainWord of
            Nothing -> False
            Just (fullWord, _) -> searchWord dictionary fullWord &&
                                  ( (isEmpty && coversCenter) || (not isEmpty && isAdjacentToExisting) )

      -- Check a single crossword possibility for a placed tile
      checkSingleCrossword :: Direction -> (Int, Int, Char) -> Maybe (Bool, Maybe (String, (Int, Int), Direction))
      checkSingleCrossword mainDir placedTile@(r, c, _) =
        case getCrossWord board mainDir placedTile of
          Nothing -> Just (True, Nothing) -- No crossword formed
          Just cw -> -- Crossword was formed
            let isValidCrossword = searchWord dictionary cw
            in if isValidCrossword then
                 let -- Find details needed for scoring
                   crossDir = case mainDir of Horizontal -> Vertical; Vertical -> Horizontal
                   (dr, dc) = case crossDir of Horizontal -> (0, 1); Vertical -> (1, 0)
                   scanBackwardToStart currentPos =
                     let prevPos = (fst currentPos - dr, snd currentPos - dc)
                     in if not (isValidPosition (fst prevPos) (snd prevPos)) then currentPos
                        else let sq = board A.! prevPos in if letter sq == ' ' then currentPos else scanBackwardToStart prevPos
                   crossWordStartPos = scanBackwardToStart (r, c)
                 in Just (True, Just (cw, crossWordStartPos, crossDir))
               else
                 Just (False, Nothing) -- Invalid crossword

      -- 5. Validate crossword using checkSingleCrossword
      (allCrossWordsValid, crossWordInfos) =
          if not passesInitialChecks then (False, []) else
              let dir = fromJust mainDirection
                  crossChecks = mapMaybe (checkSingleCrossword dir) placements
              in (all fst crossChecks, mapMaybe snd crossChecks)

      -- 6. Calculate total score
      totalScore = if passesInitialChecks && allCrossWordsValid then
                      let Just (mainWordStr, mainStartPos) = fullMainWord
                          theDir = fromJust mainDirection
                          mainScore = calculateWordScore board placements mainWordStr mainStartPos theDir
                          crossScores = map (\(cw, csPos, cDir) -> calculateWordScore board placements cw csPos cDir) crossWordInfos
                          bingoBonus = if length placements == 7 then 50 else 0
                      in mainScore + sum crossScores + bingoBonus
                   else 0

    in -- Return the final result
      if passesInitialChecks && allCrossWordsValid then
          let Just (theMainWord, theStartPos) = fullMainWord
              theDirection = fromJust mainDirection
          in Just Move { mainWord = theMainWord, startPos = theStartPos, direction = theDirection,
                         placement = placements, score = totalScore }
      else Nothing


-- | Generates potential placements for a word starting at (r, c).
generatePlacement :: String -> (Int, Int) -> [[(Int, Int, Char)]]
generatePlacement wordStr (r, c) = filter checkBounds [horizontalPlacement, verticalPlacement]
  where
    horizontalPlacement = zipWith (\offset ch -> (r, c + offset, ch)) [0..] wordStr
    verticalPlacement   = zipWith (\offset ch -> (r + offset, c, ch)) [0..] wordStr
    checkBounds placement = not (null placement) && all (\(pr, pc, _) -> isValidPosition pr pc) placement

-- | Finds all valid moves for a given rack and board state.
findAllPlays :: A.Array (Int, Int) Square -> Trie -> String -> [Move]
findAllPlays board dictionary rack =
  let startingPositions = generateStartingPositions board
      possibleWords = nub $ filter (not . null) $ generatePlays rack
      allPotentialPlacements = concatMap (\word -> concatMap (generatePlacement word) startingPositions) possibleWords
      validPlays = mapMaybe (findValidPlays board dictionary) allPotentialPlacements
  in validPlays

-- | Generates a list of all empty square coordinates on the board.
generateStartingPositions :: A.Array (Int, Int) Square -> [(Int, Int)]
generateStartingPositions board = [(r, c) | r <- [0 .. A.rangeSize (A.bounds board) - 1],
                                    c <- [0 .. A.rangeSize (A.bounds board) - 1],
                                    let coord = (r,c) in isValidPosition r c && letter (board A.! coord) == ' ']

-- | Finds the highest scoring move among all valid moves found.
findHighestScoringPlay :: A.Array (Int, Int) Square -> Trie -> String -> Maybe Move
findHighestScoringPlay board dictionary rack =
  case findAllPlays board dictionary rack of
    []    -> Nothing
    plays -> Just (maximumBy (comparing score) plays)

-- | Solve that shit.
solveScrabble :: A.Array (Int, Int) Square -> FilePath -> String -> IO (Maybe Move)
solveScrabble board dictionaryFile rackString = do
    dictionary <- loadDictionary dictionaryFile
    return (findHighestScoringPlay board dictionary rackString)