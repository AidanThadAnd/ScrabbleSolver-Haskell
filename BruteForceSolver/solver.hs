{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Solver
Description : Core logic for finding the best Scrabble move.


This module contains the primary functions for solving a Scrabble turn.
It takes a board state, dictionary (Trie), and player rack, then attempts
to find the highest-scoring valid move according to Scrabble rules,
including crossword validation, bonus square scoring, and bingo bonuses.
Note: Does not allow plays that are broken up by existing board tiles
-}
module Solver (solveScrabble, Move(..), Direction(..)) where

import Board (Square(..), letter, boardSize, isValidPosition, Bonus(..), bonus, isBoardEmpty) 
import Trie (Trie, searchWord, isEmptyTrie)
import IO (loadDictionary)

import qualified Data.Array as A
import Data.List (maximumBy, minimumBy, permutations, subsequences, sort, find, minimum, maximum, nub)
import Data.Ord (comparing)
import qualified Data.Set as Set
import System.IO (readFile)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe, mapMaybe, fromJust)
import qualified Data.ByteString.Char8 as BS

-- | Represents the details of a valid Scrabble move.
data Move = Move {
    mainWord  :: String,         -- ^ The primary word formed
    startPos  :: (Int, Int),     -- ^ The (row, col) coordinate of the first Letter.
    direction :: Direction,      -- ^ The orientation (Horizontal or Vertical) of the word`.
    placement :: Placement,      -- ^ List of tiles placed from the rack this turn.
    score     :: Int             -- ^ The total score for this move.
} deriving (Show, Eq)

-- | Standard letter scores for Scrabble.
letterScores :: [(Char, Int)]
letterScores = [('A', 1), ('B', 3), ('C', 3), ('D', 2), ('E', 1), ('F', 4), ('G', 2), ('H', 4), ('I', 1), ('J', 8), ('K', 5), ('L', 1), ('M', 3), ('N', 1), ('O', 1), ('P', 3), ('Q', 10), ('R', 1), ('S', 1), ('T', 1), ('U', 1), ('V', 4), ('W', 4), ('X', 8), ('Y', 4), ('Z', 10)]

-- | Gets the Scrabble score for a letter.
--   Returns 0 for characters not in Scrabble.
getLetterScore :: Char -- ^ The character to score.
               -> Int  -- ^ The score, or 0 if not a standard Scrabble letter.
getLetterScore c = fromMaybe 0 (lookup (toUpper c) letterScores)

-- | Represents the orientation of a word on the board.
data Direction = Horizontal | Vertical deriving (Show, Eq, Ord)

-- | Type alias for a placement: a list of (Row, Column, Character) tuples
--   representing tiles placed from the rack.
type Placement = [(Int, Int, Char)]

-- | Calculates the score of a word on the board,
--   applying bonuses based on the tiles placed.
calculateWordScore :: A.Array (Int, Int) Square -- ^ The current board state.
                   -> Placement                 -- ^ List of placed tiles.
                   -> String                    -- ^ The word being scored.
                   -> (Int, Int)                -- ^ Start position (row, col) of the word.
                   -> Direction                 -- ^ The direction (Horizontal/Vertical) of the word.
                   -> Int                       -- ^ The score for the word.
calculateWordScore board placementThisTurn fullWord startPos dir =
    let
        wordLength = length fullWord
        -- Determine the (row, col) coordinates for each letter in the word
        coords :: [(Int, Int)]
        coords = take wordLength $ iterate nextPos startPos
            where nextPos = case dir of Horizontal -> \(r,c) -> (r, c+1); Vertical -> \(r,c) -> (r+1, c)

        -- Calculate score components
        scoreComponents :: [(Int, Int)] -- [(letterScoreWithBonus, wordMultiplier)]
        scoreComponents = zipWith calculatePositionScore coords fullWord

        -- Extract letter scores and word multipliers
        letterScoresWithBonuses = map fst scoreComponents
        wordMultipliers         = map snd scoreComponents

        -- Sum the letter scores
        wordScore = sum letterScoresWithBonuses
        -- Calculate the word multiplier
        wordMultiplier = product wordMultipliers

        -- | Calculates the score contribution of a single square within the word.
        calculatePositionScore :: (Int, Int) -- ^ The (row, col) of the square.
                               -> Char       -- ^ The character at that square.
                               -> (Int, Int) -- ^ (letter score with DL/TL applied, word multiplier for this square).
        calculatePositionScore (r, c) char =
            let baseLetterScore = getLetterScore char
                sq = board A.! (r, c)
                sqBonus = bonus sq   -- Get the bonus for this square
                -- Was the tile on this square placed this turn?
                wasPlacedThisTurn = any (\(pr, pc, _) -> pr == r && pc == c) placementThisTurn
                -- Apply bonuses if the tile was placed this turn
                (letterBonusValue, wordBonusValue)
                    | wasPlacedThisTurn = case sqBonus of
                        DL   -> (baseLetterScore * 2, 1)
                        TL   -> (baseLetterScore * 3, 1)
                        DW   -> (baseLetterScore,     2)
                        TW   -> (baseLetterScore,     3)
                        None -> (baseLetterScore,     1)
                    -- Letter was already on board, no bonus applies
                    | otherwise = (baseLetterScore, 1)
            in (letterBonusValue, wordBonusValue)

    -- Final score for this word
    in wordScore * wordMultiplier

-- | Generates all possible words from the rack letters.
--   This is a major performance bottleneck.
generatePlays :: String   -- ^ The rack string.
              -> [String] -- ^ A list of possible words.
generatePlays rack = nub $ filter (not . null) $ concatMap permutations (subsequences rack)

-- | Given a placed tile's position and the direction of the main word,
--   finds the full perpendicular word (crossword) it forms, if any,
--   by combining it with adjacent existing letters on the board.
--   Returns Nothing if no crossword is found.
getCrossWord :: A.Array (Int, Int) Square -- ^ The current board state.
             -> Direction                 -- ^ The direction of the main word being placed.
             -> (Int, Int, Char)          -- ^ The placed tile (row, col, char).
             -> Maybe String              -- ^ The crossword string, otherwise Nothing.
getCrossWord board mainDir (r, c, placedChar) =
    let crossDir = case mainDir of Horizontal -> Vertical; Vertical -> Horizontal
        (dr, dc) = case crossDir of Horizontal -> (0, 1); Vertical -> (1, 0)
        -- Scan backward (left/up) to find prefix
        scan wordPart pos = let pPos = (fst pos - dr, snd pos - dc) in if not (isValidPosition (fst pPos) (snd pPos)) then wordPart else let sq = board A.! pPos in if letter sq == ' ' then wordPart else scan (letter sq : wordPart) pPos
        prefix = scan "" (r, c)
        -- Scan forward (right/down) to find suffix
        scan' wordPart pos = let nPos = (fst pos + dr, snd pos + dc) in if not (isValidPosition (fst nPos) (snd nPos)) then wordPart else let sq = board A.! nPos in if letter sq == ' ' then wordPart else scan' (wordPart ++ [letter sq]) nPos
        suffix = scan' "" (r, c)
        crossWord = prefix ++ [placedChar] ++ suffix
    in if length crossWord > 1 then Just crossWord else Nothing

-- | Validates a single potential placement against Scrabble rules:
--   valid placement on empty squares, continuity, main word validity,
--   adjacency/center rules, crossword validity, and calculates the total score including bonuses.
findValidPlays :: A.Array (Int, Int) Square -- ^ The current board state.
               -> Trie                      -- ^ The dictionary Trie.
               -> Placement                 -- ^ The candidate placement.
               -> Maybe Move                -- ^ The Move details if valid, otherwise Nothing.
findValidPlays board dictionary placements =
    if null placements then Nothing else
        let
            -- Basic properties from the placement tiles
            placedWord = map (\(_, _, c) -> c) placements
            rows = map (\(r,_,_)->r) placements
            cols = map (\(_,c,_)->c) placements

            -- Determine the orientation of the placed tiles
            mainDirection :: Maybe Direction
            mainDirection
                | length placements == 1 = Just Horizontal -- Assume horizontal for single tile
                | all (\r -> r == head rows) rows = Just Horizontal
                | all (\c -> c == head cols) cols = Just Vertical
                | otherwise = Nothing -- Not a valid direction

            -- Validation Checks
            -- 1. Check if all placed tiles land on empty, valid board positions
            isValidPlacement = all (\(r, c, _) -> isValidPosition r c && letter (board A.! (r, c)) == ' ') placements

            -- 2. Check if the placement forms a contiguous line
            isContinuous = case mainDirection of
                                Just Horizontal -> sort cols == [minimum cols .. maximum cols]
                                Just Vertical   -> sort rows == [minimum rows .. maximum rows]
                                Nothing         -> length placements == 1 -- Single tile is considered continuous

            -- 3. Find the full main word formed including existing letters and its start position
            fullMainWord :: Maybe (String, (Int,Int)) --(Full word, (start pos))
            fullMainWord = case mainDirection of
                Nothing -> if length placements == 1 then Just (placedWord, (head rows, head cols)) else Nothing
                Just dir -> -- Scan outwards from placed tiles to find boundaries
                    let firstTile = minimumBy (comparing (\(r, c, _) -> if dir == Horizontal then c else r)) placements
                        lastTile  = maximumBy (comparing (\(r, c, _) -> if dir == Horizontal then c else r)) placements
                        (firstR, firstC, _) = firstTile; (lastR, lastC, _) = lastTile
                        (dr, dc) = case dir of Horizontal -> (0, 1); Vertical -> (1, 0)
                        -- Scan backward (left/up) to find prefix and start
                        scanBkw wordPart pos = let pPos=(fst pos-dr,snd pos-dc) in if not (isValidPosition (fst pPos) (snd pPos)) then (pos, wordPart) else let sq = board A.! pPos in if letter sq == ' ' then (pos, wordPart) else scanBkw (letter sq : wordPart) pPos
                        (actualStartPos, prefix) = scanBkw "" (firstR, firstC)
                        -- Scan forward (right/down) to find suffix
                        scanFwd wordPart pos = let nPos = (fst pos+dr, snd pos+dc) in if not (isValidPosition (fst nPos) (snd nPos)) then wordPart else let sq = board A.! nPos in if letter sq == ' ' then wordPart else scanFwd (wordPart ++ [letter sq]) nPos
                        suffix = scanFwd "" (lastR, lastC)
                    in Just (prefix ++ placedWord ++ suffix, actualStartPos) -- Combine parts

            -- 4. Check Scrabble placement rules: Adjacency or Center Start
            isAdjacentToExisting = any (\(r, c, _) -> any (\(dr, dc) -> let nr=r+dr; nc=c+dc in isValidPosition nr nc && letter (board A.! (nr, nc)) /= ' ') [(-1,0),(1,0),(0,-1),(0,1)]) placements
            isEmpty = isBoardEmpty board
            coversCenter = any (\(r, c, _) -> r == 7 && c == 7) placements

            -- Combine initial rule checks: valid placement, continuity, main word validity, and placement rule
            passesInitialChecks =
                isValidPlacement &&
                isContinuous &&
                mainDirection /= Nothing && 
                case fullMainWord of
                    Nothing -> False 
                    Just (fullWord, _) ->
                        -- Check the word against the dictionary
                        searchWord dictionary fullWord &&
                        -- Check Scrabble placement rules
                        ( (isEmpty && coversCenter) || (not isEmpty && isAdjacentToExisting) )


            -- Helper function to check a single crossword.
            -- Returns Maybe (isValid, Maybe (crossWordString, startPos, crossDirection))
            checkSingleCrossword :: Direction -> (Int, Int, Char) -> Maybe (Bool, Maybe (String, (Int, Int), Direction))
            checkSingleCrossword mainDir placedTile@(r, c, _) =
                case getCrossWord board mainDir placedTile of
                Nothing -> Just (True, Nothing) -- No crossword formed
                Just cw -> -- Crossword 'cw' was formed
                    let isValidCrossword = searchWord dictionary cw -- Check dictionary
                    in if isValidCrossword then
                        let -- Find details needed for scoring (start pos, direction)
                        crossDir = case mainDir of Horizontal -> Vertical; Vertical -> Horizontal
                        (dr, dc) = case crossDir of Horizontal -> (0, 1); Vertical -> (1, 0)
                        -- Scan back to find the start of the crossword
                        scanBackwardToStart currentPos =
                            let prevPos = (fst currentPos - dr, snd currentPos - dc)
                            in if not (isValidPosition (fst prevPos) (snd prevPos)) then currentPos
                                else let sq = board A.! prevPos in if letter sq == ' ' then currentPos else scanBackwardToStart prevPos
                        crossWordStartPos = scanBackwardToStart (r, c)
                        -- Return valid flag and scoring info
                        in Just (True, Just (cw, crossWordStartPos, crossDir))
                    else
                        -- Crossword found but not in dictionary
                        Just (False, Nothing)

            -- 5. Perform crossword validation for all placed tiles
            -- Collects scoring info for valid crosswords: [(crossWordString, startPos, crossDirection)]
            (allCrossWordsValid, crossWordInfos) =
                if not passesInitialChecks then
                    (False, []) -- Short-circuit if checks fail
                else
                    -- If initial checks passed, validate all crosswords
                    let dir = fromJust mainDirection
                        crossChecks = mapMaybe (checkSingleCrossword dir) placements
                    -- Check if all crosswords were valid (first part of tuple)
                    -- Collect the scoring info (Maybe part of tuple)
                    in (all fst crossChecks, mapMaybe snd crossChecks)

            -- 6. Calculate total score
            totalScore = if passesInitialChecks && allCrossWordsValid then
                            -- Get main word info
                            let Just (mainWordStr, mainStartPos) = fullMainWord
                                theDir = fromJust mainDirection
                                -- Calculate score for the main word
                                mainScore = calculateWordScore board placements mainWordStr mainStartPos theDir
                                -- Calculate score for each crossword
                                crossScores = map (\(cw, csPos, cDir) -> calculateWordScore board placements cw csPos cDir) crossWordInfos
                                -- Add Bingo bonus if 7 tiles were placed
                                bingoBonus = if length placements == 7 then 50 else 0
                            -- Sum final score
                            in mainScore + sum crossScores + bingoBonus
                        else 0 -- Score is 0 if any check failed

            in -- Return the final move structure
            if passesInitialChecks && allCrossWordsValid then
                -- Get main word info
                let Just (theMainWord, theStartPos) = fullMainWord
                    theDirection = fromJust mainDirection
                -- Construct the result
                in Just Move { mainWord = theMainWord,
                                startPos = theStartPos,
                                direction = theDirection,
                                placement = placements,
                                score = totalScore }
            else Nothing -- Otherwise the move was invalid


-- | Generates potential horizontal and vertical placements
--   for a given word starting at a specific coordinate.
generatePlacement :: String       -- ^ The word to attempt placing.
                  -> (Int, Int)   -- ^ The starting (row, col) coordinate.
                  -> [Placement]  -- ^ List containing potential horizontal and/or vertical Placements.
generatePlacement wordStr (r, c) = filter checkBounds [horizontalPlacement, verticalPlacement]
    where
        horizontalPlacement = zipWith (\offset ch -> (r, c + offset, ch)) [0..] wordStr
        verticalPlacement   = zipWith (\offset ch -> (r + offset, c, ch)) [0..] wordStr
        -- Ensures all coordinates in a placement are within bounds
        checkBounds :: Placement -> Bool
        checkBounds placement = not (null placement) && all (\(pr, pc, _) -> isValidPosition pr pc) placement

-- | Finds all valid moves for a given rack and board state by generating candidate
--   words and placements, then validates each one.
findAllPlays :: A.Array (Int, Int) Square -- ^ The current board state.
             -> Trie                      -- ^ The dictionary Trie.
             -> String                    -- ^ The player's rack.
             -> [Move]                    -- ^ A list of all valid found moves.
findAllPlays board dictionary rack =
    let
        -- Find all empty squares to start placing words
        startingPositions = generateStartingPositions board

        -- Generate all possible word from the rack
        possibleWords = nub $ filter (not . null) $ generatePlays rack

        -- Create all potential placements by trying each word at each potential start position
        allPotentialPlacements :: [Placement]
        allPotentialPlacements = concatMap (\word -> concatMap (generatePlacement word) startingPositions) possibleWords

        -- Validate each potential placement
        validPlays :: [Move]
        validPlays = mapMaybe (findValidPlays board dictionary) allPotentialPlacements
    in
        validPlays

-- | Generates a list of all empty square coordinates on the board.
--   Used by findAllPlays to determine potential starting points for placements.
generateStartingPositions :: A.Array (Int, Int) Square -- ^ The current board state.
                          -> [(Int, Int)]              -- ^ List of (row, col) coordinates of empty squares.
generateStartingPositions board =
    [(r, c) | r <- [0 .. boardSize - 1], -- Iterate over all rows
              c <- [0 .. boardSize - 1], -- Iterate over all columns
              letter (board A.! (r, c)) == ' ' -- Keep if square is empty
    ]

-- | Finds the highest scoring move among all valid moves found by `findAllPlays`.
findHighestScoringPlay :: A.Array (Int, Int) Square -- ^ The current board state.
                       -> Trie                      -- ^ The dictionary Trie.
                       -> String                    -- ^ The player's rack.
                       -> Maybe Move                -- ^ The best move, otherwise Nothing.
findHighestScoringPlay board dictionary rack =
    case findAllPlays board dictionary rack of
        []    -> Nothing -- No valid moves were found
        plays -> Just (maximumBy (comparing score) plays) -- Find the move with the maximum score field

-- | Function to solve a Scrabble turn. Load the dictionary and finds
--   the best move.
solveScrabble :: A.Array (Int, Int) Square -- ^ The initial board state passed from Main.
              -> FilePath                  -- ^ Path to the dictionary file.
              -> String                    -- ^ The player's rack.
              -> IO (Maybe Move)           -- ^ The best move found, Nothing if dictionary is empty or no moves found.
solveScrabble board dictionaryFile rackString = do

    dictionary <- loadDictionary dictionaryFile

    if isEmptyTrie dictionary then do
        return Nothing
    else do
        let bestPlay = findHighestScoringPlay board dictionary rackString
        return bestPlay