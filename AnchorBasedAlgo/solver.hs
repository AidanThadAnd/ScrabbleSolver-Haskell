{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Solver
Description : Core logic for finding the best Scrabble move using an anchor-based approach.

This module contains the primary functions for solving a Scrabble turn using
a more efficient anchor-based generation. It identifies potential starting
squares adjacent to existing tiles and builds moves outwards.

Known issue: The recursive generation logic might still produce corrupted placement lists 
for some moves, leading to incorrect scores or final output for those specific moves.
-}
module Solver (solveScrabble, Move(..), Direction(..)) where

import Board (Square(..), letter, boardSize, isValidPosition, Bonus(..), bonus, isBoardEmpty, Coord, center)
import Trie (Trie, searchWord, isEmptyTrie)
import IO (loadDictionary)

import qualified Data.Array as A
import Data.List (maximumBy, minimumBy, nub, foldl', filter) 
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char (toUpper)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList, catMaybes, isJust) 
import qualified Data.ByteString.Char8 as BS


-- | Represents the details of a valid Scrabble move.
data Move = Move {
    mainWord  :: String,         -- ^ The primary word formed.
    startPos  :: Coord,          -- ^ The (row, col) coordinate of the first Letter of the word.
    direction :: Direction,      -- ^ The orientation (Horizontal or Vertical) of the word.
    placement :: Placement,      -- ^ List of tiles placed from the rack this turn. [(row, col, char)].
    score     :: Int             -- ^ The total score for this move.
} deriving (Show, Eq)

-- | Represents the orientation of a word on the board.
data Direction = Horizontal | Vertical deriving (Show, Eq, Ord)

-- | Type alias for a placement: a list of (Row, Column, Character) tuples.
type Placement = [(Int, Int, Char)]

-- | Type alias for the player's rack, mapping letters to their counts.
type Rack = Map.Map Char Int


-- | Standard letter scores for Scrabble.
letterScores :: [(Char, Int)]
letterScores = [('A', 1), ('B', 3), ('C', 3), ('D', 2), ('E', 1), ('F', 4), ('G', 2), ('H', 4), ('I', 1), ('J', 8), ('K', 5), ('L', 1), ('M', 3), ('N', 1), ('O', 1), ('P', 3), ('Q', 10), ('R', 1), ('S', 1), ('T', 1), ('U', 1), ('V', 4), ('W', 4), ('X', 8), ('Y', 4), ('Z', 10)]

-- | Gets the Scrabble score for a letter.
--   Returns 0 for characters not in Scrabble.
getLetterScore :: Char -> Int
getLetterScore c = fromMaybe 0 (lookup (toUpper c) letterScores)

-- | Calculates the score of a single word, applying bonuses
--   based on the tiles placed.
calculateWordScore :: A.Array (Int, Int) Square -- ^ The current board state.
                   -> Placement                -- ^ List of tiles placed.
                   -> String                   -- ^ The word being scored.
                   -> Coord                    -- ^ Start position (row, col) of the word.
                   -> Direction                -- ^ The direction (H/V) of the word.
                   -> Int                      -- ^ The score for the word.
calculateWordScore board placementThisTurn fullWord startPos dir =
    let
        wordLength = length fullWord
        coords :: [Coord]
        coords = take wordLength $ iterate nextPos startPos
            where nextPos = case dir of Horizontal -> \(r,c) -> (r, c+1); Vertical -> \(r,c) -> (r+1, c)

        wordWithCoords :: [(Coord, Char)]
        wordWithCoords = zip coords fullWord

        scoreComponents :: [(Int, Int)] -- [(letterScoreWithBonus, wordMultiplier)]
        scoreComponents = map calculatePositionScore wordWithCoords

        letterScoresWithBonuses = map fst scoreComponents
        wordMultipliers         = map snd scoreComponents

        wordScore = sum letterScoresWithBonuses
        totalWordMultiplier = product wordMultipliers

        finalScore = wordScore * totalWordMultiplier

    in finalScore
    where
        -- | Calculates the score of a single square within the word.
        calculatePositionScore :: (Coord, Char) -- ^ The ((row, col), char) of the square.
                               -> (Int, Int)    -- ^ (letter score with DL/TL, word multiplier from DW/TW).
        calculatePositionScore (coord@(r, c), char) =
            let baseLetterScore = getLetterScore char
                -- Check if this specific tile was placed this turn
                wasPlacedThisTurn = any (\(pr, pc, _) -> pr == r && pc == c) placementThisTurn
                -- Get the bonus type from the original board square
                originalBonus = if isValidPosition r c then bonus (board A.! coord) else None
                -- Apply bonus only if tile was placed this turn
                currentBonus = if wasPlacedThisTurn then originalBonus else None

                -- Determine score part and multiplier part based on the bonus
                result = case currentBonus of
                    DL   -> (baseLetterScore * 2, 1) -- Letter bonus applied to score
                    TL   -> (baseLetterScore * 3, 1) -- Letter bonus applied to score
                    DW   -> (baseLetterScore,     2) -- Word bonus applied to multiplier
                    TW   -> (baseLetterScore,     3) -- Word bonus applied to multiplier
                    None -> (baseLetterScore,     1) -- No bonus

            in result


-- | Converts a rack string to a rack map.
stringToRack :: String -> Rack
stringToRack = Map.fromListWith (+) . map (\c -> (toUpper c, 1))

-- | Removes a character from the rack map. Returns Nothing if char not present or count is zero.
useTile :: Char         -- ^ Character to use.
        -> Rack         -- ^ Current rack map.
        -> Maybe Rack   -- ^ Updated rack map, or Nothing.
useTile char rack =
    case Map.lookup uc rack of
        Nothing -> Nothing -- Char not in rack
        Just count ->
            if count > 1
            then Just (Map.adjust (\_ -> count - 1) uc rack) -- Decrement count
            else Just (Map.delete uc rack) -- Remove char if count was 1
    where uc = toUpper char

-- | Finds all squares that can be used as anchors for placing new words.
--   An anchor is an empty square adjacent to an existing tile.
findAnchorSquares :: A.Array (Int, Int) Square -> Set.Set Coord
findAnchorSquares board
    | isBoardEmpty board = Set.singleton (center, center)
    | otherwise =
        Set.fromList [ anchorCoord
                     | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]
                     , let anchorCoord = (r, c)
                     , letter (board A.! anchorCoord) == ' ' -- Must be empty
                     -- Check if any valid neighbour is occupied
                     , any (\(dr, dc) -> let nc = neighbourCoord anchorCoord (dr, dc) in isValidPosition (fst nc) (snd nc) && isOccupied nc)
                           [(-1, 0), (1, 0), (0, -1), (0, 1)]
                     ]
        where
            isOccupied :: Coord -> Bool
            isOccupied coord = letter (board A.! coord) /= ' '
            neighbourCoord :: Coord -> (Int, Int) -> Coord
            neighbourCoord (r,c) (dr, dc) = (r+dr, c+dc)

-- | Generates all valid moves from a set of anchors.
findAllAnchorPlays :: A.Array (Int, Int) Square -- ^ The current board state.
                  -> Trie                        -- ^ The dictionary Trie.
                  -> String                      -- ^ The player's rack.
                  -> [Move]                      -- ^ A list of all valid found moves.
findAllAnchorPlays board dictionary rackString =
    let
        anchors = findAnchorSquares board
        initialRack = stringToRack rackString
        -- Generate moves for both directions from all anchors
        -- Use nub to remove duplicate moves
        allMoves = nub $ concatMap (generatePlaysForAnchor board dictionary initialRack) (Set.toList anchors)
    in allMoves

-- | Generates Horizontal and Vertical plays starting from a single anchor square.
generatePlaysForAnchor :: A.Array (Int, Int) Square -- ^ Board state.
                       -> Trie                       -- ^ Dictionary.
                       -> Rack                       -- ^ Initial rack map.
                       -> Coord                      -- ^ The anchor square.
                       -> [Move]                     -- ^ List of valid moves found from this anchor.
generatePlaysForAnchor board dictionary initialRack anchor =
    let maxExtend = boardSize -- Limit search depth to the board size
        -- Generate moves in both directions (Horizontal and Vertical)
        hMoves = generateDirectionalPlays board dictionary initialRack anchor Horizontal maxExtend
        vMoves = generateDirectionalPlays board dictionary initialRack anchor Vertical maxExtend
    in hMoves ++ vMoves

-- | Function to generate plays in a specific direction from an anchor.
generateDirectionalPlays :: A.Array (Int, Int) Square  -- ^ Board state.
                         -> Trie                       -- ^ Dictionary.
                         -> Rack                       -- ^ Initial rack map.
                         -> Coord                      -- ^ Anchor coordinate.
                         -> Direction                  -- ^ Horizontal or Vertical.
                         -> Int                        -- ^ Max squares to check from anchor.
                         -> [Move]                     -- ^ List of valid moves found.
generateDirectionalPlays board dictionary rack anchor dir maxExtend =
    extendBackward [] "" rack anchor anchor 0 -- Start backward search from anchor
    where
      -- Deltas for moving backward/forward
      (deltaBwd, deltaFwd) = case dir of
                                Horizontal -> ((0, -1), (0, 1))
                                Vertical   -> ((-1, 0), (1, 0))

      -- Helper to move a coordinate by a delta
      moveCoord :: Coord -> (Int, Int) -> Coord
      moveCoord (r, c) (dr, dc) = (r + dr, c + dc)

      -- Helper to get char from original board, returns '\0' for invalid/off-board
      getCharAt :: Coord -> Char
      getCharAt coord@(r, c)
          | isValidPosition r c = letter (board A.! coord)
          | otherwise           = '\0'

      -- | Recursively explores squares backward (left/up) from a position.
      extendBackward :: Placement  -- ^ Tiles placed so far during backward pass.
                     -> String     -- ^ Characters encountered so far during backward pass.
                     -> Rack       -- ^ Current rack state.
                     -> Coord      -- ^ The coordinate currently being looked at.
                     -> Coord      -- ^ The original anchor coordinate.
                     -> Int        -- ^ Depth of backward search.
                     -> [Move]     -- ^ Valid moves found.
      extendBackward placementBwd wordPartBwdRev currentRack currentPos anchorPos squaresExtended =
          let currentSqChar = getCharAt currentPos
              nextPosBwd = moveCoord currentPos deltaBwd
              movesFromHereFwd = extendForward placementBwd wordPartBwdRev currentRack anchorPos anchorPos
          in
          -- Base Case: Off board or max backward reached
          if currentSqChar == '\0' || squaresExtended >= maxExtend then
              movesFromHereFwd -- Stop going backward

          -- Recursive Step: Continue backward
          else
              let continueBackward = -- Results from exploring further back
                      -- Case 1: Existing tile at currentPos
                      if currentSqChar /= ' ' then
                          extendBackward placementBwd (currentSqChar : wordPartBwdRev) currentRack nextPosBwd anchorPos (squaresExtended + 1)
                      -- Case 2: Empty square at currentPos
                      else
                           concatMap (tryPlaceAndExtendBwd currentPos placementBwd wordPartBwdRev currentRack anchorPos squaresExtended) (Map.keys currentRack)

              -- Combine moves
              in movesFromHereFwd ++ continueBackward

      -- | Helper for extendBackward: Tries placing charToPlace at currentPos_bwd, then continues backward.
      tryPlaceAndExtendBwd :: Coord      -- ^ The empty square to place the tile on.
                           -> Placement  -- ^ Placement list from previous steps.
                           -> String     -- ^ Word part from previous steps.
                           -> Rack       -- ^ Current rack.
                           -> Coord      -- ^ Original anchor pos.
                           -> Int        -- ^ Current backward depth.
                           -> Char       -- ^ Character to try placing.
                           -> [Move]     -- ^ Valid moves found.
      tryPlaceAndExtendBwd currentPos_bwd pbwd wpBwdRev rack anchorPos sqExt charToPlace =
          case useTile charToPlace rack of
            Nothing -> [] -- Tile not in rack
            Just nextRack ->
                let placedPos = currentPos_bwd
                    -- Prepend placed tile to backward placement list
                    newPlacement = (fst placedPos, snd placedPos, charToPlace) : pbwd
                    nextWordPart = charToPlace : wpBwdRev
                    nextPosBwd = moveCoord currentPos_bwd deltaBwd
                -- Recurse backward from the next position
                in extendBackward newPlacement nextWordPart nextRack nextPosBwd anchorPos (sqExt + 1)

      -- | Recursively explores squares forward (right/down) starting from the anchor.
      --   Builds potential placements and validates them.
      extendForward :: Placement  -- ^ Placement found during the backward pass.
                    -> String     -- ^ Word part found during the backward pass.
                    -> Rack       -- ^ Rack state after backward pass.
                    -> Coord      -- ^ Current position for forward search.
                    -> Coord      -- ^ The original anchor coordinate.
                    -> [Move]     -- ^ Valid moves found.
      extendForward placementBwd wordPartBwdRev currentRack currentPos startFwdPos =
          recurseHelper placementBwd wordPartBwdRev currentRack currentPos [] "" (currentPos == startFwdPos)
          where
              -- | Recursive helper for the forward pass.
              recurseHelper :: Placement  -- ^ Backward placement.
                            -> String     -- ^ Backward word part.
                            -> Rack       -- ^ Current rack.
                            -> Coord      -- ^ Current position being looked at.
                            -> Placement  -- ^ Tiles placed so far during this pass.
                            -> String     -- ^ Characters encountered so far during this pass.
                            -> Bool       -- ^ True if the orginal anchor was crossed. False otherwise.
                            -> [Move]     -- ^ Valid moves found.
              recurseHelper pbwd wpbRev rack pos placementAcc wordPartAcc anchorWasCrossed =
                  let charAtPos = getCharAt pos -- Read from original board
                      isAnchor = pos == startFwdPos
                      nextAnchorCrossed = anchorWasCrossed || isAnchor
                  in
                  -- Base Case: Off board
                  if charAtPos == '\0' then
                       -- Validate word ending before this position
                       validateMove pbwd wpbRev placementAcc wordPartAcc rack nextAnchorCrossed pos

                  -- Recursive Step: On board
                  else
                       let nextPos = moveCoord pos deltaFwd -- Next position to check forward
                           -- Option 1: Use existing board letter
                           useBoardLetter =
                               if charAtPos /= ' ' then
                                   let -- Validate word ending with this board letter
                                       movesEndingHere = validateMove pbwd wpbRev placementAcc (wordPartAcc ++ [charAtPos]) rack nextAnchorCrossed nextPos
                                       -- Continue forward
                                       movesContinuing = recurseHelper pbwd wpbRev rack nextPos placementAcc (wordPartAcc ++ [charAtPos]) nextAnchorCrossed
                                   in movesEndingHere ++ movesContinuing
                               else [] -- Cannot place on empty square

                           -- Option 2: Try placing rack letters if square is empty
                           placeRackLetters =
                               if charAtPos == ' ' then
                                   concatMap (tryPlaceAndGoFwd pos pbwd wpbRev rack placementAcc wordPartAcc nextAnchorCrossed) (Map.keys rack)
                               else [] -- Cannot place on occupied square

                           -- Option 3: Consider word ending before this empty square
                           movesEndingBefore = if charAtPos == ' '
                                                then validateMove pbwd wpbRev placementAcc wordPartAcc rack nextAnchorCrossed pos
                                                else [] 

                       -- Combine results
                       in useBoardLetter ++ placeRackLetters ++ movesEndingBefore

              -- | Helper for recurseHelper: Tries placing charToPlace at pos, validates, and continues forward.
              tryPlaceAndGoFwd :: Coord      -- ^ The empty square to place on.
                               -> Placement  -- ^ Backward placement list.
                               -> String     -- ^ Backward word part.
                               -> Rack       -- ^ Current rack.
                               -> Placement  -- ^ Forward placement accumulator.
                               -> String     -- ^ Forward word part accumulator.
                               -> Bool       -- ^ Anchor crossed flag.
                               -> Char       -- ^ Character from rack to try placing.
                               -> [Move]     -- ^ Valid moves found.
              tryPlaceAndGoFwd pos pbwd wpbRev rack pAcc wpAcc crossed charToPlace =
                  case useTile charToPlace rack of
                      Nothing -> [] -- Tile not available
                      Just nextRack ->
                          -- Prepend newly placed tile to forward placement accumulator
                          let newPlacementAcc = (fst pos, snd pos, charToPlace) : pAcc
                              nextWordPartAcc = wpAcc ++ [charToPlace]
                              nextPos = moveCoord pos deltaFwd
                              -- Validate word ending with this tile
                              movesEndingHere = validateMove pbwd wpbRev newPlacementAcc nextWordPartAcc nextRack crossed nextPos
                              -- Continue forward
                              movesContinuing = recurseHelper pbwd wpbRev nextRack nextPos newPlacementAcc nextWordPartAcc crossed
                          in movesEndingHere ++ movesContinuing

      -- | Central validation function.
      validateMove :: Placement  -- ^ Backward placement list.
                   -> String     -- ^ Backward word part.
                   -> Placement  -- ^ Forward placement accumulator.
                   -> String     -- ^ Forward word part accumulator.
                   -> Rack       -- ^ Rack state after tiles used.
                   -> Bool       -- ^ Anchor crossed flag.
                   -> Coord      -- ^ The coordinate after the last char considered.
                   -> [Move]     -- ^ Returns list containing the valid Move, or empty list.
      validateMove placementBwd wordPartBwdRev placementFwdRev wordPartFwd finalRack anchorCrossed endPosGeneration =
          -- Calculate the full placement list for this move
          let generatedPlacement = placementBwd ++ reverse placementFwdRev
              -- Create a map for looking up placed tiles
              placedLookup = Map.fromList [((pr, pc), pchar) | (pr, pc, pchar) <- generatedPlacement]

              -- Helper to get char considering placed tiles
              getCharAt_WithPlacement :: Coord -> Char
              getCharAt_WithPlacement coord@(r, c) =
                  case Map.lookup coord placedLookup of
                      Just placedChar -> placedChar
                      Nothing         -> if isValidPosition r c then letter (board A.! coord) else '\0'

              -- Helper: Find prefix/suffix parts for perpendicular words
              findPerpendicularParts :: Coord -> Direction -> (String, String)
              findPerpendicularParts pos p_dir =
                  let (dr, dc) = case p_dir of Horizontal -> (0, 1); Vertical -> (1, 0)
                      scanBwd :: String -> Coord -> String
                      scanBwd wp current = let prev = (fst current - dr, snd current - dc); char = getCharAt_WithPlacement prev in if char `elem` [' ', '\0'] then wp else scanBwd (char : wp) prev
                      scanFwd :: String -> Coord -> String
                      scanFwd wp current = let next = (fst current + dr, snd current + dc); char = getCharAt_WithPlacement next in if char `elem` [' ', '\0'] then wp else scanFwd (wp ++ [char]) next
                      prefix = scanBwd "" pos
                      suffix = scanFwd "" pos
                  in (prefix, suffix)

              -- Helper: Find start coordinate of a perpendicular word
              findPerpendicularStart :: Coord -> String -> Direction -> Coord
              findPerpendicularStart (r,c) prefix p_dir =
                  let prefixLen = length prefix
                      (dr, dc) = case p_dir of Horizontal -> (0, 1); Vertical -> (1, 0)
                  in (r - prefixLen * dr, c - prefixLen * dc)

              -- Helper: Get the full perpendicular word and its start coord
              getPerpendicularWord :: Coord -> Direction -> Maybe (String, Coord)
              getPerpendicularWord coord crossDir =
                  let charAtCoord = getCharAt_WithPlacement coord
                  in if charAtCoord `elem` [' ','\0'] then Nothing else
                     let (p_prefix, p_suffix) = findPerpendicularParts coord crossDir
                         p_word = p_prefix ++ [charAtCoord] ++ p_suffix
                     in if length p_word > 1 then
                            let p_startCoord = findPerpendicularStart coord p_prefix crossDir
                            in Just (p_word, p_startCoord)
                        else Nothing

          in
          -- Basic checks: Must cross anchor and use at least one tile from rack
          if not anchorCrossed then []
          else if null generatedPlacement then []
          else
             -- 1. Determine the true start and extract the true main word
             let (minR, minC, _) = minimumBy (comparing (\(r,c,_) -> if dir == Horizontal then c else r)) generatedPlacement
                 (dr, dc) = case dir of Horizontal -> (0, 1); Vertical -> (1, 0)
                 findTrueStart :: Coord -> Coord
                 findTrueStart currentPos = let prevPos = (fst currentPos - dr, snd currentPos - dc) in if getCharAt_WithPlacement prevPos `elem` [' ', '\0'] then currentPos else findTrueStart prevPos
                 trueStartPos = findTrueStart (minR, minC)
                 extractTrueWord :: Coord -> String
                 extractTrueWord start = map getCharAt_WithPlacement $ takeWhile (\pos -> let c = getCharAt_WithPlacement pos in c /= ' ' && c /= '\0') $ iterate (\p -> moveCoord p (dr, dc)) start
                 trueMainWord = if isValidPosition (fst trueStartPos) (snd trueStartPos) then extractTrueWord trueStartPos else ""
                 wordLen = length trueMainWord
             in
             -- Check main word length
             if wordLen < 2 then []
             else
                 -- Check 2: True Main word in dictionary
                 let isMainWordValid = searchWord dictionary trueMainWord in
                 if not isMainWordValid then []
                 else
                     -- Check 3: Validate perpendicular words formed by placed tiles
                     let crossDir = if dir == Horizontal then Vertical else Horizontal

                         -- Find and validate perpendicular words
                         checkPlacedTile :: (Int, Int, Char) -> (Bool, Maybe (String, Coord)) -- (Is Check OK?, Maybe WordInfo)
                         checkPlacedTile (r, c, _) =
                             let coord = (r,c)
                             in case getPerpendicularWord coord crossDir of
                                 Nothing -> (True, Nothing) -- No cross word formed
                                 Just (p_word, p_startCoord) ->
                                     let isValid = searchWord dictionary p_word
                                     in (isValid, if isValid then Just (p_word, p_startCoord) else Nothing)

                         -- Process all placed tiles
                         placedTileChecks :: [(Bool, Maybe (String, Coord))]
                         placedTileChecks = map checkPlacedTile generatedPlacement

                         -- Are all checks OK
                         allPlacedCrossWordsOK :: Bool
                         allPlacedCrossWordsOK = all fst placedTileChecks

                         -- Collect info for the valid crosswords
                         validScorableCrossInfo :: [(String, Coord)]
                         validScorableCrossInfo = mapMaybe snd placedTileChecks

                     in if not allPlacedCrossWordsOK then
                           [] -- Reject if invalid crossword
                        else -- Main word is valid and all crosswords are valid

                           let mainScore = calculateWordScore board generatedPlacement trueMainWord trueStartPos dir
                               -- Score the valid crosswords
                               crossScores = sum $ map (\(cw, csPos) -> calculateWordScore board generatedPlacement cw csPos crossDir) validScorableCrossInfo
                               bingoBonus = if length generatedPlacement == 7 then 50 else 0
                               -- Calculate total score
                               totalScore = mainScore + crossScores + bingoBonus
                           in -- Return the valid Move
                              [Move { mainWord = trueMainWord,
                                      startPos = trueStartPos,
                                      direction = dir,
                                      placement = generatedPlacement,
                                      score = totalScore }]

-- | Finds the highest scoring move among all valid moves found.
findHighestScoringPlay :: A.Array (Int, Int) Square -> Trie -> String -> Maybe Move
findHighestScoringPlay board dictionary rack =
    case findAllAnchorPlays board dictionary rack of
        []    -> Nothing
        plays -> Just (maximumBy (comparing score) plays) -- Find move with max score

-- | Function to solve a Scrabble turn. Loads the dictionary and finds the best move.
solveScrabble :: A.Array (Int, Int) Square -- ^ The initial board state passed from Main.
              -> FilePath                  -- ^ Path to the dictionary file.
              -> String                    -- ^ The player's rack.
              -> IO (Maybe Move)           -- ^ The best move found, Nothing otherwise.
solveScrabble board dictionaryFile rackString = do
    dictionary <- loadDictionary dictionaryFile
    if isEmptyTrie dictionary then do
        putStrLn "Dictionary is empty or failed to load."
        return Nothing
    else do
        let maybeBestPlay = findHighestScoringPlay board dictionary rackString
        -- Report result
        case maybeBestPlay of
             Just m -> putStrLn $ "Found best play: '" ++ mainWord m ++ "' score: " ++ show (score m)
             Nothing -> putStrLn "No valid plays found."
        return maybeBestPlay
