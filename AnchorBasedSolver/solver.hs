{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Solver
Description : Core logic for finding the best Scrabble move using an anchor-based approach.

This module implements the main algorithm for finding the optimal Scrabble move
given a board state, a player's rack, and a dictionary. It uses an anchor-based
strategy: identifying potential anchor squares (empty squares adjacent to existing tiles)
and exploring possible word placements extending from these anchors.
-}
module Solver (solveScrabble, Move(..), Direction(..)) where

import Board (Square(..), letter, boardSize, isValidPosition, Bonus(..), bonus, isBoardEmpty, Coord, center)
import Trie (Trie, searchWord, isEmptyTrie)
import IO (loadDictionary)

import qualified Data.Array as A
import Data.List (maximumBy, minimumBy, nub, foldl')
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char (toUpper)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList, catMaybes, isJust)
import qualified Data.ByteString.Char8 as BS

-- | Represents the details of a valid Scrabble move.
data Move = Move {
    mainWord  :: String,         -- ^ The primary word formed
    startPos  :: Coord,          -- ^ The (row, col) coordinate of the first Letter of the main word.
    direction :: Direction,      -- ^ The orientation (Horizontal or Vertical) of the word.
    placement :: Placement,      -- ^ List of tiles placed from the rack this turn. [(row, col, char)]
    score     :: Int             -- ^ The total score for the move.
} deriving (Show, Eq)

-- | Standard letter scores for Scrabble.
letterScores :: [(Char, Int)]
letterScores = [('A', 1), ('B', 3), ('C', 3), ('D', 2), ('E', 1), ('F', 4), ('G', 2), ('H', 4), ('I', 1), ('J', 8), ('K', 5), ('L', 1), ('M', 3), ('N', 1), ('O', 1), ('P', 3), ('Q', 10), ('R', 1), ('S', 1), ('T', 1), ('U', 1), ('V', 4), ('W', 4), ('X', 8), ('Y', 4), ('Z', 10)]

-- | Gets the Scrabble score for a letter.
getLetterScore :: Char -> Int
getLetterScore c = fromMaybe 0 (lookup (toUpper c) letterScores)

-- | Represents the orientation of a word on the board.
data Direction = Horizontal | Vertical deriving (Show, Eq, Ord)

-- | Type alias for a placement: a list of (Row, Column, Character) tuples
--   representing tiles placed from the rack.
type Placement = [(Int, Int, Char)]

-- | Type alias for the player's rack, mapping letters to their counts.
type Rack = Map.Map Char Int

-- | Calculates the score of a single word (main or cross), applying bonuses.
--   Bonuses are applied only if the tile occupying the bonus square was placed
--   in the current turn.
calculateWordScore :: A.Array (Int, Int) Square -- ^ Board state
                   -> Placement                -- ^ Tiles placed this turn
                   -> String                   -- ^ The full word
                   -> Coord                    -- ^ Start coordinate of the word
                   -> Direction                -- ^ Direction of the word
                   -> Int                      -- ^ Calculated score
calculateWordScore board placementThisTurn fullWord startPos dir =
    let
        wordLength = length fullWord
        -- Generate coordinates for each letter in the word
        coords :: [Coord]
        coords = take wordLength $ iterate nextPos startPos
            where nextPos = case dir of Horizontal -> \(r,c) -> (r, c+1); Vertical -> \(r,c) -> (r+1, c)

        -- Pair each letter with its coordinate
        wordWithCoords :: [(Coord, Char)]
        wordWithCoords = zip coords fullWord

        -- Calculate (letter score * letter bonus, word bonus multiplier) for each position
        scoreComponents :: [(Int, Int)]
        scoreComponents = map calculatePositionScore wordWithCoords

        -- Separate letter scores with bonuses and word multipliers
        letterScoresWithBonuses = map fst scoreComponents
        wordMultipliers         = map snd scoreComponents

        -- Calculate base word score by summing letter scores
        wordScore = sum letterScoresWithBonuses
        -- Calculate total word multiplier by multiplying individual multipliers
        totalWordMultiplier = product wordMultipliers

        -- Final score for this word
        finalScore = wordScore * totalWordMultiplier

    in finalScore
    where
        -- | Calculates the score contribution of a single letter at a specific position.
        --   Applies letter/word bonuses.
        calculatePositionScore :: (Coord, Char) -> (Int, Int)
        calculatePositionScore (coord@(r, c), char) =
            let baseLetterScore = getLetterScore char
                -- Check if the tile at this coordinate was part of the current placement
                wasPlacedThisTurn = any (\(pr, pc, _) -> pr == r && pc == c) placementThisTurn
                -- Get the bonus associated with the square on the board
                originalBonus = if isValidPosition r c then bonus (board A.! coord) else None
                -- Apply the bonus only if the tile was placed this turn
                currentBonus = if wasPlacedThisTurn then originalBonus else None

                -- Determine score based on the applied bonus
                result = case currentBonus of
                    DL   -> (baseLetterScore * 2, 1)
                    TL   -> (baseLetterScore * 3, 1)
                    DW   -> (baseLetterScore,     2)
                    TW   -> (baseLetterScore,     3)
                    None -> (baseLetterScore,     1)

            in result

-- | Converts a rack string to a frequency map.
stringToRack :: String -> Rack
stringToRack = Map.fromListWith (+) . map (\c -> (toUpper c, 1))

-- | Removes a character from the rack map. Returns Nothing if char not present or count is zero.
useTile :: Char -> Rack -> Maybe Rack
useTile char rack =
    case Map.lookup uc rack of
        Nothing -> Nothing
        Just count ->
            if count > 1
            then Just (Map.adjust (\_ -> count - 1) uc rack) -- Decrement count
            else Just (Map.delete uc rack)                   -- Remove tile if count was 1
    where uc = toUpper char

-- | Finds all squares that can be used as anchors for placing new words.
--   An anchor is an empty square that is adjacent to at least one occupied square.
--   If the board is completely empty, the center square is the only anchor.
findAnchorSquares :: A.Array (Int, Int) Square -> Set.Set Coord
findAnchorSquares board
    | isBoardEmpty board = Set.singleton (center, center) -- Special case for the first move
    | otherwise =
        Set.fromList [ anchorCoord
                        | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1] -- Iterate over all board positions
                        , let anchorCoord = (r, c)
                        , letter (board A.! anchorCoord) == ' ' -- Must be an empty square
                        -- Must have at least one neighbour that is on the board and occupied
                        , any (\(dr, dc) -> let nc = neighbourCoord anchorCoord (dr, dc) in isValidPosition (fst nc) (snd nc) && isOccupied nc)
                           [(-1, 0), (1, 0), (0, -1), (0, 1)] -- Check Up, Down, Left, Right 
                     ]
        where
            -- | Checks if a square at the coordinate contains a letter.
            isOccupied :: Coord -> Bool
            isOccupied coord = letter (board A.! coord) /= ' '
            -- | Calculates the coordinates of a neighbour.
            neighbourCoord :: Coord -> (Int, Int) -> Coord
            neighbourCoord (r,c) (dr, dc) = (r+dr, c+dc)

-- | Generates all potential valid moves by exploring plays from all anchor squares.
--   It calls generatePlaysForAnchor for each anchor and combines the results.
findAllAnchorPlays :: A.Array (Int, Int) Square -- ^ The current board state
                   -> Trie                      -- ^ The Trie dictionary
                   -> String                    -- ^ The players rack
                   -> [Move]                    -- ^ A list of valid moves
findAllAnchorPlays board dictionary rackString =
    let
        anchors = findAnchorSquares board
        initialRack = stringToRack rackString
        -- Generate moves for each anchor and concatenate the results and nub to avoid duplicates
        allMoves = nub $ concatMap (generatePlaysForAnchor board dictionary initialRack) (Set.toList anchors)
    in allMoves

-- | Generates Horizontal and Vertical plays starting from an anchor.
generatePlaysForAnchor :: A.Array (Int, Int) Square -- ^ The current board state
                       -> Trie                      -- ^ The Trie dictionary
                       -> Rack                      -- ^ The players rack
                       -> Coord                     -- ^ The anchor coordinate (row,col)
                       -> [Move]                    -- ^ A list of valid moves from the anchor
generatePlaysForAnchor board dictionary initialRack anchor =
    let maxExtend = boardSize -- Limit for backward search depth
        -- Generate moves horizontally from the anchor
        hMoves = generateDirectionalPlays board dictionary initialRack anchor Horizontal maxExtend
        -- Generate moves vertically from the anchor
        vMoves = generateDirectionalPlays board dictionary initialRack anchor Vertical maxExtend
    in hMoves ++ vMoves


-- | Represents the state (prefix) accumulated during the backward search.
data BwdState = BwdState {
    bwdWordRev      :: String,    -- ^ Chars encountered reversed
    bwdPlacementRev :: Placement, -- ^ Tiles placed from rack reversed
    bwdRack         :: Rack       -- ^ Rack after placing backward tiles.
} deriving (Show)

-- | Core recursive function to generate plays in a specific direction starting from an anchor square. 
--   It works in three phases:
--   1. Backward Phase: Explores sequences of letters before the anchor square finding prefixes.
--   2. Forward Phase: For each prefix, it extends forward from the anchor square,
--   3. Validation Phase: When a potential word end is found the move is validated.
generateDirectionalPlays :: A.Array (Int, Int) Square -- ^ The current board state
                         -> Trie                      -- ^ The Trie dictionary
                         -> Rack                      -- ^ The inital rack
                         -> Coord                     -- ^ The anchor coordinate (row,col)
                         -> Direction                 -- ^ The direciton to explore
                         -> Int                       -- ^ Max exploration depth
                         -> [Move]                    -- ^ Valid moves for the anchor and direction
generateDirectionalPlays board dictionary rack anchor dir maxExtend =
    -- Define coordinate steps for backward and forward movement based on direction
    let (deltaBwd, deltaFwd) = case dir of
                                 Horizontal -> ((0, -1), (0, 1))
                                 Vertical   -> ((-1, 0), (1, 0))

        -- Helper to move one step from a coordinate
        moveCoord :: Coord -> (Int, Int) -> Coord
        moveCoord (r, c) (dr, dc) = (r + dr, c + dc)

        -- Helper to get the character from the original board state.
        getCharAt :: Coord -> Char
        getCharAt coord@(r, c)
            | isValidPosition r c = letter (board A.! coord)
            | otherwise           = '\0' -- Off-board

        -- | Find all possible prefixes ending before the anchor.
        --   Starts exploring backward from the square immediately preceding the anchor.
        findAllPrefixStates :: Coord -> [BwdState]
        findAllPrefixStates anchorPosArg = 
            let preAnchorPos = moveCoord anchorPosArg deltaBwd
                -- Base case: Starting the word at the anchor (no prefix)
                emptyPrefixState = BwdState "" [] rack
            -- Combine the empty prefix state with states found by exploring backward
            in emptyPrefixState : exploreBackward preAnchorPos 0 rack -- Pass initial rack

            where
                -- | Recursive helper to find prefixes.
                --   Explores squares moving away from the anchor.
                --   Returns a list of states representing valid partial words.
                exploreBackward :: Coord -> Int -> Rack -> [BwdState]
                exploreBackward currentPos depth currentBwdRack
                    | depth >= maxExtend = [] -- Stop if max depth reached
                    | otherwise =
                        let charAtPos = getCharAt currentPos -- Get char from original board
                        in if charAtPos == '\0' then [] -- Stop if off board
                            else
                            let nextPos = moveCoord currentPos deltaBwd -- Next position to explore backward
                            in if charAtPos /= ' ' then
                                    -- Use existing board letter: Prepend char to results
                                    map (appendBwdChar charAtPos) (exploreBackward nextPos (depth + 1) currentBwdRack)
                                else
                                    -- Empty square: Try placing rack tiles and combine with results
                                    concatMap (tryPlaceAndRecurseBackward currentPos depth currentBwdRack) (Map.toList currentBwdRack)

                -- | Appends a character to the state returned from a deeper search.
                appendBwdChar :: Char -> BwdState -> BwdState
                appendBwdChar char bwdState = bwdState { bwdWordRev = char : bwdWordRev bwdState }

                -- | Tries placing a rack tile then explores further back and returns combined states.
                tryPlaceAndRecurseBackward :: Coord -> Int -> Rack -> (Char, Int) -> [BwdState]
                tryPlaceAndRecurseBackward currentPos depth parentRack (charToPlace, count) =
                    case useTile charToPlace parentRack of -- Try consuming the tile
                        Nothing -> [] -- Tile not available
                        Just nextRack -> -- Tile consumed
                            let nextPos = moveCoord currentPos deltaBwd
                                -- Explore deeper after placing tile
                                deeperStates = exploreBackward nextPos (depth + 1) nextRack
                                -- Create the state representing the prefix ending with the placed tile
                                stateEndingHere = BwdState [charToPlace] [(fst currentPos, snd currentPos, charToPlace)] nextRack
                                -- Prepend current placement to states returned from deeper calls
                                combinedStates = map (prependBwdPlacement currentPos charToPlace) deeperStates
                            in stateEndingHere : combinedStates

                -- | Prepends a newly placed char to states returned from deeper backward calls.
                prependBwdPlacement :: Coord -> Char -> BwdState -> BwdState
                prependBwdPlacement pos char deeperState =
                    deeperState {
                        bwdWordRev = char : bwdWordRev deeperState,
                        bwdPlacementRev = (fst pos, snd pos, char) : bwdPlacementRev deeperState
                    }

        -- | Extends forward from the anchor, given a prefix from the backward pass.
        extendForward :: Coord -> BwdState -> [Move]
        extendForward anchorPos (BwdState wordPartBwdRev placementBwdRev rackForForward) =
            recurseHelper anchorPos [] "" False rackForForward -- Start forward pass at anchor
            where
              -- | Recursive helper for the forward pass.
              recurseHelper :: Coord -> Placement -> String -> Bool -> Rack -> [Move]
              recurseHelper pos placementAcc wordPartAcc anchorWasCrossed currentFwdRack =
                  let charAtPos = getCharAt pos -- Char on the original board at current position
                      isAnchor = pos == anchorPos
                      -- Update anchor crossed flag
                      nextAnchorCrossed = anchorWasCrossed || (isAnchor && not (null placementAcc || null placementBwdRev)) || (not isAnchor && anchorWasCrossed)

                  in
                  if charAtPos == '\0' then
                       -- Off board: Validate word ending before this position
                       validateMove placementBwdRev wordPartBwdRev placementAcc wordPartAcc currentFwdRack nextAnchorCrossed pos
                  else
                       let nextPos = moveCoord pos deltaFwd
                           -- Option 1: Use existing board letter
                           useBoardLetter =
                               if charAtPos /= ' ' then
                                   let movesEndingHere = validateMove placementBwdRev wordPartBwdRev placementAcc (wordPartAcc ++ [charAtPos]) currentFwdRack nextAnchorCrossed nextPos
                                       movesContinuing = recurseHelper nextPos placementAcc (wordPartAcc ++ [charAtPos]) nextAnchorCrossed currentFwdRack
                                   in movesEndingHere ++ movesContinuing
                               else []
                           -- Option 2: Try placing rack letters if square is empty
                           placeRackLetters =
                               if charAtPos == ' ' then
                                   concatMap (tryPlaceAndGoFwd pos placementAcc wordPartAcc nextAnchorCrossed currentFwdRack) (Map.toList currentFwdRack)
                               else []
                           -- Option 3: Consider word ending before this empty square
                           movesEndingBefore = if charAtPos == ' '
                                                then validateMove placementBwdRev wordPartBwdRev placementAcc wordPartAcc currentFwdRack nextAnchorCrossed pos
                                                else []
                       in useBoardLetter ++ placeRackLetters ++ movesEndingBefore

              -- | Helper for recurseHelper: Tries placing charToPlace at pos, validates, and continues forward.
              tryPlaceAndGoFwd :: Coord -> Placement -> String -> Bool -> Rack -> (Char, Int) -> [Move]
              tryPlaceAndGoFwd pos pAcc wpAcc crossed currentParentRack (charToPlace, count) =
                  case useTile charToPlace currentParentRack of -- Use rack state passed in
                      Nothing -> []
                      Just nextRack -> -- nextRack is rack state after placing charToPlace
                         let newPlacementAcc = (fst pos, snd pos, charToPlace) : pAcc
                             nextWordPartAcc = wpAcc ++ [charToPlace]
                             nextPos = moveCoord pos deltaFwd
                             isAnchor = pos == anchorPos
                             placedAnchor = isAnchor
                             nextAnchorCrossedAfterPlace = crossed || placedAnchor

                             movesEndingHere = validateMove placementBwdRev wordPartBwdRev newPlacementAcc nextWordPartAcc nextRack nextAnchorCrossedAfterPlace nextPos
                             movesContinuing = recurseHelper nextPos newPlacementAcc nextWordPartAcc nextAnchorCrossedAfterPlace nextRack -- Pass nextRack
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
        validateMove placementBwdRev _wordPartBwdRev placementFwdRev _wordPartFwd _rackAfter placementWasCrossed _endPos =
            -- Reverse both lists before combining
            let generatedPlacement = reverse placementBwdRev ++ reverse placementFwdRev
                placedLookup = Map.fromList [((pr, pc), pchar) | (pr, pc, pchar) <- generatedPlacement]

                -- Helper to get char
                getCharAtWithPlacement :: Coord -> Char
                getCharAtWithPlacement coord@(r, c) =
                    case Map.lookup coord placedLookup of
                        Just placedChar -> placedChar
                        Nothing         -> if isValidPosition r c then letter (board A.! coord) else '\0'

                -- Helper: Find prefix/suffix parts for perpendicular words
                findPerpendicularParts :: Coord -> Direction -> (String, String)
                findPerpendicularParts pos p_dir =
                    let (dr_p, dc_p) = case p_dir of Horizontal -> (0, 1); Vertical -> (1, 0)
                        scanBwd :: String -> Coord -> String
                        scanBwd wp current = let prev = (fst current - dr_p, snd current - dc_p); char = getCharAtWithPlacement prev in if char `elem` [' ', '\0'] then wp else scanBwd (char : wp) prev
                        scanFwd :: String -> Coord -> String
                        scanFwd wp current = let next = (fst current + dr_p, snd current + dc_p); char = getCharAtWithPlacement next in if char `elem` [' ', '\0'] then wp else scanFwd (wp ++ [char]) next
                        prefix = scanBwd "" pos
                        suffix = scanFwd "" pos
                    in (prefix, suffix)

                -- Helper: Find start coordinate of a perpendicular word
                findPerpendicularStart :: Coord -> String -> Direction -> Coord
                findPerpendicularStart (r,c) prefix p_dir =
                    let prefixLen = length prefix
                        (dr_p, dc_p) = case p_dir of Horizontal -> (0, 1); Vertical -> (1, 0)
                    in (r - prefixLen * dr_p, c - prefixLen * dc_p)

                -- Helper: Get the full perpendicular word string and its start coord
                getPerpendicularWord :: Coord -> Direction -> Maybe (String, Coord)
                getPerpendicularWord coord crossDir_p =
                    let charAtCoord = getCharAtWithPlacement coord
                    in if charAtCoord `elem` [' ','\0'] then Nothing else
                       let (p_prefix, p_suffix) = findPerpendicularParts coord crossDir_p
                           p_word = p_prefix ++ [charAtCoord] ++ p_suffix
                       in if length p_word > 1 then
                              let p_startCoord = findPerpendicularStart coord p_prefix crossDir_p
                              in Just (p_word, p_startCoord)
                          else Nothing

            in
            -- Basic checks
            if not placementWasCrossed then []
            else if null generatedPlacement then []
            else
               -- 1. Determine the true start and extract the true main word
               let (minR, minC, _) = minimumBy (comparing (\(r,c,_) -> if dir == Horizontal then c else r)) generatedPlacement
                   (dr', dc') = case dir of Horizontal -> (0, 1); Vertical -> (1, 0)
                   findTrueStart currentPos = let prevPos = (fst currentPos - dr', snd currentPos - dc') in if getCharAtWithPlacement prevPos `elem` [' ', '\0'] then currentPos else findTrueStart prevPos
                   trueStartPos = findTrueStart (minR, minC)
                   extractTrueWord start = map getCharAtWithPlacement $ takeWhile (\pos -> let c = getCharAtWithPlacement pos in c /= ' ' && c /= '\0') $ iterate (\p -> moveCoord p (dr', dc')) start
                   trueMainWord = if isValidPosition (fst trueStartPos) (snd trueStartPos) then extractTrueWord trueStartPos else ""
                   wordLen = length trueMainWord
               in
               if wordLen < 2 then []
               else
                   -- Check 2: True Main word in dictionary
                   let isMainWordValid = searchWord dictionary trueMainWord in
                   if not isMainWordValid then []
                   else
                       -- Check 3: Validate perpendicular words formed by placed tiles
                       let crossDir = if dir == Horizontal then Vertical else Horizontal
                           checkPlacedTile (r, c, _) =
                               let coord = (r,c)
                               in case getPerpendicularWord coord crossDir of
                                   Nothing -> (True, Nothing)
                                   Just (p_word, p_startCoord) ->
                                       let isValid = searchWord dictionary p_word
                                       in (isValid, if isValid then Just (p_word, p_startCoord) else Nothing)
                           placedTileChecks = map checkPlacedTile generatedPlacement
                           allPlacedCrossWordsOK = all fst placedTileChecks
                           validScorableCrossInfos = mapMaybe snd placedTileChecks
                       in if not allPlacedCrossWordsOK then []
                          else
                             let mainScore = calculateWordScore board generatedPlacement trueMainWord trueStartPos dir
                                 crossScores = sum $ map (\(cw, csPos) -> calculateWordScore board generatedPlacement cw csPos crossDir) validScorableCrossInfos
                                 bingoBonus = if length generatedPlacement == 7 then 50 else 0
                                 totalScore = mainScore + crossScores + bingoBonus
                             in
                                [Move { mainWord = trueMainWord,
                                        startPos = trueStartPos,
                                        direction = dir,
                                        placement = generatedPlacement, -- Store the combined placement
                                        score = totalScore }]


    in let initialBwdState = BwdState "" [] rack
           -- Get all possible valid states from searching backward before the anchor
           backwardStates = initialBwdState : findAllPrefixStates anchor -- Pass anchor coord
           -- For each backward state, extend forward starting from the anchor
           allMoves = concatMap (extendForward anchor) backwardStates
       in allMoves -- Return all moves found for this anchor/direction


-- | Finds the highest scoring move among all valid moves found.
findHighestScoringPlay :: A.Array (Int, Int) Square -> Trie -> String -> Maybe Move
findHighestScoringPlay board dictionary rack =
    case findAllAnchorPlays board dictionary rack of
        []    -> Nothing
        plays -> Just (maximumBy (comparing score) plays) -- Find move with max score

-- | Function to solve a Scrabble turn. Loads the dictionary and finds the best move.
solveScrabble :: A.Array (Int, Int) Square -> FilePath -> String -> IO (Maybe Move)
solveScrabble board dictionaryFile rackString = do
    dictionary <- loadDictionary dictionaryFile

    if isEmptyTrie dictionary then do
        return Nothing
    else do
        let maybeBestPlay = findHighestScoringPlay board dictionary rackString
        case maybeBestPlay of
             Just m -> putStrLn $ "Found best play: '" ++ mainWord m ++ "' score: " ++ show (score m)
             Nothing -> putStrLn "No valid plays found."
        return maybeBestPlay

