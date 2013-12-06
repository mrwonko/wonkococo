module Scanner
    ( Error
    , errorToString
    , Match( Match )
    , scan
    ) where

import qualified Regex as RE
import CommonTypes
import qualified Data.Map as Map
import Control.Exception ( assert )

data Match alphabet tokenNames = Match [alphabet] tokenNames
    deriving (Eq, Show)

data Mark alphabet tokenNames
    = NoMark
    | Mark
        -- match
        (Match alphabet tokenNames)
        -- Position after match
        Position
        -- remaining word
        [alphabet]

data Error alphabet
    = IllegalCharacter (PositionInformation [alphabet])
    | UnexpectedEndOfFile [alphabet]
    deriving (Show)

errorToString :: Maybe ([alphabet] -> String) -> Error alphabet -> String
errorToString maybeWordConverter error = case error of
    IllegalCharacter (PositionInformation position word) -> "Illegal character at " ++ positionToString position ++
        case maybeWordConverter of
            Nothing            -> []
            Just wordConverter -> " in " ++ wordConverter word
    UnexpectedEndOfFile word -> "Unexpected end of file" ++
        case maybeWordConverter of
            Nothing            -> []
            Just wordConverter -> " while reading " ++ wordConverter word
    

deriveCharacterNotNull :: Eq alphabet => alphabet -> RE.Regex alphabet -> Maybe (RE.Regex alphabet)
deriveCharacterNotNull char regex = case RE.deriveCharacter regex char of
    -- Note: this assumes that the smart constructors take care of simplifying regexes down to NullSet where possible.
    -- Note: That could be fatal! It would potentially result in a complete scan of the remaining word after each derivation!
    RE.NullSet -> Nothing
    r          -> assert (not $ RE.null r) $ Just r

scan ::
    -- Alphabet needs to be showable for errors (could supply a toString function as parameter if that's ever undesirable?)
    (Eq alphabet, Show alphabet, Ord tokenNames)
    -- Token names and their regular expressions (token order matters!)
    => Map.Map tokenNames (RE.Regex alphabet)
    -- Constructor for tokens, whatever that may be, from a name and the match
    -> (Match alphabet tokenNames -> Maybe token)
    -- Check for Line break (for position display)
    -> (alphabet -> Bool)
    -- word to tokenize
    -> [alphabet]
    -- Result: Error or list of tokens with line/char info
    -> Either (Error alphabet) [PositionInformation token]
scan tokenDefinitions tokenConstructor isNewLine
    = fmap reverse . scan' NoMark beginning tokenDefinitions (PositionInformation beginning []) []
    where
        beginning = Position 1 1
        scan'
            -- Last valid match, if any
            mark
            -- current position in word
            position
            -- current derivations of the token definition regexes
            currentTokenDefinitions
            -- current prefix being checked (reverse for cons speed)
            (PositionInformation currentPrefixPosition currentReversePrefix)
            -- result accumulator (reverse for cons speed)
            reverseMatchAccum
            -- remaining unscanned word
            remainingWord
            
            -- No regexes will match, no need to read any further, go back to last matching position, if any
            | Map.null currentTokenDefinitions = case mark of
                -- No match yet -> illegal input
                NoMark
                    -> Left $ IllegalCharacter $ PositionInformation position currentPrefix
                -- Had a match -> go back there and add it to result (backtracking! This is why this is not O(n).)
                Mark markedMatch markedPosition markedRemainingWord
                    -> backtrack
            -- Regexes may find a result yet, be greedy and read further
            | otherwise  = case remainingWord of
                -- end of input. Are we in a valid match?
                [] -> case mark of
                    -- No valid match yet. Maybe there has been no new input since the last one?
                    NoMark -> case currentReversePrefix of
                        -- No new input, so no need to generate any more
                        []        -> Right reverseMatchAccum
                        -- Had new input which didn't match anything so far, throw an error.
                        otherwise -> Left $ UnexpectedEndOfFile currentPrefix
                    -- We're in a valid match, backtrack.
                    Mark {} -> backtrack
                -- there is input to eat
                (nextChar:newRemainingWord) -> let
                    -- derive all regexes, throw away those that won't ever match anything again
                    newTokenDefinitions = Map.mapMaybe (deriveCharacterNotNull nextChar) currentTokenDefinitions
                    newPosition = if isNewLine nextChar then incLine position else incChar position
                    -- add new character to current prefix (to front for speed, reverse it when setting Mark)
                    newReversePrefix = nextChar : currentReversePrefix
                    newPrefixInfo = PositionInformation currentPrefixPosition newReversePrefix
                    -- if this is a match, set a mark.
                    newMark = let matchingTokenDefinitions = Map.filter RE.matchesEmptyWord newTokenDefinitions in
                        if Map.null matchingTokenDefinitions
                            -- no matches, keep old mark
                            then mark
                            -- matches! Create a mark with the first match.
                            else Mark
                                (Match (reverse newReversePrefix) $ fst $ head $ Map.toAscList matchingTokenDefinitions)
                                newPosition
                                newRemainingWord
                    in scan' newMark newPosition newTokenDefinitions newPrefixInfo reverseMatchAccum newRemainingWord 
            where
                currentPrefix = reverse currentReversePrefix
                -- only call if mark is a Mark
                backtrack = let
                    (Mark markedMatch markedPosition markedRemainingWord) = mark
                    newReverseMatchAccum = case tokenConstructor markedMatch of
                            -- token constructor decided to keep the token - add location information and save it.
                            Just newToken -> PositionInformation currentPrefixPosition newToken:reverseMatchAccum
                            -- token constructor decided to throw the token away. (Comment or whatever.)
                            Nothing       -> reverseMatchAccum
                    in scan'
                        -- new scan has nothing found yet
                        NoMark
                        -- starts after the last match
                        markedPosition
                        -- uses the underived original token regexes
                        tokenDefinitions
                        -- new prefix starts after the last one, is empty so far
                        (PositionInformation markedPosition [])
                        -- result accumulator might have had a new token added to it
                        newReverseMatchAccum
                        -- parse the remaining word
                        markedRemainingWord
