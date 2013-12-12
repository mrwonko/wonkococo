module Scanner
    (-- scan
    ) where

import qualified Regex as RE
import CommonTypes
import qualified Data.Map as Map
import Control.Exception ( assert )
import Token
import Error
import Control.Monad
import Control.Monad.State

data Mark alphabet tokenNames
    = NoMark
    | Mark
        -- match
        (Token alphabet tokenNames)
        -- Position after match
        Position
        -- remaining word
        [alphabet]


-- Mutable Data for scanner
data ScannerData alphabet tokenNames
    = ScannerData
    { mark :: Mark alphabet tokenNames
    , position :: Position
    , tokenDefinitions :: TokenDefinitions tokenNames alphabet
    , reverseMatch :: [alphabet]
    , matchPosition :: Position
    , reverseResults :: Tokens tokenNames alphabet -- do x <- fmap reverse reverseResults
    , word :: [alphabet]
    }

type ScannerState alphabet tokenNames = StateT (ScannerData alphabet tokenNames) (Result alphabet) ()

putError :: CompilerError alphabet -> ScannerState alphabet tokenNames
putError error = StateT $ \ _ -> Left error

setMark :: Token alphabet tokenNames -> Position -> [alphabet] -> ScannerState alphabet tokenNames
setMark token position remainingWord = modify $ \ d -> d { mark = Mark token position remainingWord }

removeMark :: ScannerState alphabet tokenNames
removeMark =  modify $ \ d -> d { mark = NoMark }

-- Reads a character, errors on EOF
readCharacter :: Eq alphabet => (alphabet -> Bool) -> ScannerState alphabet tokenNames
readCharacter isNewline = do
    oldWord <- gets word
    case oldWord of
        -- End of Input?
        [] -> do
            err <- fmap (UnexpectedEndOfFile . reverse) (gets reverseMatch)
            putError err
        (curChar:newWord) -> modify $ \ oldState -> oldState
            { matchPosition = (if isNewline curChar then incLine else incChar) (position oldState)
            , reverseMatch = curChar : reverseMatch oldState
            -- Token Definitions may be empty now.
            , tokenDefinitions = do
                    definition <- fmap (mapTokenDefinitionRegex (RE.deriveCharacter curChar)) (tokenDefinitions oldState)
                    guard $ tdRegex definition /= RE.NullSet
                    return definition
            }

initialData :: TokenDefinitions tokenNames alphabet -> [alphabet] -> ScannerData alphabet tokenNames
initialData initialTokenDefinitions word = ScannerData
    { mark = NoMark
    , position = Position 1 1
    , tokenDefinitions = initialTokenDefinitions
    , reverseMatch = []
    , matchPosition = Position 1 1
    , reverseResults = []
    , word = word
    }

{-

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
    = scan' NoMark beginning tokenDefinitions (PositionInformation beginning []) []
    where
        beginning = Position 1 1
        scan'
            -- Last valid match, if any
            mark -- TODO: use State instead?
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
                Mark {}
                    -> backtrack
            -- Regexes may find a result yet, be greedy and read further
            | otherwise  = case remainingWord of
                -- end of input. Are we in a valid match?
                [] -> case mark of
                    -- No valid match yet. Maybe there has been no new input since the last one?
                    NoMark -> case currentReversePrefix of
                        -- No new input, so no need to generate any more
                        []        -> Right $ reverse reverseMatchAccum
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

-}

