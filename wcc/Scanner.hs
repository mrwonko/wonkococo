module Scanner
    ( scan
    ) where

import qualified Regex as RE
import CommonTypes
import Token
import Error
import Control.Monad
import Control.Monad.State

data Mark tokenNames alphabet
    = NoMark
    | Mark
        -- match
        (Maybe (Token tokenNames alphabet))
        -- Position after match
        Position
        -- remaining word
        [alphabet]
    deriving (Show)


-- Mutable Data for scanner
data ScannerData tokenNames alphabet
    = ScannerData
    { mark :: Mark tokenNames alphabet
    , position :: Position
    , tokenDefinitions :: TokenDefinitions tokenNames alphabet
    , reverseMatch :: [alphabet]
    , matchPosition :: Position
    , reverseResults :: Tokens tokenNames alphabet -- do x <- fmap reverse reverseResults
    , word :: [alphabet]
    }

type ScannerState tokenNames alphabet = StateT (ScannerData tokenNames alphabet) (Result alphabet) ()

putError :: CompilerError alphabet -> ScannerState tokenNames alphabet
putError error = StateT $ \ _ -> Left error

setMark :: Maybe (Token tokenNames alphabet) -> Position -> [alphabet] -> ScannerState tokenNames alphabet
setMark token position remainingWord = modify $ \ d -> d { mark = Mark token position remainingWord }

-- Places a mark, if possible.
-- i.e. if any token matches the current prefix
markIfPossible :: ScannerState tokenNames alphabet
markIfPossible = do
    state <- get
    -- We're interested in the first token that matches the current prefix
    let emptyWordMatches = filter (RE.matchesEmptyWord . tdRegex) $ tokenDefinitions state
    unless (null emptyWordMatches) $ do
        -- What's its name? Where is it? Are we supposed to keep it?
        let
            tokenDefinition = head emptyWordMatches
            tokenName = tdName tokenDefinition
            tokenPosition = matchPosition state
            tokenMatch = Just $ reverse $ reverseMatch state
            token = case tdType tokenDefinition of
                DiscardToken -> Nothing
                KeepToken -> Just $ Token tokenName Nothing tokenPosition
                KeepTokenAndMatch -> Just $ Token tokenName tokenMatch tokenPosition 
        setMark token (position state) (word state)

-- Reads a character.
-- Precondition: There is a character to read. LogicError otherwise.
readCharacter :: Eq alphabet => (alphabet -> Bool) -> ScannerState tokenNames alphabet
readCharacter isNewline = do
    oldWord <- gets word
    case oldWord of
        -- End of Input?
        [] -> putError $ AssertionError "readCharacter called at EOF"
        -- Modify state accordingly with new char
        (curChar:newWord) -> modify $ \ oldState -> oldState
            -- Remember new position
            { position = (if isNewline curChar then incLine else incChar) (position oldState)
            -- Remember newly read character
            , reverseMatch = curChar : reverseMatch oldState
            -- Adjust remaining word
            , word = newWord
            -- derive the token definition regexes
            -- >> This is the central idea behind the matching! Deriving Regular Expressions. <<
            -- Token Definitions may be empty now, meaning we need not read further, there won't be more matches.
            -- But that's not for us to check.
            , tokenDefinitions = do
                definition <- fmap (mapTokenDefinitionRegex (RE.deriveCharacter curChar)) (tokenDefinitions oldState)
                -- XXX: How expensive is the definitelyNull check?
                guard $ not $ RE.definitelyNull $ tdRegex definition
                return definition
            }

-- Backtracks, if possible, errors otherwise.
backtrack :: TokenDefinitions tokenNames alphabet -> CompilerError alphabet -> ScannerState tokenNames alphabet
backtrack initialTokenDefinitions onFailure = do
    m <- gets mark
    case m of
        NoMark -> putError onFailure
        Mark maybeToken markedPosition markedWord -> do
            modify $ \ d -> d
                { position = markedPosition
                , word = markedWord
                , reverseMatch = []
                , matchPosition = markedPosition
                , tokenDefinitions = initialTokenDefinitions
                , mark = NoMark
                }
            case maybeToken of
                Just token -> modify $ \ d -> d { reverseResults = token : reverseResults d }
                otherwise -> return ()

extractResults :: ScannerData tokenNames alphabet -> Tokens tokenNames alphabet
extractResults = reverse . reverseResults

initialData :: TokenDefinitions tokenNames alphabet -> [alphabet] -> ScannerData tokenNames alphabet
initialData initialTokenDefinitions word = ScannerData
    { mark = NoMark
    , position = Position 1 1
    , tokenDefinitions = initialTokenDefinitions
    , reverseMatch = []
    , matchPosition = Position 1 1
    , reverseResults = []
    , word = word
    }

scan :: (Eq alphabet)
    -- Newline check
    => (alphabet -> Bool)
    -- Token Definitions
    -> TokenDefinitions tokenNames alphabet
    -- Word to scan
    -> [alphabet]
    -- Resulting Tokens
    -> Result alphabet (Tokens tokenNames alphabet)
scan isNewline initialTokenDefinitions initialWord =
    fmap extractResults $ execStateT scanStep $ initialData initialTokenDefinitions initialWord
    where
        backtrack' = backtrack initialTokenDefinitions
        readCharacter' = readCharacter isNewline
        scanStep = do
            state <- get
            -- Are we at EOF?
            if null (word state)
                -- We're at EOF. If we're not in the middle of a match, that's okay and we're done.
                -- But usually we'll have to try to backtrack and continue.
                then unless (null $ reverseMatch state) $ do
                    backtrack' $ UnexpectedEndOfFile $ reverse $ reverseMatch state
                    scanStep
                -- Not at EOF. Do any token definitions remain?
                else if null $ tokenDefinitions state
                    -- No tokens match anymore. Try to backtrack and continue.
                    then do
                        backtrack' $ IllegalCharacter (position state) (reverse $ reverseMatch state)
                        scanStep
                    -- We might find more matches yet! Onward!
                    else do
                        readCharacter'
                        markIfPossible
                        scanStep

