module Error where

import CommonTypes
import Control.Monad.Error

data CompilerError alphabet
    = IllegalCharacter Position [alphabet] -- string containing illegal character & current token match prefix
    | UnexpectedEndOfFile [alphabet] -- string read while encountering premature EOF
    | InvalidDiscardableProduction -- In the grammar definition a production was marked as Discardable, but it multiple children
    | NotYetImplementedError String -- component/function not yet implemented
    | AssertionError String -- Where did the programmer err?
    | GeneralCompilerError String
    deriving (Show)

instance Error (CompilerError a) where
    noMsg  = GeneralCompilerError "Unspecified Error!"
    strMsg = GeneralCompilerError

-- XXX: Does this belong here?
type Result alphabet = Either (CompilerError alphabet)

toString :: CompilerError alphabet -> String
toString (IllegalCharacter (Position line char) _) = "Illegal character at line " ++ show line ++ " char " ++ show char ++ "!"
toString (UnexpectedEndOfFile _) = "Unexpected end of file!"
toString InvalidDiscardableProduction = "Production with multiple non-discardable elements marked as discardable!"
toString (NotYetImplementedError what) = what ++ " not yet implemented!"
toString (AssertionError what) = "Internal precondition violated: " ++ what ++ " (the coder screwed up, report this)"
toString (GeneralCompilerError str) = str

toDetailedString :: (Show alphabet) => CompilerError alphabet -> String
toDetailedString (IllegalCharacter (Position line char) str)
    = "Illegal character at line " ++ show line ++ " char " ++ show char ++ " in " ++ show str ++ "!"
toDetailedString (UnexpectedEndOfFile curPrefix) = "Unexpected end of file reading " ++ show curPrefix ++ "!"
-- default: Just 
toDetailedString error = toString error
