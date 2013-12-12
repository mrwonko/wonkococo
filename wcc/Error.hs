module Error where

import CommonTypes
import Control.Monad.Error

data CompilerError alphabet
    = IllegalCharacter Position [alphabet] -- string containing illegal character & current token match prefix
    | UnexpectedEndOfFile [alphabet] -- string read while encountering premature EOF
    | NotYetImplementedError String -- component/function not yet implemented
    | GeneralCompilerError String
    deriving (Show)

instance Error (CompilerError a) where
    noMsg  = GeneralCompilerError "Unspecified Error!"
    strMsg = GeneralCompilerError

-- TODO: Does this belong here?
type Result alphabet = Either (CompilerError alphabet)

toString :: CompilerError alphabet -> String
toString (IllegalCharacter (Position line char) _) = "Illegal character at line " ++ show line ++ " char " ++ show char ++ "!"
toString (UnexpectedEndOfFile _) = "Unexpected end of file!"
toString (NotYetImplementedError what) = what ++ " not yet implemented!"
toString (GeneralCompilerError str) = str

toDetailedString :: (Show alphabet) => CompilerError alphabet -> String
toDetailedString (IllegalCharacter (Position line char) str)
    = "Illegal character at line " ++ show line ++ " char " ++ show char ++ " in " ++ show str ++ "!"
toDetailedString (UnexpectedEndOfFile curPrefix) = "Unexpected end of file reading " ++ show curPrefix ++ "!"
-- default: Just 
toDetailedString error = toString error
