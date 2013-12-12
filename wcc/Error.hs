module Error where

import CommonTypes

data CompilerError alphabet
    = IllegalCharacter (PositionInformation [alphabet]) -- string containing illegal character & current token match prefix
    | UnexpectedEndOfFile [alphabet] -- string read while encountering premature EOF
    | GeneralCompilerError String
    deriving (Show)

instance Error (CompilerError a) where
    noMsg    = GeneralCompilerError "Unspecified Error!"
    strMsg s = GeneralCompilerError s

type CompilerMonad = Either CompilerError

toString :: CompilerError alphabet -> String
toString (IllegalCharacter (PositionInformation (Position line char) _)) = "Illegal character at line " ++ show line ++ " char " ++ show char ++ "!"
toString (UnexpectedEndOfFile _) = "Unexpected end Of file!"
toString (GeneralCompilerError str) = str
toString _ = "Undocumented error!"

toDetailedString :: (Show alphabet) => CompilerError alphabet -> String
toDetailedString (IllegalCharacter (PositionInformation (Position line char) str)) = "Illegal character at line " ++ show line ++ " char " ++ show char ++ " in " ++ show str ++ "!"
-- default: Just 
toDetailedString error = toString error
