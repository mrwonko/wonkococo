module CommonTypes
    ( Position( Position )
    , incChar
    , incLine
    , PositionInformation( PositionInformation )
    ) where

data Position = Position
    { line :: Int       -- We won't have billions of LOC in a single file. Or shouldn't.
    , character :: Int  -- Same for characters per line.
    }
    deriving (Eq)

instance Show Position where
    show (Position line character) = "line " ++ show line ++ " character " ++ show character

incChar :: Position -> Position
incChar (Position line char) = Position line (char + 1)

incLine :: Position -> Position
incLine (Position line char) = Position (line + 1) 1

data PositionInformation x = PositionInformation Position x
    deriving (Eq, Show)    
