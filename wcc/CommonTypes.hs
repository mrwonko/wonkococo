module CommonTypes
    ( Position( Position )
    , LocationInformation( LocationInformation )
    ) where

data Position = Position
    { line :: Int       -- We won't have billions of LOC in a single file. Or shouldn't.
    , character :: Int  -- Same for characters per line.
    }
    deriving (Eq)

instance Show Position where
    show (Position line character) = "line " ++ show line ++ " character " ++ show character

data LocationInformation x = LocationInformation Position x
    deriving (Eq, Show)    
