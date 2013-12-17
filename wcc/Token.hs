module Token
    ( TokenDefinition (..)
    , TokenDefinitions
    , TokenType (..)
    , mapTokenDefinitionRegex
    
    , Token (..)
    , Tokens
    , mapTokenMatch
    ) where

import qualified Regex as RE
import Error
import CommonTypes ( Position, positionToString )


--    Token Definition    --

data TokenDefinition tokenNames alphabet
    = TokenDefinition
    { tdName :: tokenNames
    , tdRegex :: RE.Regex alphabet
    , tdType :: TokenType
    }

mapTokenDefinitionRegex :: (RE.Regex a1 -> RE.Regex a2) -> TokenDefinition tokens a1 -> TokenDefinition tokens a2
mapTokenDefinitionRegex f (TokenDefinition name regex type') = TokenDefinition name (f regex) type'

type TokenDefinitions tokenNames alphabet = [TokenDefinition tokenNames alphabet]

data TokenType = KeepToken | KeepTokenAndMatch | DiscardToken
    deriving (Show)

instance (Show tokenNames, Show alphabet, Eq alphabet) => Show (TokenDefinition tokenNames alphabet) where
    show (TokenDefinition name regex type') = show name ++ " (" ++ show type' ++ ") ::= " ++ show regex


--    Token    --

data Token tokenNames alphabet
    = Token
    { tName :: tokenNames
    , tMatch :: Maybe [alphabet]
    , tPosition :: Position
    }
    deriving (Eq)

type Tokens tokenNames alphabet = [Token tokenNames alphabet]

instance (Show tokenNames, Show alphabet) => Show (Token tokenNames alphabet) where
    show (Token name Nothing      position) = show name ++ " @" ++ positionToString position
    show (Token name (Just match) position) = show name ++ " @" ++ positionToString position ++ " " ++ show match

mapTokenMatch :: ([a1] -> [a2]) -> Token tokens a1 -> Token tokens a2
mapTokenMatch f (Token name match position) = Token name (fmap f match) position
