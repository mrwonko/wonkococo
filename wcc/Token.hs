module Token
    ( TokenDefinition (..)
    , TokenDefinitions
    , TokenType (..)
    , mapTokenDefinitionRegex
    , parseTokenDefinition
    , concatParsedTokenDefinitions
    
    , Token (..)
    , Tokens
    , mapTokenMatch
    ) where

import qualified Regex as RE
import Control.Applicative
import Error
import CommonTypes ( Position )


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

-- this could be generalized to any language instead of strings, but why would you?
parseTokenDefinition :: tokenNames -> String -> TokenType -> Result Char (TokenDefinition tokenNames Char)
parseTokenDefinition _ _ _ = Left $ NotYetImplementedError "Regex Parsing"

concatParsedTokenDefinitions
    :: [Result Char (TokenDefinition tokenNames alphabet)] -> Result Char (TokenDefinitions tokenNames alphabet)
concatParsedTokenDefinitions = foldr (liftA2 (:)) (Right [])


--    Token    --

data Token tokenNames alphabet
    = Token
    { tName :: tokenNames
    , tMatch :: Maybe [alphabet]
    , tPosition :: Position
    }
    deriving (Eq, Show)

type Tokens tokenNames alphabet = [Token tokenNames alphabet]

mapTokenMatch :: ([a1] -> [a2]) -> Token tokens a1 -> Token tokens a2
mapTokenMatch f (Token name match position) = Token name (fmap f match) position
