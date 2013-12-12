module Token
    (
    ) where

import qualified Regex as RE
import Control.Applicative

data TokenDefinition tokenNames alphabet
    = TokenDefinition
    { tokenName :: tokenNames
    , tokenRegex :: RE.Regex alphabet
    }

instance (Show tokenNames, Show alphabet, Eq alphabet) => Show (TokenDefinition tokenNames alphabet) where
    show (TokenDefinition name regex) = show name ++ " ::= " ++ show regex

-- this could be generalized to any language instead of strings, but why would you?
parseTokenDefinition :: tokenNames -> String -> Either String (TokenDefinition tokenNames Char)
parseTokenDefinition _ _ = Left "Parser generator not yet implemented so regex parsing not yet possible!"

concatParsedTokenDefinitions :: [Either String (TokenDefinition tokenNames alphabet)] -> Either String [TokenDefinition tokenNames alphabet]
concatParsedTokenDefinitions = foldr (liftA2 (:)) (Right [])
