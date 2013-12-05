module Scanner
    ( generateScanner
    ) where

import qualified Regex as RE
import CommonTypes
import qualified Data.Map as Map

generateScanner ::
    (Eq alphabet, Ord tokenNames)
    -- Token names and their regular expressions (token order matters!)
    => Map.Map tokenNames (RE.Regex alphabet)
    -- Constructor for tokens, whatever that may be, from a name and the match
    -> (tokenNames -> [alphabet] -> Maybe token)
    -- Output: Scanner, which takes a word and tries to generate a token stream
    -> [alphabet] -> Either String [LocationInformation token]
generateScanner tokenDefinitions tokenConstructor = scanner
    where
        scanner :: [alphabet] -> Either String [LocationInformation token]
        scanner word = Left "Scanner-generation not yet implemented!"
