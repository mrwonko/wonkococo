module Scanner
    ( scan
    ) where

import qualified Regex as RE
import CommonTypes
import qualified Data.Map as Map
import Control.Exception ( assert )

-- TODO: create a test file for stuff like this and move this there
testScanner :: String -> Either String [LocationInformation (String, String)]
testScanner = scan (Map.fromList tokenDefs) $ ((.).(.)) Just (,)
    where
        tokenDefs =
            [ ("Numbers", RE.repeatAtLeast 1 $ RE.range '0' '9')
            , ("Strings", RE.repeatAtLeast 1 $ RE.range 'a' 'z' `RE.union` RE.range 'A' 'Z')
            , ("Whitespace", RE.repeatAtLeast 1 $ RE.anyOf " \t\r\n")
            , ("Error", RE.AnySymbol)
            ]

data Mark alphabet tokenName
    = NoMark
    | Mark Position [alphabet] tokenName

deriveCharacterNotNull :: Eq alphabet => alphabet -> RE.Regex alphabet -> Maybe (RE.Regex alphabet)
deriveCharacterNotNull char regex = case RE.deriveCharacter regex char of
    -- Note: this assumes that the smart constructors take care of simplifying regexes down to NullSet where possible.
    -- Note: That could be fatal! It would potentially result in a complete scan of the remaining word after each derivation!
    RE.NullSet -> Nothing
    r          -> assert (not $ RE.null r) $ Just r

scan ::
    (Eq alphabet, Ord tokenNames)
    -- Token names and their regular expressions (token order matters!)
    => Map.Map tokenNames (RE.Regex alphabet)
    -- Constructor for tokens, whatever that may be, from a name and the match
    -> (tokenNames -> [alphabet] -> Maybe token)
    -- word to tokenize
    -> [alphabet]
    -- Result: Error or list of tokens with line/char info
    -> Either String [LocationInformation token]
scan tokenDefinitions tokenConstructor = fmap reverse . scan' NoMark (Position 0 0) tokenDefinitions []
    where
        scan'
            -- last token match (or NoMark)
            :: Mark alphabet tokenNames
            -- current position in word
            -> Position
            -- current token definitions (derived)
            ->Map.Map tokenNames (RE.Regex alphabet)
            -- result accumulator in reverse order (because that's faster)
            -> [LocationInformation token]
            -- remaining word
            -> [alphabet]
            -- Result: Error or list of tokens with line/char info
            -> Either String [LocationInformation token]
        scan' mark position currentTokenDefinitions tokens word = Left "Scanner-generation not yet implemented!"

-- just map, then filter
-- Data.Map.filterWithKey
-- or mapMaybe/mapMaybeWithKey