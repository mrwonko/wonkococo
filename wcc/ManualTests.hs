module ManualTests where

import CommonTypes
import qualified Scanner
import qualified Regex as RE
import qualified Data.Map as Map

testScan :: String -> Either (Scanner.Error Char) [PositionInformation (Scanner.Match Char String)]
testScan = Scanner.scan (Map.fromList tokenDefs) Just (=='\n')
    where
        tokenDefs =
            [ ("1Numbers", RE.repeatAtLeast 1 $ RE.range '0' '9')
            , ("2Strings", RE.repeatAtLeast 1 $ RE.range 'a' 'z' `RE.union` RE.range 'A' 'Z')
            , ("3Whitespace", RE.repeatAtLeast 1 $ RE.anyOf " \t\r\n")
            --, ("4Error", RE.AnySymbol)
            ]

matchResultToString :: Either (Scanner.Error Char) [PositionInformation (Scanner.Match Char String)] -> String
matchResultToString (Left error) = Scanner.errorToString (Just id) error ++ "\n"
matchResultToString (Right []) = ""
matchResultToString (Right (PositionInformation position (Scanner.Match word tokenName):xs))
    = (++) (positionToString position ++ " " ++ tokenName ++ " (" ++ word ++ ")\n") $ matchResultToString $ Right xs
