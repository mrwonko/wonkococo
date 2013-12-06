module ManualTests where

import CommonTypes
import qualified Scanner
import qualified Regex as RE
import qualified Data.Map as Map

matchResultToString :: Either (Scanner.Error Char) [PositionInformation (Scanner.Match Char String)] -> String
matchResultToString (Left error) = Scanner.errorToString (Just show) error ++ "\n"
matchResultToString (Right []) = ""
matchResultToString (Right (PositionInformation position (Scanner.Match word tokenName):xs))
    = (++) (positionToString position ++ " " ++ tokenName ++ " (" ++ show word ++ ")\n") $ matchResultToString $ Right xs

testScan :: String -> Either (Scanner.Error Char) [PositionInformation (Scanner.Match Char String)]
testScan = Scanner.scan (Map.fromList tokenDefs) Just (=='\n')
    where
        tokenDefs =
            [ ("1Numbers", RE.repeatAtLeast 1 $ RE.range '0' '9')
            , ("2Strings", RE.repeatAtLeast 1 $ RE.range 'a' 'z' `RE.union` RE.range 'A' 'Z')
            , ("3Whitespace", RE.repeatAtLeast 1 $ RE.anyOf " \t\r\n")
            --, ("4Error", RE.AnySymbol)
            ]

lectureExampleScan :: String -> Either (Scanner.Error Char) [PositionInformation (Scanner.Match Char String)]
lectureExampleScan = Scanner.scan (Map.fromList tokenDefs) Just (=='\n')
    where
        re_az = RE.range 'a' 'z'
        re_az_star = RE.repeat re_az
        re_09 = RE.range '0' '9'
        re_09_plus = RE.repeatAtLeast 1 re_09
        re_09_star = RE.repeat re_09
        tokenDefs =
            [ ("1 - IF", RE.fromWord "if")
            , ("2 - ID", re_az `RE.concat` RE.repeat (re_az `RE.union` re_09))
            , ("3 - NUM", re_09_plus)
            , ("4 - REAL", (RE.repeatAtLeast 1 re_09 `RE.concat` RE.Symbol '.' `RE.concat` re_09_star)
                `RE.union`
                (RE.Symbol '.' `RE.concat` re_09_plus))
            , ("5 - WHITESPACE", RE.repeatAtLeast 1 (RE.anyOf " \n\t")
                `RE.union`
                (RE.fromWord "--" `RE.concat` re_az_star `RE.concat` RE.Symbol '\n'))
            , ("6 - ERROR", RE.AnySymbol)
            ]

printResult scanner program = putStr $ matchResultToString $ scanner program

testPrograms =
    [ "wello world"
    , "hello123"
    , "I contain an exclamation mark!"
    ]

lectureExamplePrograms =
    [ "if8 if 1 .1 1.1"
    , "127.0.0.1"
    , "hello --world\n"
    , "--hello world\n"
    ]

printTestResults scanner programs = 
    mapM_ (\x -> do
        putStr $ "===     Program     ===\n" ++ x ++ "\n\n=== Scanner results ===\n";
        printResult scanner x;
        putStr "\n\n\n"
    ) programs