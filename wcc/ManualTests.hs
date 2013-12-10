module ManualTests where

import CommonTypes
import qualified Grammar
import qualified Scanner
import qualified SyntaxTree as ST
import qualified Regex as RE
import qualified Data.Map as Map
import Data.Char (isAscii, isPrint, ord, chr)
import qualified Data.Foldable as F

--    Scanner Tests    --

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
    [ "if8\nif\n1\n.1 1.1"
    , "127.0.0.1"
    , "hello --world\n"
    , "--hello world\n"
    ]

printTestResults scanner = 
    mapM_ (\x -> do
        putStr $ "===     Program     ===\n" ++ x ++ "\n\n=== Scanner results ===\n";
        printResult scanner x;
        putStr "\n\n\n"
    )


--    Grammar Test    --

-- Just a simple grammar to test show

data ShowMeGrammarTerminals = SMGTNumber | SMGTPlus deriving (Enum, Show)
data ShowMeGrammarSymbols = SMGSSum | SMGSTerm deriving (Eq, Show)
data ShowMeGrammarProductions = SMGPSingleSum | SMGPMultiSum | SMGPNumber deriving (Ord, Eq, Enum, Show)
showMeGrammar = Grammar.Grammar SMGSSum $ Map.fromList
    [ (SMGPSingleSum, (SMGSSum, [Grammar.Symbol SMGSTerm]))
    -- careful: no left recursion, thus SMGSSum may not come first
    , (SMGPMultiSum, (SMGSSum, [Grammar.Symbol SMGSTerm, Grammar.Terminal SMGTPlus, Grammar.Symbol SMGSSum]))
    , (SMGPNumber, (SMGSTerm, [Grammar.Terminal SMGTNumber]))
    ]

--    Parser Test    --

--  Simple Grammar for sequential prints  --

data SequentialPrintTokenName
    = SPTNPrint
    | SPTNOpenParen
    | SPTNCloseParen
    | SPTNStringLiteral
    | SPTNWhitespace
    | SPTNComment
    deriving (Eq, Ord)

data SequentialPrintToken
    = SPPrint
    | SPOpenParen
    | SPCloseParen
    | SPStringLiteral String
    deriving (Show)

sequentialPrintUnescape :: String -> String
sequentialPrintUnescape [] = []
sequentialPrintUnescape ('\\':x:xs) = x:sequentialPrintUnescape xs
sequentialPrintUnescape (x:xs) = x:sequentialPrintUnescape xs

sequentialPrintScan :: String -> Either (Scanner.Error Char) [PositionInformation SequentialPrintToken]
sequentialPrintScan = Scanner.scan (Map.fromList tokenDefs) postProcess (=='\n')
    where
        postProcess (Scanner.Match _ SPTNPrint) = Just SPPrint
        postProcess (Scanner.Match _ SPTNOpenParen) = Just SPOpenParen
        postProcess (Scanner.Match _ SPTNCloseParen) = Just SPCloseParen
        postProcess (Scanner.Match match SPTNStringLiteral) = Just $ SPStringLiteral $ sequentialPrintUnescape $ init $ tail match
        postProcess (Scanner.Match _ _) = Nothing -- Throw away Whitespace and Comment
        
        allChars = filter isPrint $ map chr [0..127]
        re_allChars = RE.anyOf allChars
        re_allCharsExcept x = RE.anyOf $ filter (not . (`elem` x)) allChars
        
        tokenDefs =
            [ (SPTNPrint, RE.fromWord "print")
            , (SPTNOpenParen, RE.Symbol '(')
            , (SPTNCloseParen, RE.Symbol ')')
            -- "([^\\"]|\\.)*"    (where . is a printable character)
            , (SPTNStringLiteral, RE.Symbol '"' `RE.concat`
                RE.repeat (re_allCharsExcept "\\\"" `RE.union` (RE.Symbol '\\' `RE.concat` re_allChars))
                `RE.concat` RE.Symbol '"')
            , (SPTNWhitespace, RE.repeatAtLeast 1 $ RE.anyOf " \n\t")
            -- //[^\n]*\n?
            , (SPTNComment, RE.fromWord "//" `RE.concat` RE.repeat (re_allCharsExcept "\n") `RE.concat` RE.repeatAtMost 1 (RE.Symbol '\n'))
            ]

{-
    (Throw whitespace away during scanning)
    Block := Statement | Statement Block
    Statement := PrintKeyword OpenParen StringLiteral CloseParen
-}

data SequentialPrintBlock
    -- stmt
    = SPBStatement SequentialPrintStatement
    -- stmt block
    | SPBBlock SequentialPrintStatement SequentialPrintBlock

data SequentialPrintStatement
    -- print("...")
    = SPSPrint String

data T1 a = T1

test :: T1 (T1 (T1 ()))
test = T1

{-
sequentialPrintGrammar :: Grammar.Grammar 
sequentialPrintGrammar = Grammar.Grammar
-}

--    Syntax Tree Tests    --

tree1 = ST.Node 1 [ST.Leaf 'A', ST.Leaf 'B', ST.Node 2 [], ST.Node 3 [ST.Leaf 'C']]
tree1leaves = F.foldr (:) [] tree1
nodeTree1 = ST.NodeSyntaxTree tree1
tree1nodes = F.foldr (:) [] nodeTree1
tree2 = fmap (:"-Leaf") tree1
nodeTree2 = fmap (*2) nodeTree1