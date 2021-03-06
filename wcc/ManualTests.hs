module ManualTests where

import CommonTypes
import qualified Grammar
import qualified Scanner
import qualified SyntaxTree as ST
import qualified Regex as RE
import qualified Data.Map as Map
import Data.Char (isAscii, isPrint, ord, chr)
import qualified Data.Foldable as F
import qualified Error
import Token
import qualified GrammarDetails as GD
import qualified SyntaxTreeDetails as STD
-- TODO: Remove either from package dependencies once this is no longer needed
import Control.Monad.Trans.Either (EitherT (..))
import Control.Monad.Trans (lift)
import Control.Monad (liftM)

runTests = foldl (>>) (return ()) tests

tests =
    [ putStrLn "** MY SMALL SCAN **\n\n" >> printTestResults testScan testPrograms
    , putStrLn "** LECTURE EXAMPLE SCAN **\n\n" >> printTestResults lectureExampleScan lectureExamplePrograms
    , putStrLn "** SEQUENTIAL PRINT SCAN **\n\n" >> print testSequentialPrint >> putStrLn "\n"
    ]

--    Scanner Tests    --

type SimpleResult = Error.Result Char (Tokens String Char)

matchResultToString :: SimpleResult -> String
matchResultToString (Left error) = Error.toDetailedString error ++ "\n"
matchResultToString (Right []) = ""
matchResultToString (Right (token:tokens))
    = (++) (positionToString (tPosition token) ++ " " ++ tName token ++ matchIfAny ++ "\n") $ matchResultToString $ Right tokens
    where
        matchIfAny = case tMatch token of
            Nothing -> ""
            Just match -> " (" ++ show match ++ ")"

testScan :: String -> SimpleResult
testScan = Scanner.scan (=='\n') tokenDefs
    where
        tokenDefs =
            [ TokenDefinition "Number" (RE.repeatAtLeast 1 $ RE.range '0' '9') KeepTokenAndMatch
            , TokenDefinition "String" (RE.repeatAtLeast 1 $ RE.range 'a' 'z' `RE.union` RE.range 'A' 'Z') KeepTokenAndMatch
            , TokenDefinition "Whitespace" (RE.repeatAtLeast 1 $ RE.anyOf " \t\r\n") DiscardToken
            --, TokenDefinition "Error" RE.AnySymbol KeepTokenAndMatch
            ]

lectureExampleScan :: String -> SimpleResult
lectureExampleScan = Scanner.scan (=='\n') tokenDefs
    where
        re_az = RE.range 'a' 'z'
        re_az_star = RE.repeat re_az
        re_09 = RE.range '0' '9'
        re_09_plus = RE.repeatAtLeast 1 re_09
        re_09_star = RE.repeat re_09
        tokenDefs =
            [ TokenDefinition "IF" (RE.fromWord "if") KeepToken
            , TokenDefinition "ID" (re_az `RE.concat` RE.repeat (re_az `RE.union` re_09)) KeepTokenAndMatch
            , TokenDefinition "NUM" re_09_plus KeepTokenAndMatch
            , TokenDefinition "REAL" ((RE.repeatAtLeast 1 re_09 `RE.concat` RE.Symbol '.' `RE.concat` re_09_star)
                `RE.union`
                (RE.Symbol '.' `RE.concat` re_09_plus)) KeepTokenAndMatch
            , TokenDefinition "WHITESPACE" (RE.repeatAtLeast 1 (RE.anyOf " \n\t")
                `RE.union`
                (RE.fromWord "--" `RE.concat` re_az_star `RE.concat` RE.Symbol '\n')) DiscardToken
            , TokenDefinition "ERROR" RE.AnySymbol KeepTokenAndMatch
            ]

printResult :: (String -> SimpleResult) -> String -> IO ()
printResult scanner program = putStr $ matchResultToString $ {- fmap (take 2) $ -} scanner program

testPrograms =
    [ "hello world"
    , "hello123"
    , "I contain an exclamation mark!"
    ]

lectureExamplePrograms =
    [ "if8\nif\n1\n.1 1.1"
    , "127.0.0.1"
    , "hello --world\n"
    , "--hello world\n"
    ]

printTestResults :: (String -> SimpleResult) -> [String] -> IO ()
printTestResults scanner = 
    mapM_ (\x -> do
        putStr $ "===     Program     ===\n" ++ x ++ "\n\n=== Scanner results ===\n";
        printResult scanner x;
        putStr "\n\n\n"
    )


--    Grammar Test    --

-- Just a simple grammar to test show

{-
<Sum>  ::= <Term> <Sum2> // Sum
<Sum2> ::=               // Sum2Epsilon
<Sum2> ::= Plus <Sum>    // Sum2Sum
<Term> ::= Number        // Number
-}

data ShowMeGrammarTokenNames = SMGTNNumber | SMGTNPlus deriving (Enum, Show, Eq, Ord)
type ShowMeGrammarTerminals = Token ShowMeGrammarTokenNames Char
data ShowMeGrammarSymbols = SMGSSum | SMGSSum2 | SMGSTerm deriving (Eq, Show, Ord)
data ShowMeGrammarProductions = SMGPSum | SMGPSum2Epsilon | SMGPSum2Sum | SMGPTermNumber | SMGPTest deriving (Ord, Eq, Enum, Show)
showMeGrammar = Grammar.Grammar SMGSSum $ Map.fromList
    -- <Sum> ::= <Term> <Sum2>
    [ ( SMGPSum
      , Grammar.Production SMGSSum True [Grammar.Symbol SMGSTerm, Grammar.Symbol SMGSSum2]
      )
    -- <Sum2> ::=
    , ( SMGPSum2Epsilon
      , Grammar.Production SMGSSum2 True []
      )
    -- <Sum2> ::= Plus <Sum>
    -- careful: no left recursion, thus SMGSSum must not come first
    , ( SMGPSum2Sum
      , Grammar.Production SMGSSum2 True [Grammar.DiscardableTerminal SMGTNPlus, Grammar.Symbol SMGSSum]
      )
    -- <Term> ::= Number
    -- A number is always just a number terminal so it can be simplified to that
    -- (so the abstract syntax tree could be as small as a single leaf)
    , ( SMGPTermNumber
      , Grammar.Production SMGSTerm True [Grammar.Terminal SMGTNNumber]
      )
    
    -- For testing first
    {-
    -- <Term> ::= <Sum2> <Sum>
    , ( SMGPTest
      , Grammar.Production SMGSTerm True [Grammar.Symbol SMGSSum2, Grammar.Symbol SMGSSum]
      )
    --}
    {-
    -- <Sum> ::=
    , ( SMGPTest
      , Grammar.Production SMGSSum True []
      )
    --}
    ]

-- Some trees for the above grammar

showMeGrammarTree1EOF :: STD.SyntaxTreeWithEOF Char ShowMeGrammarProductions ShowMeGrammarTokenNames
showMeGrammarTree1EOF =
    ST.Node GD.StartProductionName
    [ ST.Node (GD.NormalProductionName SMGPSum)
        [ -- this isn't a valid tree, but it's enough to undo EOF
        ]
    , ST.Leaf (Token GD.EOFTerminal Nothing (Position (-1) (-1)))
    ]
showMeGrammarTree1 = STD.syntaxTreeWithoutEOF showMeGrammarTree1EOF

-- "42 + 1337"
showMeGrammarTree2 =
    ST.Node SMGPSum
        [ ST.Node SMGPTermNumber
            [ ST.Leaf (Token SMGTNNumber (Just "42") (Position 1 1))
            ]
        , ST.Node SMGPSum2Sum
            [ ST.Leaf (Token SMGTNPlus Nothing (Position 1 4))
            , ST.Node SMGPSum
                [ ST.Node SMGPTermNumber
                    [ ST.Leaf (Token SMGTNNumber (Just "1337") (Position 1 6))
                    ]
                , ST.Node SMGPSum2Epsilon []
                ]
            ]
        ]

printTree tree = putStrLn $ ST.drawTree tree

printSimplifiedTree grammar tree = runEitherT $ do
    tree <- EitherT $ return $ STD.simplifySyntaxTree grammar tree
    lift $ printTree tree
    return ()

--    Parser Test    --

--  Simple Grammar for sequential prints  --

data SequentialPrintTokenName
    = SPTNPrint
    | SPTNOpenParen
    | SPTNCloseParen
    | SPTNStringLiteral
    | SPTNWhitespace
    | SPTNComment
    deriving (Eq, Show)

sequentialPrintUnescape :: String -> String
sequentialPrintUnescape [] = []
sequentialPrintUnescape ('\\':x:xs) = x:sequentialPrintUnescape xs
sequentialPrintUnescape (x:xs) = x:sequentialPrintUnescape xs

type SequentialPrintToken = Token SequentialPrintTokenName Char
type SequentialPrintScanResult = Error.Result Char [SequentialPrintToken]

sequentialPrintScan :: String -> SequentialPrintScanResult
sequentialPrintScan = Scanner.scan (=='\n') tokenDefs
    where
        allChars = filter isPrint $ map chr [0..127]
        re_allChars = RE.anyOf allChars
        re_allCharsExcept x = RE.anyOf $ filter (not . (`elem` x)) allChars
        
        tokenDefs =
            [ TokenDefinition SPTNPrint (RE.fromWord "print") KeepToken
            , TokenDefinition SPTNOpenParen (RE.Symbol '(') KeepToken
            , TokenDefinition SPTNCloseParen (RE.Symbol ')') KeepToken
            -- "([^\\"]|\\.)*"    (where . is a printable character)
            , TokenDefinition SPTNStringLiteral (RE.Symbol '"' `RE.concat`
                RE.repeat (re_allCharsExcept "\\\"" `RE.union` (RE.Symbol '\\' `RE.concat` re_allChars))
                `RE.concat` RE.Symbol '"') KeepTokenAndMatch
            , TokenDefinition SPTNWhitespace (RE.repeatAtLeast 1 $ RE.anyOf " \n\t") DiscardToken
            -- //[^\n]*\n?
            , TokenDefinition SPTNComment (RE.fromWord "//" `RE.concat`
                RE.repeat (re_allCharsExcept "\n") `RE.concat`
                RE.repeatAtMost 1 (RE.Symbol '\n')) DiscardToken
            ]

-- Removes "" around strings, unescapes them
sequentialPrintPostprocessString :: String -> String
sequentialPrintPostprocessString s
    = init $ tail $ unescape s
    where
        unescape ('\\':x:xs) = x : unescape xs
        unescape (x:xs) = x : unescape xs
        unescape [] = []

sequentialPrintPostprocessToken :: SequentialPrintToken -> SequentialPrintToken
sequentialPrintPostprocessToken t @ (Token SPTNStringLiteral (Just match) _)
    = t { tMatch = Just $ sequentialPrintPostprocessString match }
sequentialPrintPostprocessToken t = t

sequentialPrintPostScan :: SequentialPrintScanResult -> SequentialPrintScanResult
sequentialPrintPostScan = fmap $ map sequentialPrintPostprocessToken

testSequentialPrint = sequentialPrintPostScan
    $ sequentialPrintScan
    "print(\"Hello \\\"World\\\"\") // printing hello world\nprint(\"foo \\bar\")"

{-
    (Throw whitespace away during scanning)
    Block ::= epsilon | Statement Block'
    Block' ::= epsilon | Block
    Statement ::= PrintKeyword OpenParen StringLiteral CloseParen
-}
