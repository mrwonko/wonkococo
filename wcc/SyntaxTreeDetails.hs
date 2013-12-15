module SyntaxTreeDetails
    ( SyntaxTreeWithEOF
    , syntaxTreeWithoutEOF
    , simplifySyntaxTree
    ) where

import GrammarDetails ( TerminalsAndEOF(..), ProductionNamesAndStart(..) )
import Grammar
import SyntaxTree
import Error
import Token
import Helpers ( maybeToEither )

import Control.Applicative ( (<$>) )
import Control.Monad ( mapM )
import qualified Data.Map as Map

type SyntaxTreeWithEOF productionNames terminals
    = SyntaxTree (ProductionNamesAndStart productionNames) (TerminalsAndEOF terminals)

syntaxTreeWithoutEOF
    :: SyntaxTreeWithEOF productionNames (Token tokenNames alphabet)
    -> Result alphabet (SyntaxTree productionNames (Token tokenNames alphabet))

-- EOF leaf? shouldn't be there, EOFTerminal is a DiscardableTerminal.
syntaxTreeWithoutEOF (Leaf EOFTerminal)
    = Left $ AssertionError "syntaxTreeWithoutEOF called on an invalid syntax tree with an EOF leaf"

-- Normal leaf
syntaxTreeWithoutEOF (Leaf (NormalTerminal terminal))
    = Right $ Leaf terminal

-- Start production
syntaxTreeWithoutEOF (Node StartProductionName [child])
    = syntaxTreeWithoutEOF child
-- Start production should only have one child since the terminal EOF following it is discardable.
syntaxTreeWithoutEOF (Node StartProductionName _)
    = Left $ AssertionError "syntaxTreeWithoutEOF called on an invalid syntax tree with a StartProduction node with multiple/0 children"

-- Normal production
syntaxTreeWithoutEOF (Node (NormalProductionName productionName) children)
    = Node productionName <$> mapM syntaxTreeWithoutEOF children

-- Discards what's discardable. Side-effect of verifying the tree matches the productions.
simplifySyntaxTree :: (Ord productionNames, Eq tokenNames, Eq symbols)
    -- Grammar which defines discardable productions and terminals
    => Grammar (Token tokenNames alphabet) symbols productionNames
    -- Tree to simplify
    -> SyntaxTree productionNames (Token tokenNames alphabet)
    -- Simplified tree
    -> Result alphabet (SyntaxTree productionNames (Token tokenNames alphabet))
simplifySyntaxTree _ (Leaf _) = Left $ AssertionError "simplifySyntaxTree: Called on leaf, which is no valid syntax tree!"
simplifySyntaxTree grammar node = do
    list <- simplifySyntaxTree' grammar (startSymbol grammar) True node
    if null list
        then Left $ AssertionError "simplifySyntaxTree' returned [] for root node, it shouldn't do that!"
        else return $ head list

-- returns [] if the current node is completely discardable (e.g. a comment)
-- except for the root node (isRoot)
-- otherwise returns [simplified node]
simplifySyntaxTree' :: (Ord productionNames, Eq tokenNames, Eq symbols)
    -- Grammar which defines discardable productions and terminals
    => Grammar (Token tokenNames alphabet) symbols productionNames
    -- Symbol the current node must match
    -> symbols
    -- Whether this is the root node
    -> Bool
    -- Tree to simplify
    -> SyntaxTree productionNames (Token tokenNames alphabet)
    -- Simplified tree
    -> Result alphabet [(SyntaxTree productionNames (Token tokenNames alphabet))]
simplifySyntaxTree' _ _ _ (Leaf _) = Left $ AssertionError "simplifySyntaxTree' called on Leaf!"
simplifySyntaxTree' grammar symbol isRoot (Node productionName children) = do
    production <- lookupProduction productionName grammar
    if productionSymbol production /= symbol
        then Left $ AssertionError
            "simplifySyntaxTree: Tree does not match grammar, node production does not match parents' requirements"
        else do
            simplifiedChildren <- simplifyNodeForest grammar productionName production children
            -- can only discard if we have a parent or there is exactly one child
            if productionDiscardable production && (not isRoot || not (null simplifiedChildren))
                then if length simplifiedChildren > 1
                    then Left InvalidDiscardableProduction
                    else if null simplifiedChildren -- implies not isRoot
                        then return []
                    else -- length simplifiedChildren == 1 
                        return [head simplifiedChildren]
                else -- not discardable
                    return [Node productionName simplifiedChildren]

lookupProduction :: (Ord productionNames)
    => productionNames
    => Grammar (Token tokenNames alphabet) symbols productionNames
    -> Result alphabet (Production (Token tokenNames alphabet) symbols)
lookupProduction productionName grammar = maybeToEither
    (AssertionError "simplifySyntaxTree: syntax tree contains production name that's not in the grammar")
    $ Map.lookup productionName (productions grammar)

simplifyNodeForest :: (Ord productionNames, Eq tokenNames, Eq symbols)
    => Grammar (Token tokenNames alphabet) symbols productionNames
    -- Which production's forest is this?
    -> productionName
    -> Production (Token tokenNames alphabet) symbols
    -- What to simplify?
    -> [SyntaxTree productionNames (Token tokenNames alphabet)]
    -> Result alphabet [SyntaxTree productionNames (Token tokenNames alphabet)]
simplifyNodeForest grammar prodName production forest = do
    children <- sequence $ simplifyNodeForest' grammar (productionString production) forest
    return children

simplifyNodeForest' :: (Ord productionNames, Eq tokenNames, Eq symbols)
    => Grammar (Token tokenNames alphabet) symbols productionNames
    -- Production string this should match
    -> [ProductionElement (Token tokenNames alphabet) symbols]
    -> [SyntaxTree productionNames (Token tokenNames alphabet)]
    -> [Result alphabet (SyntaxTree productionNames (Token tokenNames alphabet))]
simplifyNodeForest' _ [] [] = []
simplifyNodeForest' _ _ [] = [Left $ AssertionError "Syntax Tree does not match production; Node with too few children"]
simplifyNodeForest' _ [] _ = [Left $ AssertionError "Syntax Tree does not match production; Node with too many children"]
simplifyNodeForest' grammar (productionElement:productionElements) (tree:trees)
    = simplifyNodeTree grammar productionElement tree ++ simplifyNodeForest' grammar productionElements trees

expectedTerminalError :: Result a b
expectedTerminalError = Left $ AssertionError "Syntax Tree does not match production; Expected Terminal, got Symbol"

expectedSymbolError :: Result a b
expectedSymbolError = Left $ AssertionError "Syntax Tree does not match production; Expected Symbol, got Terminal"

differingTerminalError :: Result a b
differingTerminalError = Left $ AssertionError "Syntax Tree does not match production; Differing Terminals"

differingSymbolError :: Result a b
differingSymbolError = Left $ AssertionError "Syntax Tree does not match production; Differing Symbols"

simplifyNodeTree :: (Ord productionNames, Eq tokenNames, Eq symbols)
    => Grammar (Token tokenNames alphabet) symbols productionNames
    -> ProductionElement (Token tokenNames alphabet) symbols
    -> SyntaxTree productionNames (Token tokenNames alphabet)
    -> [Result alphabet (SyntaxTree productionNames (Token tokenNames alphabet))]
simplifyNodeTree grammar (Terminal _) (Node _ _)
    = [expectedTerminalError]
simplifyNodeTree grammar (DiscardableTerminal _) (Node _ _)
    = [expectedTerminalError]
simplifyNodeTree grammar (Symbol _) (Leaf _)
    = [expectedSymbolError]
simplifyNodeTree grammar (Terminal t) (Leaf l)
    | tName t == tName l = [return $ Leaf l]
    | otherwise = [differingTerminalError]
simplifyNodeTree grammar (DiscardableTerminal t) (Leaf l)
    | tName t == tName l = [] -- discard
    | otherwise = [differingTerminalError]
simplifyNodeTree grammar (Symbol symbol) tree
    = case simplifySyntaxTree' grammar symbol False tree of
        Left error -> [Left error]
        Right list -> map Right list