module GrammarDetails where

-- Grammars have to be modified with an additional start symbol, but users need not know that.

import Grammar
import qualified Data.Map as Map
import SyntaxTree
import Error
import Token
import Control.Applicative ((<$>))
import Control.Monad (mapM)

data TerminalsAndEOF terminals
    = NormalTerminal terminals
    | EOFTerminal
    deriving (Eq, Show)

data SymbolsAndStart symbols
    = NormalSymbol symbols
    | StartSymbol
    deriving (Eq, Ord, Show)

data ProductionNamesAndStart productionNames
    = NormalProductionName productionNames
    | StartProductionName
    deriving (Eq, Ord, Show)

grammarWithEOF
    :: Ord productionNames
    => Grammar terminals symbols productionNames
    -> Grammar (TerminalsAndEOF terminals) (SymbolsAndStart symbols) (ProductionNamesAndStart productionNames)
grammarWithEOF (Grammar startSymbol productions)
    = Grammar StartSymbol newProductions
    where
        newProductions
            -- Add a new production that maps to former start symbol and EOF
            = Map.insert StartProductionName startProduction
            -- add prefix to production names to distinguish them from the new start production name
            $ Map.mapKeysMonotonic NormalProductionName
            -- convert productions to distinguish them from the start production
            $ Map.map convertProduction productions
        startProduction = Production StartSymbol True [Symbol (NormalSymbol startSymbol), DiscardableTerminal EOFTerminal]
        convertProduction (Production symbol discardable string)
            = Production (NormalSymbol symbol) discardable (convertProductionString string)
        convertProductionString = map convertProductionElement
        convertProductionElement (Terminal t) = Terminal $ NormalTerminal t
        convertProductionElement (Symbol i) = Symbol $ NormalSymbol i
        convertProductionElement (DiscardableTerminal t) = DiscardableTerminal $ NormalTerminal t

terminalsWithEOF :: [terminals] -> [TerminalsAndEOF terminals]
terminalsWithEOF = (++ [EOFTerminal]) . map NormalTerminal

type SyntaxTreeWithEOF productionNames terminals
    = SyntaxTree (ProductionNamesAndStart productionNames) (TerminalsAndEOF terminals)

syntaxTreeWithoutEOF
    :: SyntaxTreeWithEOF productionNames (Tokens tokenNames alphabet)
    -> Result alphabet (SyntaxTree productionNames (Tokens tokenNames alphabet))

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
