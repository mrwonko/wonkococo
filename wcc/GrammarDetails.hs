module GrammarDetails where

-- Grammars have to be modified with an additional start symbol, but users need not know that.

import Grammar
import qualified Data.Map as Map

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
