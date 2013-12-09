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

data ProductionsNamesAndStart productionNames
    = NormalProductionName productionNames
    | StartProduction
    deriving (Eq, Ord, Show)

grammarWithEOF
    :: Ord productionNames
    => Grammar terminals symbols productionNames
    -> Grammar (TerminalsAndEOF terminals) (SymbolsAndStart symbols) (ProductionsNamesAndStart productionNames)
grammarWithEOF (Grammar startSymbol productions)
    = Grammar StartSymbol newProductions
    where
        newProductions
            = Map.insert StartProduction (StartSymbol, [Symbol (NormalSymbol startSymbol), Terminal EOFTerminal])
            $ Map.mapKeysMonotonic NormalProductionName
            $ Map.map convertProduction productions
        convertProduction (productionName, x) = (NormalSymbol productionName, map convertProductionElement x)
        convertProductionElement (Terminal t) = Terminal $ NormalTerminal t
        convertProductionElement (Symbol i) = Symbol $ NormalSymbol i

terminalsWithEOF :: [terminals] -> [TerminalsAndEOF terminals]
terminalsWithEOF = (++ [EOFTerminal]) . map NormalTerminal
