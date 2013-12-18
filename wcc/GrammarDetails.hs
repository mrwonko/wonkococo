module GrammarDetails
    ( TerminalsAndEOF (..)
    , SymbolsAndStart (..)
    , ProductionNamesAndStart (..)
    , grammarWithEOF
    , tokensWithEOF
    , nullables
    ) where

-- Grammars have to be modified with an additional start symbol, but users need not know that.

import Grammar
import qualified Data.Map as Map
import qualified Data.Set as Set
import Token
import Helpers (findFixedPoint)
import CommonTypes ( Position(..) )

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
    => Grammar terminals productionNames symbols
    -> Grammar (TerminalsAndEOF terminals) (ProductionNamesAndStart productionNames) (SymbolsAndStart symbols)
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

tokensWithEOF :: [Token tokenNames alphabet] -> [Token (TerminalsAndEOF tokenNames) alphabet]
tokensWithEOF = (++ [Token EOFTerminal Nothing (Position (-1) (-1))]) . map convertToken
    where
        convertToken token = token { tName = NormalTerminal $ tName token}

productionElementNullable :: (Ord symbols)
    => Set.Set symbols
    -> ProductionElement terminals symbols
    -> Bool
productionElementNullable _ (Terminal _)             = False
productionElementNullable _ (DiscardableTerminal _)  = False
productionElementNullable nullableSymbols (Symbol s) = s `Set.member` nullableSymbols

nullablesIteration :: (Ord symbols) 
    => [Production terminals symbols]
    -> Set.Set symbols
    -> Set.Set symbols
nullablesIteration prods nullableSymbols = newSymbols `Set.union` nullableSymbols
    where
        -- Create a Set
        newSymbols = Set.fromList
            -- of the symbols of the productions
            $ map productionSymbol
            -- whose strings are nullable
            $ filter (all (productionElementNullable nullableSymbols) . productionString)
            -- out of all of them
            prods

nullables :: (Ord symbols)
    => Grammar terminals productionNames symbols
    -> Set.Set symbols
nullables grammar = findFixedPoint (nullablesIteration $ Map.elems $ productions grammar) Set.empty
