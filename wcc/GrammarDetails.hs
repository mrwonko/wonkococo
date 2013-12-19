module GrammarDetails
    ( TerminalsAndEOF (..)
    , SymbolsAndStart (..)
    , ProductionNamesAndStart (..)
    , grammarWithEOF
    , tokensWithEOF
    , nullables
    , firsts
    ) where

-- Grammars have to be modified with an additional start symbol, but users need not know that.

import Grammar
import qualified Data.Map as Map
import qualified Data.Set as Set
import Token
import Helpers (findFixedPoint)
import CommonTypes ( Position(..) )
import Control.Arrow ( (&&&), first )

data TerminalsAndEOF terminals
    = NormalTerminal terminals
    | EOFTerminal
    deriving (Eq, Ord, Show)

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

-- If a given symbol has a first element, which other symbols have that as well?
firstTransitives :: (Ord symbols)
    => [Production terminals symbols]
    -> (symbols -> Bool)
    -> Map.Map symbols [symbols]
firstTransitives prods nullable
    = Map.map Set.toAscList
    $ Map.fromListWith Set.union
    -- :: [(Symbol, Set Symbol)]
    $ concat
    -- :: [[(Symbol, Set Symbol)]]
    $ map (\(ls, r) -> map (\ l -> (l, Set.singleton r)) ls)
    -- :: [([Symbol], Symbol)]
    $ map (relevantSymbols . productionString &&& productionSymbol)
    -- of those productions starting with symbols :: [Production]
    $ filter (isSymbol . head . productionString)
    -- of those with a nonempty string :: [Production]
    $ filter (not . null . productionString)
    -- of all the productions :: [Production]
    prods
    where
        relevantSymbols [] = []
        relevantSymbols (x:xs) = case x of
            -- Only symbols are relevant.
            Symbol s -> if nullable s
                -- If a symbol is nullable, its successor's first element is relevant, too.
                then s : relevantSymbols xs
                else [s]
            _ -> []

applyFirstTransitives :: (Ord symbols, Ord terminals)
    => Map.Map symbols [symbols]
    -> Map.Map symbols (Set.Set terminals)
    -> Map.Map symbols (Set.Set terminals)
applyFirstTransitives transitives firsts
    = Map.unionWith Set.union firsts
    -- :: Map Symbol (Set Terminal)
    $ Map.fromListWith Set.union
    -- :: [(Symbol, Set Terminal)]
    $ concat
    -- :: [[(Symbol, Set Terminal)]]
    $ map (\ (ls, r) -> map (\l -> (l, r)) ls)
    -- look the transitives up :: [([Symbol], Set Terminal)]
    $ map (first (\x -> Map.findWithDefault [] x transitives))
    -- :: [(Symbol, Set Terminal)]
    $ Map.toAscList firsts

firsts :: (Ord symbols, Ord terminals)
    => Grammar terminals productionNames symbols
    -> Map.Map symbols (Set.Set terminals)
firsts grammar
    = findFixedPoint (applyFirstTransitives (firstTransitives prods nullable))
    $ Map.filter (not . Set.null)
    $ Map.fromListWith Set.union
    $ map (productionSymbol &&& firstTerminals . productionString)
    prods
    where
        prods = Map.elems $ productions grammar
        -- Find all the Terminals at the string start or after nullables
        firstTerminals [] = Set.empty
        firstTerminals (x:xs) = case x of
            Terminal x -> Set.singleton x
            DiscardableTerminal x -> Set.singleton x
            Symbol s -> if nullable s
                then firstTerminals xs
                else Set.empty
        nullable s = s `Set.member` n
        n = nullables grammar -- XXX: would haskell remember the result even if I didn't assign it a name?

-- XXX: WIP alternative implementation of firsts
{-
firstsIteration :: (Ord symbols, Ord terminals)
    => [Production terminals symbols]
    -> Map.Map (ProductionElement terminals symbols) (Set.Set terminals)
    -> Map.Map (ProductionElement terminals symbols) (Set.Set terminals)

firsts :: (Ord symbols, Ord terminals)
    => Grammar terminals productionNames symbols
    -> Map.Map symbols (Set.Set terminals)
firsts grammar
    -- We only care about what Symbols start with.
    = Map.mapKeys (\Symbol s -> s)
    $ Map.filterWithKey
        (\k _ -> case k of
            Symbol _ -> True
            _ -> False)
    $ findFixedPoint (firstsIteration $ Map.elems $ productions grammar) Map.empty
-}
