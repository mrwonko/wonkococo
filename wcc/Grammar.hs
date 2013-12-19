module Grammar
    ( Grammar (..)
    , ProductionElement (..)
    , Production (..)
    , isDiscardable
    , isSymbol
    , fromSymbol
    ) where

import qualified Data.Map as Map

data Grammar terminals productionNames symbols = Grammar
    { startSymbol :: symbols
    , productions :: Map.Map productionNames (Production terminals symbols)
    }

data Production terminals symbols
    = Production
    { productionSymbol :: symbols
    -- Some productions serve only to define precedence, which is unambiguous in a tree, so they can be discarded
    -- Those must not have more than 1 non-discardable production element
    , productionDiscardable :: Bool
    , productionString :: [ProductionElement terminals symbols]
    }

data ProductionElement terminals symbols
    = Terminal terminals
    -- Many terminals are only used in one production so the productionName uniquely identifies the result and they are discardable
    | DiscardableTerminal terminals
    | Symbol symbols
    deriving (Eq, Ord)

instance (Show terminals, Show symbols) => Show (ProductionElement terminals symbols) where
    show (Terminal t) = show t
    show (DiscardableTerminal t) = "(" ++ show t ++ ")"
    show (Symbol s)   = "<"  ++ show s ++ ">"

instance (Show terminals, Show symbols) => Show (Production terminals symbols) where
    show (Production symbol discardable string) = foldl (\ l r -> l ++ " " ++ show r) accum string
        where accum = "<" ++ show symbol ++ ">" ++ if discardable then " (discardable)" else "" ++ " ::="

instance (Show terminals, Show symbols, Eq symbols, Show productionNames, Ord productionNames)
    => Show (Grammar terminals symbols productionNames) where
    show (Grammar startSymbol productions)
        = Map.foldlWithKey concatProduction
        (   Map.foldlWithKey concatProduction "==  Grammar   ==\n= Start Symbol ="
            (Map.filter isStartProduction productions)
            ++ "\n\n=     Rest     ="
        )
        $ Map.filter (not . isStartProduction) productions
        where
            isStartProduction prod = productionSymbol prod == startSymbol
            concatProduction accum productionName production
                = accum ++ "\n" ++ show productionName ++ ":\n  "
                ++ show production

isDiscardable :: ProductionElement terminals symbols -> Bool
isDiscardable (DiscardableTerminal _) = True
isDiscardable _ = False

isSymbol :: ProductionElement terminals symbols -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

fromSymbol :: ProductionElement terminals symbols -> symbols
fromSymbol (Symbol s) = s
