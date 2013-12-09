module Grammar
    ( Grammar( Grammar )
    , TerminalOrSymbol( Terminal, Symbol )
    ) where

import qualified Data.Map as Map

data Grammar terminals symbols productionNames = Grammar
    { startSymbol :: symbols
    , productions :: Map.Map productionNames (symbols, [TerminalOrSymbol terminals symbols])
    }

data TerminalOrSymbol terminals symbols
    = Terminal terminals
    | Symbol symbols
    deriving (Eq)

instance (Show terminals, Show symbols) => Show (TerminalOrSymbol terminals symbols) where
    show (Terminal t) = show t
    show (Symbol s)   = "<"  ++ show s ++ ">"

instance (Show terminals, Show symbols, Eq symbols, Show productionNames) => Show (Grammar terminals symbols productionNames) where
    show (Grammar startSymbol productions)
        = Map.foldlWithKey concatProduction
        (   (Map.foldlWithKey concatProduction "==  Grammar   ==\n= Start Symbol ="
            $ Map.filter (\(symbol, _) -> symbol == startSymbol) productions)
        ++ "\n\n=     Rest     =")
        $ Map.filter (\(symbol, _) -> symbol /= startSymbol) productions
        where
            concatProduction accum productionName (symbol, production)
                = accum ++ "\n" ++ show productionName ++ ":\n  "
                ++ foldl (\ l r -> l ++ " " ++ show r) ("<" ++ show symbol ++ "> ::=") production
