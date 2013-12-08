module SimpleGrammar
    ( Grammar( Grammar )
    , TerminalOrIntermediate( Terminal, Intermediate )
    , grammarWithEOF
    , terminalsWithEOF
    ) where

import qualified Data.Map as Map

data Grammar terminals intermediates = Grammar
    { startSymbol :: intermediates
    , productions :: Map.Map intermediates [[TerminalOrIntermediate terminals intermediates]]
    }
    deriving (Show)

data TerminalOrIntermediate terminals intermediates
    = Terminal terminals
    | Intermediate intermediates
    deriving (Eq, Show)

data TerminalsAndEOF terminals
    = NormalTerminal terminals
    | EOFTerminal
    deriving (Eq, Show)

data IntermediatesAndStart intermediates
    = NormalIntermediate intermediates
    | StartIntermediate
    deriving (Eq, Ord, Show)

grammarWithEOF
    :: Ord intermediates
    => Grammar terminals intermediates
    -> Grammar (TerminalsAndEOF terminals) (IntermediatesAndStart intermediates)
grammarWithEOF (Grammar startSymbol productions)
    = Grammar StartIntermediate newProductions
    where
        newProductions = Map.insert StartIntermediate [[Intermediate (NormalIntermediate startSymbol), Terminal EOFTerminal]]
            $ Map.mapKeysMonotonic NormalIntermediate
            $ Map.map convertProductions productions
        convertProductions = map $ map convertProductionElement
        convertProductionElement (Terminal t) = Terminal $ NormalTerminal t
        convertProductionElement (Intermediate i) = Intermediate $ NormalIntermediate i

terminalsWithEOF :: [terminals] -> [TerminalsAndEOF terminals]
terminalsWithEOF = (++ [EOFTerminal]) . map NormalTerminal