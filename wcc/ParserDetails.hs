module ParserDetails where

import Parser
import SyntaxTreeDetails
import GrammarDetails

parserWithoutEOF :: (Ord productionNames)
    => Parser alphabet (TerminalsAndEOF tokenNames) (ProductionNamesAndStart productionNames) (SymbolsAndStart symbols)
    -> Parser alphabet tokenNames productionNames symbols
parserWithoutEOF parser grammar tokens = do
    let grammar' = grammarWithEOF grammar
    let tokens' = tokensWithEOF tokens
    tree <- parser grammar' tokens'
    syntaxTreeWithoutEOF tree