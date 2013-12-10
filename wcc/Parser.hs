module Parser where

{-
    Generate an Abstract Syntax Tree, i.e. drop irrelevant tokens:
    
    1.  Brackets are required to define precedence in string representation but are implicit in tree.
    2.  Operators are implicitly saved in production name;
        I only need to know *what's* added, and that it's *added*, not that it's added /with a plus/
    
    1 is possible by annotating the tokens in the productions.
    2 can be partially automated - in productions that, aside from discardable tokens, are X ::= X.
    But usually it's something like Factor ::= "(" Expression ")"
    So annotation seems more feasible than automation.
-}
