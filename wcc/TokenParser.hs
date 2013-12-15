module TokenParser
    -- For when you want to use a string as your regular expression.
    ( parseTokenDefinition
    ) where

import Token
import Error
import Control.Monad (sequence)

-- this could be generalized to any language instead of strings, but why would you?
-- use Control.Monad.sequence to concatenate the results if you create a list of these.
parseTokenDefinition :: tokenNames -> String -> TokenType -> Result Char (TokenDefinition tokenNames Char)
parseTokenDefinition _ _ _ = Left $ NotYetImplementedError "Regex Parsing"
