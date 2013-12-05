module Regex
    ( Regex (EmptyWord, Symbol, AnySymbol)
    , Regex.concat
    , repetition
    , union
    , intersection
    , difference
    ) where

data Eq alphabet => Regex alphabet = NullSet        -- matches nothing
    | EmptyWord                                     -- matches the empty string
    | Symbol alphabet                               -- matches a single symbol
    | AnySymbol                                     -- matches any symbol
    | Concat (Regex alphabet) (Regex alphabet)      -- matches any concatenation of words where the first is from the first set and the second from the second
    | Repetition (Regex alphabet)                   -- matches any number of repeated concatenations of words from the given set (even 0)
    | Union (Regex alphabet) (Regex alphabet)       -- matches a word in either of the given sets
    | Intersection (Regex alphabet) (Regex alphabet)-- matches any word that is in both the first set and the second one
    | Difference (Regex alphabet) (Regex alphabet)  -- matches any word that is in the first set but not the second
    deriving Eq

instance (Show alphabet, Eq alphabet) => Show (Regex alphabet) where
    show (EmptyWord)        = "()"
    show (Symbol s)         = show s --escapeRegexString $ show s
    show (AnySymbol)        = "."
    show (Concat l r)       = show l ++ show r
    show (Repetition r)     = "(" ++ show r ++ ")*"
    show (Union l r)        = "(" ++ show l ++ "|" ++ show r ++ ")"
    show (Intersection l r) = "(" ++ show l ++ "&" ++ show r ++ ")"
    show (Difference l r)   = "(" ++ show l ++ "-" ++ show r ++ ")"

{- TODO remove?
regexSpecialChars :: String
regexSpecialChars = "\\.[]+-|*?&" -- + and ? are not actually used for display, but might confuse

escapeRegexString :: String -> String
escapeRegexString [] = []
escapeRegexString (x:xs)
    | x `elem` regexSpecialChars = '\\' : x : escapedXs
    | otherwise = x : escapedXs
    where escapedXs = escapeRegexString xs
-}

concat :: Eq alphabet => Regex alphabet -> Regex alphabet -> Regex alphabet
concat EmptyWord regex = regex
concat regex EmptyWord = regex
concat NullSet _ = NullSet
concat _ NullSet = NullSet
concat l r = Concat l r 

repetition :: Eq alphabet => Regex alphabet -> Regex alphabet
repetition EmptyWord = EmptyWord
repetition NullSet = EmptyWord
repetition regex = Repetition regex

union :: Eq alphabet => Regex alphabet -> Regex alphabet -> Regex alphabet
union NullSet regex = regex
union regex NullSet = regex
union l r
    | l == r = l
    | otherwise = Union l r

intersection :: Eq alphabet => Regex alphabet -> Regex alphabet -> Regex alphabet
intersection NullSet _ = NullSet
intersection _ NullSet = NullSet
intersection l r
    | l == r    = l
    | otherwise = Intersection l r

difference :: Eq alphabet => Regex alphabet -> Regex alphabet -> Regex alphabet
difference NullSet _ = NullSet
difference regex NullSet = regex
difference l r
    | l == r    = NullSet
    | otherwise = Difference l r 
