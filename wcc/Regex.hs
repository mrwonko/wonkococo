module Regex
    ( Regex (EmptyWord, Symbol, AnySymbol)
    , Regex.concat
    , repeat
    , union
    , intersection
    , difference
    , range
    , repeatExactly
    , repeatAtLeast
    , repeatAtMost
    , repeatBetween
    ) where

import Prelude hiding (concat, repeat)


--    regex type    --


data Eq alphabet => Regex alphabet = NullSet        -- matches nothing
    | EmptyWord                                     -- matches the empty string
    | Symbol alphabet                               -- matches a single symbol
    | AnySymbol                                     -- matches any symbol
    | Concat (Regex alphabet) (Regex alphabet)      -- matches any concatenation of words where the first is from the first set and the second from the second
    | Repeat (Regex alphabet)                       -- matches any number of repeated concatenations of words from the given set (even 0)
    | Union (Regex alphabet) (Regex alphabet)       -- matches a word in either of the given sets
    | Intersection (Regex alphabet) (Regex alphabet)-- matches any word that is in both the first set and the second one
    | Difference (Regex alphabet) (Regex alphabet)  -- matches any word that is in the first set but not the second
    deriving Eq

instance (Show alphabet, Eq alphabet) => Show (Regex alphabet) where
    show NullSet            = "{}"
    show EmptyWord          = "()"
    show (Symbol s)         = show s
    show (AnySymbol)        = "."
    show (Concat l r)       = show l ++ show r
    show (Repeat r)         = "(" ++ show r ++ ")*"
    show (Union l r)        = "(" ++ show l ++ "|" ++ show r ++ ")"
    show (Intersection l r) = "(" ++ show l ++ "&" ++ show r ++ ")"
    show (Difference l r)   = "(" ++ show l ++ "-" ++ show r ++ ")"


--    smart constructors    --


concat :: Eq alphabet => Regex alphabet -> Regex alphabet -> Regex alphabet
-- Pre-/suffixing the empty word changes nothing 
concat EmptyWord regex = regex
concat regex EmptyWord = regex
-- Concatenation of NullSet yields nothing since there's nothing in NullSet
concat NullSet _ = NullSet
concat _ NullSet = NullSet
concat l r = Concat l r 

repeat :: Eq alphabet => Regex alphabet -> Regex alphabet
-- Repeating the empty word changes nothing
repeat EmptyWord = EmptyWord
-- Repeating the empty set 0 times yields an empty word, anything more nothing.
repeat NullSet = EmptyWord
repeat regex = Repeat regex

union :: Eq alphabet => Regex alphabet -> Regex alphabet -> Regex alphabet
-- empty set is the neutral element of union
union NullSet regex = regex
union regex NullSet = regex
-- union with yourself changes nothing
union l r
    | l == r = l
    | otherwise = Union l r

intersection :: Eq alphabet => Regex alphabet -> Regex alphabet -> Regex alphabet
-- Intersection with an empty set yields an empty set
intersection NullSet _ = NullSet
intersection _ NullSet = NullSet
-- Intersection with yourself changes nothing 
intersection l r
    | l == r    = l
    | otherwise = Intersection l r

difference :: Eq alphabet => Regex alphabet -> Regex alphabet -> Regex alphabet
-- You can't remove anything from an empty set
difference NullSet _ = NullSet
-- Removing nothing from a set changes nothing
difference regex NullSet = regex
-- Removing everything leaves nothing
difference l r
    | l == r    = NullSet
    | otherwise = Difference l r 


--    convenience constructors    --


range :: (Enum alphabet, Eq alphabet) => alphabet -> alphabet -> Regex alphabet
range low high = foldl union NullSet $ map Symbol $ enumFromTo low high

repeatExactly :: (Eq alphabet) => Int -> Regex alphabet -> Regex alphabet
repeatExactly 0 _ = EmptyWord
repeatExactly 1 regex = regex
repeatExactly n regex
    | n < 0     = NullSet
    | n == 0    = EmptyWord
    | n == 1    = regex
    | otherwise = concat regex $ repeatExactly (n-1) regex

repeatAtLeast :: (Eq alphabet) => Int -> Regex alphabet -> Regex alphabet
repeatAtLeast n regex = concat (repeatExactly n regex) (repeat regex) 

repeatAtMost :: (Eq alphabet) => Int -> Regex alphabet -> Regex alphabet
repeatAtMost n regex
    | n < 0     = NullSet
    | n == 0    = EmptyWord
    | otherwise = union (repeatExactly n regex) $ repeatAtMost (n-1) regex

repeatBetween :: (Eq alphabet) => Int -> Int -> Regex alphabet -> Regex alphabet
repeatBetween low high regex
    | low > high = NullSet
    | otherwise  = concat (repeatExactly low regex) $ repeatAtMost (high - low) regex 
