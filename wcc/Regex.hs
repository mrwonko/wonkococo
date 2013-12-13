module Regex
    --    constructing Regular Expressions    --
    ( Regex (NullSet, EmptyWord, Symbol, AnySymbol)
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
    , fromWord
    , anyOf
    , anyBut
    
    --    Working with regular expressions    --
    -- Does a regular expression definitely match no words at all? O(n), actually checks!
    -- Caution: False negatives possible if difference is used! No real way to prevent that.
    , definitelyNull
    -- Does a regular expression match the empty word?
    , matchesEmptyWord
    {- Returns the regular expression that matches all the words matched by the given
       regular expression beginning with the given character without their first character
       (the given one)
       
       Example:
       (derive 'a' $ repeatBetween 2 5 $ Symbol 'a') == (repeatBetween 1 4 $ Symbol 'a') 
    -} 
    , deriveCharacter
    -- Applying deriveCharacter for each character in a word
    , deriveWord
    -- Whether a regex matches a given word
    , matches
    ) where

import Prelude hiding (concat, repeat, null)


--    regex type    --


data Regex alphabet = NullSet            -- matches nothing
    | EmptyWord                                         -- matches the empty string
    | Symbol alphabet                                   -- matches a single symbol
    | AnySymbol                                         -- matches any symbol
    | (Regex alphabet) `Concat` (Regex alphabet)        -- matches any concatenation of words where the first is from the first set and the second from the second
    | Repeat (Regex alphabet)                           -- matches any number of repeated concatenations of words from the given set (even 0)
    | (Regex alphabet) `Union` (Regex alphabet)         -- matches a word in either of the given sets
    | (Regex alphabet) `Intersection` (Regex alphabet)  -- matches any word that is in both the first set and the second one
    | (Regex alphabet) `Difference` (Regex alphabet)    -- matches any word that is in the first set but not the second
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
EmptyWord `concat` regex = regex
regex `concat` EmptyWord = regex
-- concatenation with something optional (empty word always comes left in union)
l `concat` Union EmptyWord r = l `union` (l `concat` r)
Union EmptyWord l `concat` r = r `union` (l `concat` r)
-- Concatenation of NullSet yields nothing since there's nothing in NullSet
NullSet `concat` _ = NullSet
_ `concat` NullSet = NullSet
l `concat` r = Concat l r 

repeat :: Eq alphabet => Regex alphabet -> Regex alphabet
-- Repeating the empty word changes nothing
repeat EmptyWord = EmptyWord
-- Repeating the empty set 0 times yields an empty word, anything more nothing.
repeat NullSet = EmptyWord
repeat regex = Repeat regex

-- Checks whether the potential superset is a union of the subset and something else.
-- Returns false negatives! But it does keep anyOf "baa" from returning (b|a|a) instead of (b|a)
isDefinitelySubsetOf :: Eq alphabet => Regex alphabet -> Regex alphabet -> Bool
subset `isDefinitelySubsetOf` superset
    | subset == superset = True
    | otherwise = case superset of 
        (l `Union` r) -> subset `isDefinitelySubsetOf` l || subset `isDefinitelySubsetOf` r
        otherwise   -> False 

union :: Eq alphabet => Regex alphabet -> Regex alphabet -> Regex alphabet
-- empty set is the neutral element of union
NullSet `union` regex = regex
regex `union` NullSet = regex
l `union` r
    -- if either is a subset of the other, only use the superset
    | l `isDefinitelySubsetOf` r = r
    | r `isDefinitelySubsetOf` l = l
    -- if you can't be sure either's a subset (it may still be, just too hard to check), use union to be safe
    | otherwise              = l `union'` r
    where
        -- keep empty word outmost and leftmost since patternmatching against it is useful for simplifications
        l `union'` (EmptyWord `Union` r) = EmptyWord `Union` (l `union` r)
        (EmptyWord `Union` l) `union'` r = EmptyWord `Union` (l `union` r)
        l `union'` EmptyWord = EmptyWord `Union` l
        l `union'` r = l `Union` r

intersection :: Eq alphabet => Regex alphabet -> Regex alphabet -> Regex alphabet
-- Intersection with an empty set yields an empty set
NullSet `intersection` _ = NullSet
_ `intersection` NullSet = NullSet
-- Intersection with yourself changes nothing 
l `intersection` r
    {- CAUTION! This is no proper check for equivalence!
       Probably no big problem though? Just makes expressions more complex,
       but the cost of that is probably smaller than that of checking for
       actual equivalence...
    -} 
    | l == r    = l
    | otherwise = l `Intersection` r

difference :: Eq alphabet => Regex alphabet -> Regex alphabet -> Regex alphabet
-- You can't remove anything from an empty set
NullSet `difference` _     = NullSet
-- Removing nothing from a set changes nothing
regex `difference` NullSet = regex
-- Removing everything leaves nothing
l `difference` r
    | l `isDefinitelySubsetOf` r = NullSet -- Is not always sufficient, but catches some common cases.
    | otherwise                  = l `Difference` r 


--    convenience constructors    --


range :: (Enum alphabet, Eq alphabet) => alphabet -> alphabet -> Regex alphabet
range low high = foldl union NullSet $ map Symbol $ enumFromTo low high

repeatExactly :: Eq alphabet => Int -> Regex alphabet -> Regex alphabet
repeatExactly n regex
    | n < 0     = NullSet
    | n == 0    = EmptyWord
    | n == 1    = regex
    | otherwise = regex `concat` repeatExactly (n-1) regex

repeatAtLeast :: Eq alphabet => Int -> Regex alphabet -> Regex alphabet
repeatAtLeast n regex = repeatExactly n regex `concat` repeat regex 

repeatAtMost :: Eq alphabet => Int -> Regex alphabet -> Regex alphabet
repeatAtMost n regex
    | n < 0     = NullSet
    | n == 0    = EmptyWord
    -- TODO: Consider using (r|)(r|)(r|) instead of (rrr|rr|r|)
    -- (which one is faster?)
    | otherwise = repeatExactly n regex `union` repeatAtMost (n-1) regex

repeatBetween :: Eq alphabet => Int -> Int -> Regex alphabet -> Regex alphabet
repeatBetween low high regex
    | low > high = NullSet
    -- TODO: Consider using (r|)(r|)r instead of (rrr|rr|r)
    -- (which one is faster?)
    | otherwise  = repeatExactly low regex `concat` repeatAtMost (high - low) regex

fromWord :: Eq alphabet => [alphabet] -> Regex alphabet
fromWord word = foldl concat EmptyWord $ map Symbol word

anyOf :: Eq alphabet => [alphabet] -> Regex alphabet
anyOf chars = foldl union NullSet $ map Symbol chars

anyBut :: Eq alphabet => [alphabet] -> Regex alphabet
anyBut chars = AnySymbol `difference` anyOf chars


--    Regex matching    --

matchesEmptyWord :: Regex alphabet -> Bool
matchesEmptyWord EmptyWord = True
-- Repeating 0 times yields the empty word
matchesEmptyWord (Repeat _) = True
-- I could use a catch-all pattern for these, but I'd rather get errors when adding new patterns and not changing these
matchesEmptyWord NullSet = False
matchesEmptyWord (Symbol _) = False
matchesEmptyWord AnySymbol = False
-- Concatenating two empty words yields the empty word
matchesEmptyWord (l `Concat` r) = matchesEmptyWord l && matchesEmptyWord r
matchesEmptyWord (l `Union` r) = matchesEmptyWord l || matchesEmptyWord r
matchesEmptyWord (l `Intersection` r) = matchesEmptyWord l && matchesEmptyWord r
matchesEmptyWord (l `Difference` r) = matchesEmptyWord l && not (matchesEmptyWord r)

-- Like matchesEmptyWord, but either returns an empty set (no match) or the set with the empty word (match)
matchesEmptyWord1 :: Eq alphabet => Regex alphabet -> Regex alphabet
matchesEmptyWord1 regex = if matchesEmptyWord regex then EmptyWord else NullSet

deriveCharacter :: Eq alphabet => alphabet -> Regex alphabet -> Regex alphabet
deriveCharacter _ NullSet              = NullSet
deriveCharacter _ EmptyWord            = NullSet
deriveCharacter c (Symbol s)
    | c == s                           = EmptyWord
    | otherwise                        = NullSet
deriveCharacter _ AnySymbol            = EmptyWord
deriveCharacter c (Repeat r)           = deriveCharacter c r `concat` Repeat r
-- either remove c from the left side, or use the empty word left and remove c from the right side
deriveCharacter c (l `Concat` r)       = (deriveCharacter c l `concat` r) `union` (matchesEmptyWord1 l `concat` deriveCharacter c r)
deriveCharacter c (l `Union` r)        = deriveCharacter c l `union`        deriveCharacter c r
deriveCharacter c (l `Intersection` r) = deriveCharacter c l `intersection` deriveCharacter c r
deriveCharacter c (l `Difference` r)   = deriveCharacter c l `difference`   deriveCharacter c r

deriveWord :: Eq alphabet => [alphabet] -> Regex alphabet -> Regex alphabet
deriveWord = flip $ foldl (flip deriveCharacter)

matches :: Eq alphabet => Regex alphabet -> [alphabet] -> Bool
matches regex word = matchesEmptyWord $ deriveWord word regex

definitelyNull :: Eq alphabet => Regex alphabet -> Bool
definitelyNull NullSet = True
definitelyNull EmptyWord = False
definitelyNull (Repeat _) = False
definitelyNull (Symbol _) = False
definitelyNull AnySymbol = False
definitelyNull (l `Concat` r) = definitelyNull l || definitelyNull r
definitelyNull (l `Union` r) = definitelyNull l && definitelyNull r
definitelyNull (l `Intersection` r) = definitelyNull l || definitelyNull r
definitelyNull (l `Difference` r) = definitelyNull l || l `isDefinitelySubsetOf` r -- CAUTION! Does not catch all cases!
