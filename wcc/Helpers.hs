module Helpers where

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither onNothing (Nothing) = Left onNothing
maybeToEither _ (Just something) = Right something

findFixedPoint :: (Eq a) => (a -> a) -> a -> a
findFixedPoint f x = let x' = f x in if x == x' then x else findFixedPoint f x'
