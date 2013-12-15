module Helpers where

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither onNothing (Nothing) = Left onNothing
maybeToEither _ (Just something) = Right something
