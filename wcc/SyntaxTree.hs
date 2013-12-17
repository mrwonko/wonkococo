module SyntaxTree
    ( SyntaxTree (..)
    , NodeSyntaxTree (..)
    , drawTree
    )where

import Data.Foldable (Foldable, foldMap)
import Data.Monoid (mconcat, mempty, mappend)
import Data.Functor (Functor, fmap)
import Token
import qualified Data.Tree as Tree

data SyntaxTree alphabet productionNames tokenNames
    = Node productionNames [SyntaxTree alphabet productionNames tokenNames]
    | Leaf (Token tokenNames alphabet)
    deriving (Eq, Show)

toStringTree :: (Show alphabet, Show productionNames, Show tokenNames)
    => SyntaxTree alphabet productionNames tokenNames
    -> Tree.Tree String
toStringTree = Tree.unfoldTree unfold
    where
        unfold (Leaf token) = (show token, [])
        unfold (Node productionName subtree) = (show productionName, subtree)

drawTree :: (Show alphabet, Show productionNames, Show tokenNames)
    => SyntaxTree alphabet productionNames tokenNames
    -> String
drawTree = Tree.drawTree . toStringTree

--    Definitions for folding and mapping (i.e. Functor and Foldable instances)    --

-- Mapping over leafs
instance Functor (SyntaxTree alphabet productionNames) where
    fmap f (Leaf token@Token{}) = Leaf $ token { tName = f $ tName token }
    fmap f (Node productionName forest) = Node productionName $ map (fmap f) forest

-- Folding leafs
instance Foldable (SyntaxTree alphabet productionNames) where
    foldMap f (Leaf token@Token{}) = f $ tName token
    foldMap f (Node _ forest) = mconcat $ map (foldMap f) forest


-- SyntaxTree with flipped type parameters

newtype NodeSyntaxTree alphabet tokenNames productionNames
    = NodeSyntaxTree {fromNodeSyntaxTree :: SyntaxTree alphabet productionNames tokenNames } deriving (Eq, Show)

-- Mapping over nodes
instance Functor (NodeSyntaxTree alphabet tokenNames) where
    fmap f (NodeSyntaxTree (Leaf l)) = NodeSyntaxTree $ Leaf l
    fmap f (NodeSyntaxTree (Node productionName forest))
        = NodeSyntaxTree $ Node (f productionName) $ map (fromNodeSyntaxTree . fmap f . NodeSyntaxTree) forest

-- Folding nodes
instance Foldable (NodeSyntaxTree alphabet tokenNames) where
    foldMap f (NodeSyntaxTree (Leaf _)) = mempty
    foldMap f (NodeSyntaxTree (Node productionName forest))
        = f productionName `mappend` mconcat ( map (foldMap f . NodeSyntaxTree) forest)