module SyntaxTree
    ( SyntaxTree ( Node, Leaf )
    , NodeSyntaxTree ( NodeSyntaxTree )
    , fromNodeSyntaxTree
    )where

import Data.Foldable (Foldable, foldMap)
import Data.Monoid (mconcat, mempty, mappend)
import Data.Functor (Functor, fmap)

data SyntaxTree nodes leafs
    = Node nodes [SyntaxTree nodes leafs]
    | Leaf leafs
    deriving (Eq, Show)

--    Definitions for folding and mapping (i.e. Functor and Foldable instances)    --

-- Mapping over leafs
instance Functor (SyntaxTree nodes) where
    fmap f (Leaf l) = Leaf $ f l
    fmap f (Node node forest) = Node node $ map (fmap f) forest

-- Folding leafs
instance Foldable (SyntaxTree nodes) where
    foldMap f (Leaf l) = f l
    foldMap f (Node _ forest) = mconcat $ map (foldMap f) forest


-- SyntaxTree with flipped type parameters

newtype NodeSyntaxTree leafs nodes = NodeSyntaxTree {fromNodeSyntaxTree :: SyntaxTree nodes leafs} deriving (Eq, Show)

-- Mapping over nodes
instance Functor (NodeSyntaxTree leafs) where
    fmap f (NodeSyntaxTree (Leaf l)) = NodeSyntaxTree $ Leaf l
    fmap f (NodeSyntaxTree (Node node forest))
        = NodeSyntaxTree $ Node (f node) $ map (fromNodeSyntaxTree . fmap f . NodeSyntaxTree) forest

-- Folding nodes
instance Foldable (NodeSyntaxTree leafs) where
    foldMap f (NodeSyntaxTree (Leaf _)) = mempty
    foldMap f (NodeSyntaxTree (Node node forest))
        = f node `mappend` mconcat ( map (foldMap f . NodeSyntaxTree) forest)