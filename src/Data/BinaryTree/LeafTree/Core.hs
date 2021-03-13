{-# Language DeriveFunctor#-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.BinaryTree.LeafTree.Core
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Binary trees with its elements in the leaves
--
--------------------------------------------------------------------------------
module Data.BinaryTree.LeafTree.Core
  ( Tree(..)
  , Height
  , rootValue

  , foldTree
  , traversePrefix

  , labelLeaves
  , height
  , levels

  , binarySearch
  , binarySearchLeaf

  , showByLevels, printByLevels
  ) where

import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Bits
import qualified Data.List as List
import           Data.Maybe
import qualified Data.Vector as V
import           Debug.Trace
import           GHC.Stack

--------------------------------------------------------------------------------

type Height = Int

-- | Binary tree with elements of type 'v' in the leaves and type 'k'
-- in internal nodes.
data Tree k v = Leaf !v
              | Node (Tree k v) !k (Tree k v)
                deriving (Show,Eq,Ord)

instance Bifunctor Tree where
  bimap f g = \case
    Leaf v     -> Leaf (g v)
    Node l k r -> Node (bimap f g l) (f k) (bimap f g r)


rootValue :: Tree v v -> v
rootValue = \case
  Leaf x     -> x
  Node _ x _ -> x

foldTree           :: (v -> b) -> (b -> k -> b -> b) -> Tree k v -> b
foldTree leaf node = go
  where
    go = \case
      Leaf v     -> leaf v
      Node l k r -> node (go l) k (go r)


traversePrefix           :: Applicative f
                         => (v -> f v') -> (k -> f k') -> Tree k v -> f (Tree k' v')
traversePrefix leaf node = go
  where
    go = \case
      Leaf v     -> Leaf <$> leaf v
      Node l k r -> (\k' l' r' -> Node l' k' r') <$> node k <*> go l <*> go r


labelLeaves  :: Num i => Tree k v -> Tree k (i,v)
labelLeaves = flip evalState 0 . traversePrefix (\k -> do i <- get
                                                          modify (+1)
                                                          pure (i,k)
                                                ) pure

leaves :: Tree k v -> [v]
leaves = foldTree (:[]) (\l _ r -> l <> r)

height :: Tree k v -> Height
height = foldTree (const 0) (\l _ r -> 1 + max l r)

-- | Essentially a BFS traversal of the tree
levels   :: Tree k v -> [[Tree k v]]
levels = init . levels'
  where
    levels' t = [t] : case t of
      Leaf _     -> [[]]
      Node l _ r -> zipWith (<>) (levels' l) (levels' r)

--------------------------------------------------------------------------------


-- | Version of binary search that also actually checks the leaf that
-- we obtain
binarySearch     :: (k -> Bool) -> Tree k k -> Maybe k
binarySearch p t = let v = binarySearchLeaf p t
                   in if p v then Just v else Nothing

-- | If we satisfy the predicate, go left, otherwise go right.
binarySearchLeaf   :: (k -> Bool) -> Tree k v -> v
binarySearchLeaf p = go
  where
    go = \case
      Leaf v                 -> v
      Node l k r | p k       -> go l
                 | otherwise -> go r


--------------------------------------------------------------------------------

showByLevels :: Show k => Tree k k -> String
showByLevels = unlines
             . map (show . map rootValue)
             . levels

printByLevels :: Show k => Tree k k -> IO ()
printByLevels = putStr . showByLevels
