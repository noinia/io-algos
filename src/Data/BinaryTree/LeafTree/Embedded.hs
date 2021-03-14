module Data.BinaryTree.LeafTree.Embedded
  ( Tree
  , Node(..)
  , Index
  , fromLayout

  , searchLeafR
  , lookup, lookupGT, lookupGE
  )  where

import           Control.DeepSeq
import           Control.Exception(assert)
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.BinaryTree.LeafTree.Complete (split, lg, pow, div2, size, fullTree)
import qualified Data.BinaryTree.LeafTree.Complete as Complete
import qualified Data.BinaryTree.LeafTree.Core as Core
import           Data.BinaryTree.LeafTree.Core ( Height
                                               , foldTree, traversePrefix
                                               , labelLeaves
                                               )
import qualified Data.BinaryTree.LeafTree.Core as Tree
import qualified Data.Foldable as F
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Semigroup
import           Data.Semigroup.Foldable
import qualified Data.Vector as V
import           Debug.Trace
import           GHC.Generics
import           Prelude hiding (lookup)

--------------------------------------------------------------------------------

type Index  = Word


type Tree k v = V.Vector (Node k v)

data Node k v = Leaf !v
              | Node {-# UNPACK #-} !Index !k {-# UNPACK #-} !Index
              deriving (Show,Eq,Ord,Generic,Generic1)

instance (NFData k, NFData v) => NFData (Node k v) where rnf = rnf1
instance NFData k => NFData1 (Node k)

--------------------------------------------------------------------------------
-- * Reconstructing a Tree

-- pre: input is nonEmpty ; i.e. the vector has at least one element.
fromLayout :: Tree k v -> Core.Tree k v
fromLayout = fromLayout' 0

-- All of this should work for any tree embedded in an array.

fromLayout'      :: Index -> Tree k v -> Core.Tree k v
fromLayout' s xs = go s
  where
    go i = case xs V.! fromIntegral i of
             Leaf v       -> Core.Leaf v
             Node li k ri -> Core.Node (go li) k (go ri)


--------------------------------------------------------------------------------
-- * Querying

-- | Binary-search on a tree. the predicate indicates if we should go right
searchLeafR           :: (k -> Bool) -> Tree k v -> v
searchLeafR goRight t = go 0
  where
    go i = case t V.! fromIntegral i of
             Leaf v       -> v
             Node li k ri -> if goRight k then go ri else go li


-- | Lookup an the value of key k
lookup   :: Ord k => k -> Tree (Maybe k) (Maybe (k,v)) -> Maybe v
lookup q = fmap snd . lookup' (== Just q)

-- | Find the strict successor of k, if it exists
lookupGT   :: Ord k => k -> Tree (Maybe k) (Maybe (k,v)) -> Maybe (k,v)
lookupGT q = lookup' (> Just q)

-- | Find k or the the successor of k, if it exists
lookupGE   :: Ord k => k -> Tree (Maybe k) (Maybe (k,v)) -> Maybe (k,v)
lookupGE q = lookup' (>= Just q)



-- lookupLT   :: Ord k => k -> Tree (Maybe k) (Maybe (k,v)) -> Maybe (k,v)
-- lookupLT q = lookup' (< Just q)

-- lookupLE   :: Ord k => k -> Tree (Maybe k) (Maybe (k,v)) -> Maybe (k,v)
-- lookupLE q = lookup' (<= Just q)




-- | Helper function to implement the lookups
lookup'     :: (Maybe k -> Bool) -> Tree (Maybe k) (Maybe (k,v)) -> Maybe (k,v)
lookup' p t = let lf = searchLeafR p t
              in if p (fst <$> lf) then lf else Nothing
