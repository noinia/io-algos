{-# Language DeriveFunctor#-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.BinaryTree.LeafTree.Complete
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Complete Binary trees of size 2^h with its elements in the leaves
--
--------------------------------------------------------------------------------
module Data.BinaryTree.LeafTree.Complete
  ( fullTree
  , height

  , fromAscList
  , fromAscList2

  , size
  , split

  , pow, lg
  , div2
  ) where

import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.BinaryTree.LeafTree.Core hiding (height)
import           Data.Bits
import qualified Data.Foldable as F
import qualified Data.List as List
import           Data.Maybe
import qualified Data.Vector as V
import           Debug.Trace
import           GHC.Stack

--------------------------------------------------------------------------------

-- | computes the height of the complete tree
height :: Tree k v -> Height
height = go
  where
    go = \case
      Leaf _     -> 0
      Node l _ _ -> 1 + go l
         -- since the tree is a complete tree we can always just go left

fullTree                 :: Num i => i -> Height -> Tree i i
fullTree s h | h == 0    = Leaf s
             | otherwise = let h' = h - 1
                           in Node (fullTree s h')
                                   (s + pow h')
                                   (fullTree (s + pow h') h')

-- | Builds a tree from the list of key,value pairs.
--
-- pre: the keys are given in non-decreasing order.
fromAscList     :: Foldable f => f (k,v) -> Tree (Maybe k) (Maybe (k,v))
fromAscList xs' = let xs = F.toList xs'
                      n  = length xs'
                      h  = lg n
                      m  = pow h
                      m' | m == n    = m
                         | otherwise = 2*m
                  in first (fmap fst) . fromAscList2' $ replicate (m' - n) Nothing <> map Just xs

-- |
--
-- pre: - input is has lenght a power of 2
--      - keys are given in non-decreasing order.
fromAscList2 :: Foldable f => f (k,v) -> Tree k (k,v)
fromAscList2 = first fst . fromAscList2' . F.toList

fromAscList2' :: [k] -> Tree k k
fromAscList2' = fst . head . pairup [] . map (\x -> (Leaf x,x))
  where
    pairup _acc [x]                = [x]
    pairup acc  []                 = pairup [] (reverse acc)
    pairup acc  ((x,k):(y,m):rest) = pairup ((Node x k y,m) : acc) rest


-- | Given the height computes the size of the tree.
size   :: (Integral h, Num i) => h -> i
size h = pow (h+1) - 1

--------------------------------------------------------------------------------

split                            :: Height -> Tree k v -> Tree k (Tree k v, k, Tree k v)
split h (Node l k r) | h == 0    = Leaf (l,k,r)
                     | otherwise = let h' = h - 1 in Node (split h' l) k (split h' r)

--------------------------------------------------------------------------------
-- * Generic Helper functions

pow h = 2 ^ h

lg 1 = 0
lg n = 1 + lg (div2 n)

div2 h = h `shiftR` 1 -- h `div` 2
