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


  , fromAscList
  , fromAscList2

  , size
  , split

  , pow, lg
  , div2
  ) where

import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.BinaryTree.LeafTree.Core
import           Data.Bits
import qualified Data.List as List
import           Data.Maybe
import qualified Data.Vector as V
import           Debug.Trace
import           GHC.Stack

--------------------------------------------------------------------------------

fullTree                 :: Num i => i -> Height -> Tree i i
fullTree s h | h == 0    = Leaf s
             | otherwise = let h' = h - 1
                           in Node (fullTree s h')
                                   (s + (pow h'))
                                   (fullTree (s + pow h') h')

fromAscList    :: [v] -> Tree (Maybe v) (Maybe v)
fromAscList xs = let n  = length xs
                     h  = lg n
                     m  = pow h
                     m' | m == n    = m
                        | otherwise = 2*m
                 in fromAscList2 $ replicate (m' - n) Nothing <> map Just xs

-- | pre: input is has lenght a power of 2
fromAscList2 :: [v] -> Tree v v
fromAscList2 = fst . head . pairup [] . map (\x -> (Leaf x,x))
  where
    pairup _acc [x]                = [x]
    pairup acc  []                 = pairup [] (reverse acc)
    pairup acc  ((x,k):(y,m):rest) = pairup ((Node x k y,m) : acc) rest



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
