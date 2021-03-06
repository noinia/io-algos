{-# Language DeriveFunctor#-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.BinaryTree.LeafTree
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Binary trees with its elements in the leaves
--
--------------------------------------------------------------------------------
module Data.BinaryTree.LeafTree
  ( Tree(..)
  , rootValue

  , foldTree
  , traversePrefix

  , labelLeaves
  , height
  , levels

  , binarySearch

  , fromAscList
  , fromAscList2
  , split
  , size
  ) where

import Data.BinaryTree.LeafTree.Core
import Data.BinaryTree.LeafTree.Complete

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

myTree = Node (Leaf "a") "u" (Node (Leaf "l") "v" (Leaf "r"))

myFullTree = Node (Node (Leaf "a") "x" (Leaf "b"))
                  "u"
                  (Node (Leaf "l") "v" (Leaf "r"))

myTree1 = Node (Leaf "l") "v" (Leaf "r")


-- testT :: Tree Int Int
testT = fromAscList [0,1,2,3,4,5,6,7]

testQ = binarySearch (> Just 5) testT -- successor query
