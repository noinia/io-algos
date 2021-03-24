{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.BinaryTree.LeafTree.Embedded
  ( Tree
  , Node(..)
  , Index
  , fromLayout

  , searchLeafR
  , lookup, lookupGT, lookupGE
  )  where

import Foreign.C

import           Control.DeepSeq
import           Control.Exception (assert)
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.BinaryTree.LeafTree.Complete (split, lg, pow, div2, size, fullTree)
import qualified Data.BinaryTree.LeafTree.Complete as Complete
import           Data.BinaryTree.LeafTree.Core ( Height
                                               , foldTree, traversePrefix
                                               , labelLeaves
                                               )
import qualified Data.BinaryTree.LeafTree.Core as Core
import qualified Data.BinaryTree.LeafTree.Core as Tree
import qualified Data.Foldable as F
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Semigroup
import           Data.Semigroup.Foldable
-- import           Data.Store
-- import           Data.Store.TH
import qualified Data.Vector as V
import           Data.Word (Word8)
import           Debug.Trace
-- import           Flat
-- import qualified Flat.Class as Flat
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics
import           Prelude hiding (lookup)
-- import           TH.Derive

import qualified Data.Vector.Storable as SV

--------------------------------------------------------------------------------

type Index  = Word


type Tree k v = V.Vector (Node k v)

data Node k v = Leaf !v
              | Node {-# UNPACK #-} !Index !k {-# UNPACK #-} !Index
              deriving (Show,Eq,Ord,Generic,Generic1)

-- $($(derive [d|
--     instance (Store k, Store v) => Deriving (Store (Node k v))
--     |]))

-- instance (Flat k, Flat v) => Flat (Node k v)

instance (NFData k, NFData v) => NFData (Node k v) where rnf = rnf1
instance NFData k => NFData1 (Node k)

-- instance (Storable k, Storable v) => Storable (Node k v) where
--     sizeOf x = alignment @CInt undefined
--              + max (sizeOf @v undefined)
--                    (2* sizeOf @Index undefined + sizeOf @k undefined)
--       -- don't we need a bit to distinguish between the two types?

--     alignment _ = lcm (alignment @v undefined)
--                       (alignment @k undefined)
--       -- still not sure what I should do here.

--     peek ptr = do
--         tag <- peek (castPtr @_ @CInt ptr)
--         let ptr' = ptr `plusPtr` alignment @(Node k v) undefined
--         case tag of
--             0 -> Leaf <$> peek (castPtr @_ @v ptr')
--             1 -> let ptr1 = ptr  `plusPtr`  alignment @(Node k v) undefined
--                      ptr2 = ptr1 `plusPtr` alignment @Index undefined
--                      ptr3 = ptr2 `plusPtr` alignment @k undefined
--                  in Node <$> peek (castPtr @_ @Index ptr1)
--                          <*> peek (castPtr @_ @k     ptr2)
--                          <*> peek (castPtr @_ @Index ptr3)

--     poke ptr (Leaf x) = do
--         poke (castPtr @_ @Word8 ptr) 0
--         let ptr' = ptr `plusPtr` alignment @(Node k v) undefined
--         poke (castPtr ptr' :: Ptr a) x
--     -- poke ptr (Node l k r) = do
--     --     poke (castPtr ptr :: Ptr Word8) 1
--     --     let ptr' = ptr `plusPtr` alignment @(Node k v) undefined
--     --     poke (castPtr ptr' :: Ptr b) x


myLeaf :: Node Int Char
myLeaf = Leaf 'a'

myNode :: Node Int Char
myNode = Node 4 10 5



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
