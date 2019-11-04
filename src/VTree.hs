module VTree where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import           Data.Vector.Generic (Vector, (!))

import Debug.Trace

lg :: Int -> Int
lg = ceiling . logBase 2 . fromIntegral

-- | size of a tree with n leaves
size n = 2*n - 1

-- | Height of a tree with n leaves
height n = 1 + lg n

-- | number of leaves in a full tree of height h
leaves 0 = 0
leaves h = 2 ^ (h - 1)

-- | Size of a full tree of height h
sizeH = size . leaves

-- | Leaves on on the max depth
remLeaves n = let h = height n
                  l = leaves $ h - 1
              in (n - l) * 2


-- Size = total number of nodes in the tree
-- #leaves = number of leaves.

-- | Let n be the number of leaves in a tree. Computes the indices of
-- the roots of the bottom subtrees in a VEB layout, the number of
-- leaves
vebSplitIndices :: Int -> [(Int,Int)]
vebSplitIndices n | n <= 1    = []
                  | otherwise = fulls <> partial <> nonFulls
  where
    -- height of the tree
    h = height n
    -- number of items on the bottom row
    r = remLeaves n

    -- height of the top tree
    topH = (h + 1) `div` 2 -- = ceiling (h/2)
    -- number of leaves in the top tree:
    topN = leaves topH
    -- size of the top tree
    topS = sizeH topH

    -- height of the full bottom trees
    bottomH = h - topH
    -- number of leaves in a full bottom tree
    bottomN = leaves bottomH
    -- size of a full bottomTree
    bottomS = sizeH bottomH

    -- number of full bottom trees, and the number of remaining elems
    -- on the last row of the at most one tree with height bottomH
    -- that is not full.
    (k,bottomR) = r `divMod` bottomN

    -- number of partial trees of height bottomH
    p = if bottomR == 0 then 0 else 1

    -- number of trees of height bottomH'
    l = 2*topN - k - p

    -- remaining bottom tree heights
    bottomH' = bottomH - 1
    -- number of items
    bottomN' = leaves bottomH'
    -- size of the these trees
    bottomS' = sizeH bottomH'

    -- number of leaves in the partial Tree
    bottomRN = p * (bottomR + bottomN')
    -- size of the at most one partial tree
    bottomRS = p * (size bottomRN)

    -- indices and number of leaves of the full bottom trees:
    fulls    = [ (1 + topS + i*bottomS, bottomN)   | i <- [0..(k-1)] ]
    -- -- index of our parital tree (if it exists)
    partial  = [ (1 + topS + k*bottomS, bottomRN)  | i <- [0..(p-1)] ]
    -- and the non-full ones
    nonFulls = [ (1 + topS + k*bottomS + bottomRS + i*bottomS', bottomN')
               | i <- [0..(l-1)], bottomN' > 0 ]


newtype GVTree v a = VTree (v a) deriving (Show,Eq)

data GTree v a = Tree !Int !(GVTree v a) deriving (Show,Eq)

type Tree v a = Maybe (GTree v a)

pattern Leaf :: Tree v a
pattern Leaf = Nothing

pattern Node       :: Vector v a => Tree v a -> a -> Tree v a -> Tree v a
pattern Node l x r <- ((>>= node) -> Just (l,x,r))

node                      :: Vector v a => GTree v a -> Maybe (Tree v a, a, Tree v a)
node (Tree i t@(VTree v)) = (\x -> (l, x,r)) <$> (v GV.!? i)
  where
    l   = f (i+1)
    r   = f (i+1+sizeOf l)
    f j = (\_ -> Tree j t) <$> v GV.!? j

sizeOf _ = 1


test :: Tree V.Vector Int
test = Just . Tree 0 . VTree . V.fromList $ [1..3]
-- FIXME: this does not look right yet


show' :: (Vector v a, Show a) => Tree v a -> String
show' Leaf = "Leaf"
show' (Node l x r) = "Node (" <> show' l <> ") " <> show x <> " (" <> show' r <> ")"


-- data GVTree v i l = VTree { start    :: Int
--                           , size     :: Int
--                           , treeData :: v a
--                           } deriving (Show,Eq,Ord)

-- data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show,Eq,Functor,Foldable,Traversable)

-- toVec :: Tree a -> V.Vector a
-- toVec = go
--   where
--     go Leaf         = V.empty
--     go (Node l x r) = go l <> V.singleton x <> go r


-- pattern Leaf = Nothing



-- pattern Node l x r <- Just (l,x,r)
--   where
--     Node l x r = Just (l,x,r)

-- empty = VTree 0 GV.empty




-- vebSplit' :: Vector v a => GVTree v a -> (GVTree v a, V.Vector (GVTree v a))
-- vebSplit'




-- root                       :: Vector v a => GVTree v a -> Maybe (GVTree v a, a, GVTree v a)
-- root (VTree v) = case GV.length v of
--     0 -> Nothing
--     1 -> Just (empty, r, empty)
--     2 -> Just (VTree $ GV.slice 1 1 v, r, empty)
--     n -> Just (VTree $ GV.slice 1 1 v, r, GV.slice 2 1 v)
--     n -> Just
--   where
--     r = GV.head v
