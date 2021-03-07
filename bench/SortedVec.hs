module SortedVec
  ( SortedVec
  , fromAscList
  , lookupGE
  ) where

import qualified Data.Vector.Unboxed as UV

type SortedVec k v = UV.Vector (k,v)

fromAscList :: (UV.Unbox k, UV.Unbox v) => [(k,v)] -> SortedVec k v
fromAscList = UV.fromList

-- lookupGT q = binarySearchIn (> q)

lookupGE   :: (UV.Unbox k, UV.Unbox v, Ord k) => k -> SortedVec k v -> Maybe (k,v)
lookupGE q = binarySearchIn (\(k,_) -> k >= q)

--------------------------------------------------------------------------------

-- | Given a monotonic predicate p, a lower bound l, and an upper bound u, with:
--  p l = False
--  p u = True
--  l < u.
--
-- Get the index h such that everything strictly smaller than h has: p i =
-- False, and all i >= h, we have p h = True
--
-- running time: \(O(\log(u - l))\)
{-# SPECIALIZE binarySearch :: (Int -> Bool) -> Int -> Int -> Int #-}
{-# SPECIALIZE binarySearch :: (Word -> Bool) -> Word -> Word -> Word #-}
binarySearch   :: Integral a => (a -> Bool) -> a -> a -> a
binarySearch p = go
  where
    go l u = let d = u - l
                 m = l + (d `div` 2)
             in if d == 1 then u else if p m then go l m
                                             else go m u

binarySearchIdxIn                   :: (UV.Unbox k, UV.Unbox v)
                                    => ((k,v) -> Bool) -> SortedVec k v -> Maybe Int
binarySearchIdxIn p' v | UV.null v  = Nothing
                       | not $ p n' = Nothing
                       | otherwise  = Just $ if p 0 then 0 else binarySearch p 0 n'
    where
      n' = UV.length v - 1
      p = p' . (v UV.!)

binarySearchIn     :: (UV.Unbox k, UV.Unbox v)
                   => ((k,v) -> Bool) -> SortedVec k v -> Maybe (k,v)
binarySearchIn p v = (v UV.!) <$>  binarySearchIdxIn p v
