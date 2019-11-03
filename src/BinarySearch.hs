module BinarySearch where

import Control.Monad.Primitive(PrimMonad, PrimState)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
--------------------------------------------------------------------------------

-- -- | Given a monotonic predicate, Get the index h such that everything strictly
-- -- smaller than h has: p i = False, and all i >= h, we have p h = True
-- --
-- -- returns Nothing if no element satisfies p
-- --
-- -- running time: \(O(\log^2 n + T*\log n)\), where \(T\) is the time to execute the
-- -- predicate.
-- binarySearchSeq     :: (a -> Bool) -> Seq a -> Maybe Int
-- binarySearchSeq p s = case S.viewr s of
--                        EmptyR                 -> Nothing
--                        (_ :> x)   | p x       -> Just $ case S.viewl s of
--                          (y :< _) | p y          -> 0
--                          _                       -> binarySearch p' 0 u
--                                   | otherwise -> Nothing
--   where
--     p' = p . S.index s
--     u  = S.length s - 1



-- | Given a monotonic predicate, get the index h such that everything strictly
-- smaller than h has: p i = False, and all i >= h, we have p h = True
--
-- returns Nothing if no element satisfies p
--
-- running time: \(O(T*\log n)\), where \(T\) is the time to execute the
-- predicate.
binarySearchMVec                    :: (PrimMonad m, MV.MVector v a)
                                    => (a -> m Bool) -> v (PrimState m) a -> m (Maybe Int)
binarySearchMVec p' v | MV.null v   = pure Nothing
                      | otherwise   = p n' >>= \case
                          False -> pure Nothing
                          True  -> p 0 >>= \case
                            True  -> pure $ Just 0
                            False -> Just <$> binarySearchM p 0 n'
  where
    n' = MV.length v - 1
    p i = MV.read v i >>= p'


-- | Given a monotonic predicate, get the index h such that everything strictly
-- smaller than h has: p i = False, and all i >= h, we have p h = True
--
-- returns Nothing if no element satisfies p
--
-- running time: \(O(T*\log n)\), where \(T\) is the time to execute the
-- predicate.
binarySearchVec                             :: V.Vector v a
                                            => (a -> Bool) -> v a -> Maybe Int
binarySearchVec p' v | V.null v   = Nothing
                     | not $ p n' = Nothing
                     | otherwise  = Just $ if p 0 then 0
                                                  else binarySearch p 0 n'
  where
    n' = V.length v - 1
    p = p' . (v V.!)




-- -- | Partition the seq s given a monotone predicate p into (xs,ys) such that
-- --
-- -- all elements in xs do *not* satisfy the predicate p
-- -- all elements in ys do       satisfy the predicate p
-- --
-- -- all elements in s occur in either xs or ys.
-- --
-- -- running time: \(O(\log^2 n + T*\log n)\), where \(T\) is the time to execute the
-- -- predicate.
-- splitMonotone     :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
-- splitMonotone p s = case binarySearchSeq p s of
--                       Nothing -> (s,S.empty)
--                       Just i  -> S.splitAt i s


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
binarySearch       :: Integral a => (a -> Bool) -> a -> a -> a
binarySearch p l u = let d = u - l
                         m = l + (d `div` 2)
                     in if d == 1 then u else
                          if p m then binarySearch p l m
                                 else binarySearch p m u

{-# SPECIALIZE binarySearchM :: PrimMonad m => (Int -> m Bool) -> Int -> Int -> m Int #-}
{-# SPECIALIZE binarySearchM :: PrimMonad m => (Word -> m Bool) -> Word -> Word -> m Word #-}
binarySearchM       :: (PrimMonad m, Integral a) => (a -> m Bool) -> a -> a -> m a
binarySearchM p l u = let d = u - l
                          m = l + (d `div` 2)
                      in if d == 1 then pure u else
                           p m >>= \case
                             True  -> binarySearchM p l m
                             False -> binarySearchM p m u
