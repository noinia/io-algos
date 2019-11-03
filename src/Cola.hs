module Cola where

import Prelude hiding (lookup)
import           BinarySearch
import           Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import           Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as UV
import qualified Data.List as List
import qualified Merge

newtype Cola v a = Cola [v a] deriving (Show,Eq)

empty = Cola []

insert             :: (Ord a, Vector v a) => a -> Cola v a -> Cola v a
insert x (Cola vs) = Cola $ insertVs 1 (GV.singleton x) vs

insertVs _  l []         = [l]
insertVs nl l rs@(r:rs') = case nl `compare` GV.length r of
                             LT -> l:rs
                             EQ -> let n = 2*nl
                                   in insertVs n (merge n l r) rs'
                             GT -> r:insertVs (GV.length r) l rs'
                               -- only useful when inserting a full vec at once

merge n l r = GV.fromListN n (GV.toList l `Merge.merge` GV.toList r)


binSearchVal p v = (v GV.! ) <$> binarySearchVec p v


minimum' :: Ord a => [a] -> Maybe a
minimum' = \case
  [] -> Nothing
  xs -> Just $ minimum xs

maximum' :: Ord a => [a] -> Maybe a
maximum' = \case
  [] -> Nothing
  xs -> Just $ maximum xs


-- O(log^2 n)
lookupGEs             :: (Vector v a, Ord a) => a -> Cola v a -> Maybe a
lookupGEs x (Cola vs) = minimum' $ mapMaybe (binSearchVal (>= x)) vs

-- O(log^2 n)
lookup             ::  (Vector v a, Ord a) => a -> Cola v a -> [a]
lookup x (Cola vs) = filter (== x) $ mapMaybe (binSearchVal (>= x)) vs

-- O(n log^2 n)
fromList' :: (Vector v a, Ord a) => [a] -> Cola v a
fromList' = foldr insert empty

fromList'' :: Ord a => [a] -> Cola V.Vector a
fromList'' = fromList'
