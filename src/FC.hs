{-# LANGUAGE UndecidableInstances #-}
module FC where

import qualified Merge
import Prelude hiding (lookup)
import           BinarySearch
import           Data.Maybe (mapMaybe, isJust)
import qualified Data.Vector as V
import           Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as UV
import qualified Data.List as List


data ItemKind = DataVal | Bridge {-# UNPACK #-} !Int deriving (Show,Read,Eq)

data MaybeIdx = Nothing' | Just' {-# UNPACK #-} !Int deriving (Show,Read,Eq)


data CatalogVal a = CatalogVal { _itemKind    :: {-# UNPACK #-} !ItemKind
                               , _catalogVal  :: !a
                               , _prevDataIdx :: {-# UNPACK #-} !MaybeIdx
                               , _nextDataIdx :: {-# UNPACK #-} !MaybeIdx
                               } deriving (Show,Eq)

asBridge    :: CatalogVal a -> MaybeIdx
asBridge cv = case _itemKind cv of
                Bridge i  -> Just' i
                DataVal   -> Nothing'

dataVal    :: CatalogVal a -> Maybe a
dataVal cv = case _itemKind cv of
               Bridge _ -> Nothing
               DataVal  -> Just $ _catalogVal cv

newtype Catalog v a = Catalog (v (CatalogVal a))

instance (Show (v (CatalogVal a)), Vector v a) => Show (Catalog v a) where
  show (Catalog v) = show v


-- dataVals             :: (Vector v (CatalogVal a), Vector v (WithIdx a))
--                      => Catalog v a -> v (WithIdx a)
-- dataVals (Catalog v) = GV.imapMaybe (\i cv -> WI i <$> dataVal cv) v


-- | Returns all values, with the index in the current catalog
-- (forgetting) wether or not the element was as data val or an index iteslf.
iVals             :: (Vector v (CatalogVal a), Vector v (WithIdx a))
                  => Catalog v a -> v (WithIdx a)
iVals (Catalog v) = GV.imap (\i cv -> WI i $ _catalogVal cv) v


-- | Compare only on the a value, not on the index
data WithIdx a = WI {-# UNPACK #-} !Int !a deriving (Show)
-- instance Ord a => Eq (WithIdx a) where
--   l == r = l `compare` r == EQ
-- instance Ord a => Eq (WithIdx a) where
--   (WI _ x) `compare` (WI _ y) = x `compare` y


-- | Construct a fractional cascading data structure
--
-- running time: \(O(n)\), where \(n\) is the total size of all input
-- vectors.
mkFC :: forall v a. (Functor v, Vector v a, Vector v (CatalogVal a), Vector v (WithIdx a), Ord a)
     => [v a] -> [Catalog v a]
mkFC = List.foldr f []
  where
     -- f   :: v a -> [Catalog v a] -> [Catalog v a]
    f v = \case
            []       -> let n = GV.length v in [Catalog $ GV.imap (g n) v]
            cs@(c:_) -> addInto (sample c) (GV.toList v) : cs

    g n i x = CatalogVal DataVal x (if i == 0     then Nothing' else Just' $ i+1)
                                   (if i == (n-1) then Nothing' else Just' $ i-1)





addInto     :: (Ord a, Vector v a, Vector v (CatalogVal a)) => [WithIdx a] -> [a] -> Catalog v a
addInto c v = Catalog . GV.fromList $
    Merge.mergeWith (\(WI _ x) y -> x `compare` y)
                    (\(WI i x) -> Bridge i x)
                    DataVal
                    c v

sample :: (Vector v (CatalogVal a), Vector v (WithIdx a)) => Catalog v a -> [WithIdx a]
sample = halve . GV.toList . iVals

-- | returns a list of length floor (n/2) by dropping every second element
halve :: [a] -> [a]
halve = map snd . filter (odd . fst) . zip [1..]


--------------------------------------------------------------------------------

-- | Searches for the successor in each catalog. Returns the indices of these successors
-- (together with the Catalog itself, so that we can start walking from there)
--
-- \(O(\log n + k)\), where \(n\) is the total size of all catalogs,
-- and k is the number of catalogs we search in.
lookupGEs   :: (Vector v (CatalogVal a), Ord a)
            => a -> [Catalog v a] -> [ WithIdx (Catalog v a) ]
lookupGEs x = \case
    []                 -> []
    (c@(Catalog v):cs) -> let i  = binarySearchVec (\cv -> _catalogVal cv >= x) v
                              mj = undefined
                          in WI i c : lookupGEsWalk x mj cs
  -- TODO: If there is no j, will there not be a suitable j further down?
  -- I Think I may have to add sentinels that you always promote
  -- even then there may not be a j, i.e. if i was already a sentinel



lookupGEsWalk x mj cs' = case (mj,cs') of
    (Nothing, _)                  -> []
    (Just _,  [])                 -> []
    (Just i,  (c@(Catalog v):cs)) -> let n  = GV.length v
                                         v' = GV.slice i (n-i) v
                                         mj = undefined
                                     in undefined

-- -- TODO: looks like we will need to be able to find the next data
-- -- value/bridge value in constant time instead: i.e. maybe there are
-- -- 100000 Idx'es here, and then only a single dataVal , we cannot
-- -- afford to walk across all of these Idx'es here. (Since we may still
-- -- need them at a later time)

-- -- | Finds the index in the next array to start the search from (by just walking)
-- nextIdx     :: (Vector v (CatalogVal a)) => Int -> v (CatalogVal a) -> Maybe Int
-- nextIdx i v = let n  = GV.length v
--                   v' = GV.slice i (n-i) v
--               in GV.find (isJust . asBridge) v' >>= asBridge


nextIdx = undefined
--------------------------------------------------------------------------------


test = mkFC
  [ V.fromList [1,3,5,7,9,11]
  , V.fromList [2,4,6,8,100,102,104]
  , V.fromList [(-100)..(-90)]
  ]
