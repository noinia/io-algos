module Data.VEB.Map
  ( Map(..)
  , fromAscList
  , lookup, lookupGT, lookupGE, lookupLT, lookupLE
  ) where

import qualified Data.VEB.Layout as Layout
import qualified Data.Vector as V

--------------------------------------------------------------------------------

data Map k v = Map { searchStructure :: Layout.VEBTree (Maybe k) (Maybe k)
                   , values          :: V.Vector v
                   } deriving (Show,Eq)

fromAscList :: Enum k => [(k,v)] -> Map k v
fromAscList = undefined

lookupGT   :: Ord k => k -> VEBTree (Maybe k) (Maybe k) -> Maybe k
lookupGT q = lookup' (> Just q)

lookupGE   :: Ord k => k -> VEBTree (Maybe k) (Maybe k) -> Maybe k
lookupGE q = lookup' (>= Just q)

lookupLT   :: Ord k => k -> VEBTree (Maybe k) (Maybe k) -> Maybe k
lookupLT q = lookup' (< Just q)

lookupLE   :: Ord k => k -> VEBTree (Maybe k) (Maybe k) -> Maybe k
lookupLE q = lookup' (<= Just q)
