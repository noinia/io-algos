module MergeSort where

import qualified Data.Vector.Generic.Mutable as V
import Data.Vector.Generic.Mutable(MVector)


mergeAll    :: (MVector v, Monad m) => [v a] -> m (v a)
mergeAll vs = initialize >>= go
  where
    initialize = Map.fromList [ (x,v) | ]
