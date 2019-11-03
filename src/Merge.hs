module Merge where


merge                       :: Ord a => [a] -> [a] -> [a]
merge []         rs         = rs
merge ls         []         = ls
merge ls@(l:ls') rs@(r:rs') = case l `compare` r of
                                 LT -> l : merge ls' rs
                                 EQ -> l : merge ls' rs
                                 GT -> r : merge ls  rs'

-- | Generic merging
mergeWith               :: (l -> r -> Ordering) -> (l -> a) -> (r -> a) -> [l] -> [r] -> [a]
mergeWith cmp f g as bs = go as bs
  where
    go []         rs         = map g rs
    go ls         []         = map f ls
    go ls@(l:ls') rs@(r:rs') = case l `cmp` r of
                                 LT -> f l : go ls' rs
                                 EQ -> f l : go ls' rs
                                 GT -> g r : go ls  rs'
