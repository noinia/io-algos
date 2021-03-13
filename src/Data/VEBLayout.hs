module Data.VEBLayout
  ( VEBTree
  , VEBNode(..)
  , Index
  , fromAscList

  , vebLayout
  , fromLayout

  , lookup, lookupGT, lookupGE -- , lookupLT, lookupLE
  , searchLeafR
  ) where

import           Control.DeepSeq
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.BinaryTree.LeafTree.Complete (split, lg, pow, div2, size, fullTree)
import qualified Data.BinaryTree.LeafTree.Complete as Complete
import           Data.BinaryTree.LeafTree.Core ( Tree(..)
                                               , Height
                                               , foldTree, traversePrefix
                                               , labelLeaves
                                               )
import qualified Data.BinaryTree.LeafTree.Core as Tree
import qualified Data.List as List
import           Data.Maybe
import qualified Data.Vector as V
import           Debug.Trace
import           GHC.Generics
import           Prelude hiding (lookup)

import           System.Random (randomIO)

--------------------------------------------------------------------------------

type Index  = Word

type VEBTree k v = V.Vector (VEBNode k v)

data VEBNode k v = Leaf' !v
                 | Node' {-# UNPACK #-} !Index !k {-# UNPACK #-} !Index
                 deriving (Show,Eq,Ord,Generic,Generic1)

instance (NFData k, NFData v) => NFData (VEBNode k v) where rnf = rnf1
instance NFData k => NFData1 (VEBNode k)

-- | pre: input is a complete tree
vebLayout   :: Tree k v -> VEBTree k v
vebLayout t = V.fromList $ vebLayout' 0 t (Complete.height t)

vebLayout'                   :: Index        -- ^ starting index
                             -> Tree k v
                             -> Height       -- ^ height of the input tree
                             -> [VEBNode k v]
vebLayout' _ (Leaf v)       0 = [Leaf' v]
vebLayout' s t@(Node l k r) h = case h of
    1 -> let res = [ Node' (s+1) k (s+2)] <> vebLayout' (s+1) l 0 <> vebLayout' (s+2) r 0
         in res
    _ -> let ht               = div2 h -- height of the top tree
             hb               = h - ht - 1 -- height of the bottom tree
             top              = labelLeaves $ split ht t
             st               = size ht  -- size of the top tree
             sb               = size hb  -- size of a bottom tree
             leaf (i,(l,k,r)) = let sl = s + st + i*2*sb -- start of the left subtree
                                    sr = sl + sb         -- start of the right subtree
                                in ( Leaf (Node' sl k sr)
                                   , vebLayout' sl l hb <> vebLayout' sr r hb
                                   )
             (top',bottoms)   = foldTree leaf (\(l,ls) k (r,rs) -> (Node l k r,
                                                                    ls <> rs)) top
             topTree          = map extract (vebLayout' s top' ht)

         in topTree <> bottoms
vebLayout' s t h = error "vebLayout': absurd" --  $ show (s,t,h)


extract = \case
  Leaf' v     -> v
  Node' l k r -> Node' l k r

roundTrip' h = roundTrip (fullTree 0 h)

roundTrip t = fromLayout (vebLayout t) == t


-- |
-- pre: input is nonEmpty
fromLayout :: VEBTree k v -> Tree k v
fromLayout = fromLayout' 0

extractLeaf = \case
  Leaf' v -> Just v
  _       -> Nothing


--------------------------------------------------------------------------------

shift       :: Index -> [VEBNode k v] -> [VEBNode k v]
shift delta = map (\case
                      Node' l k r -> Node' (l + delta) k (r + delta)
                      lf          -> lf
                  )







baseCase   :: (v -> v -> k) -> [v] -> [VEBNode k v]
baseCase f = concat
           . zipWith (\i [l,r] -> [Node' (3*i+1) (f l r) (3*i+2), Leaf' l, Leaf' r]
                     ) [0..]
           . chunksOf 2

-- -- | pre; input is of size 2^h
-- bottomUp     :: (v -> k) -> (k -> k -> k) -> [v] -> [VEBNode k v]
-- bottomUp f m = repeatedly combine . map Leaf'
--   where
--     combine         :: [[VEBNode k v]] -> [VEBNode k v]
--     combine bottoms = zipWith (\i (b:_) -> case b of
--                                              Leaf' _ ->

--                                  ) [0..] bottoms

-- mkVEB :: [k] -> [VEB k]
-- mkVEB = repeatedly (\xs -> ND (length xs) xs) . map LF


-- combine                       :: Height -- ^ height of the top tree
--                               -> Height -- ^ height of the bottom trees
--                               -> [(Index,k)] -- ^  bottom trees
--                               -> (k, [VEBNode k v]) -- ^ key to use for the next round
--                                                     -- as well as the nodes for the top tree
-- combine th bh bottoms@((_,m)) = (m,ys)
--   where
--     vals = zipWith (\i [(li,_),(ri,kr)] -> Node' li kr ri
--                    ) .chunksOf 2 $ bottoms


-- data Chunk k v = Chunk { delta :: Index -- offset to apply to all elements in this list
--                        ,
--                        }

-- bottomUp :: Height -> -- height of the trees we have so far
--          -> [[VEBNode k v]] -- the bottom trees so far
--          -> []



data VEB k v = LF !v
             | ND {-# UNPACK #-} !Height
                                 [(k,VEB k v)]
             deriving (Show,Eq)

mkVEB   :: (v -> k) -> [v] -> VEB k v
mkVEB f = snd . bottomUp (\v -> (f v, LF v)) (\h _ chs@((k,_):_) -> (k, ND h chs))

-- levelsOf :: VEB k v -> [[k]]
-- levelsOf = \case
--   LF _ -> [[]]
--   ND _ chs -> map fst chs : flatten $ map (levelsOf . snd) chs
--   where
--     -- flatten :: [ [[k]] ] -> [[k]]
--     flatten = foldr1 (zipWith (<>))

type Size = Int

bottomUp               :: (v -> b)
                       -> (Height -> Size -> [b] -> b)
                       -- ^ height of the children, number of children, result from children
                       -> [v] -> b
bottomUp mkLeaf mkNode = go 1 4 . baseCase . map mkLeaf
  where
    -- go     :: Height -> Size -> [b] -> b
    go h n = \case
               [x] -> x
               xs  -> let h'     = 2*h
                          n'     = n*n
                          chunks = chunksOf n xs
                      in go h' n' (map (mkNode h n) chunks)

    baseCase = \case
      xs@[_] -> xs
      xs     -> map (mkNode 0 2) . chunksOf 2 $ xs

layout2 :: [k] -> VEBTree k k
layout2 = V.fromList . layout' 0 . layout id

data VEBL k v = LF' !v
              | ND' !Height -- height of the children
                    [(k,VEBL k v)]
              deriving (Show,Eq)

layout   :: (v -> k) -> [v] -> VEBL k v
layout f = snd . bottomUp (\v -> (f v,LF' v))
                          (\h n chs -> case h of
                            0 -> let [(m,_),(_,_)] = chs
                                 in (m, ND' 0 chs)
                            _ -> let ((m,_):_) = chs
                                 in (m, ND' h chs)
                          )

size' = \case
  LF' _   -> 1
  ND' h _ -> pow (h + 2) - 1

layout'   :: Index -> VEBL k v -> [VEBNode k v]
layout' s = \case
    LF' v                -> [Leaf' v]
    ND' 0  [(_,l),(k,r)] -> [ Node' (s+1) k (s+2) ] <> layout' (s+1) l <> layout' (s+2) r
    ND' 1  [(_,l),(k,r)] -> [ Node' (s+1) k (s+4) ] <> layout' (s+1) l <> layout' (s+4) r
    ND' hb chs           -> let top         = layout fst bottomRoots
                                bottomRoots = mkTop s sb chs
                                sb          = size hb
                                st          = size' top
                                bottoms   = concat $ zipWith f [0..] chs
                                f i (_,b) = layout' (s + st + i*sb) b
                            in map cleanLeaf (layout' s top) <> bottoms


cleanLeaf = \case
  Leaf' (k,lf) -> lf
  Node' l k r  -> Node' l k r

mkTop      :: Index -> Index -> [(k, b)] -> [(k, VEBNode k v)]
mkTop s sb = zipWith (\i [(m,l),(k,r)] -> let li = s+i*2*sb
                                              ri = li + sb
                                          in (m,Node' li k ri)
                     ) [0..] . chunksOf 2





-- -- | pre; input is of size 2^h
-- repeatedly   :: (Height -> Int -> [a] -> a)
--              -> [a] -> [a]
-- repeatedly f = go 1 2 []
--   where
--     go h n acc = \case
--                    [x] -> x:acc
--                    xs  -> let h'     = 2*h -- height of the tree to construct doubles
--                               n'     = n*n -- number of children squares
--                               chunks = chunksOf n xs
--                           in go h' n' (xs <> acc) (map (f h n) chunks)





-- -- | pre; input is of size 2^h
-- repeatedly   :: (Height -> Int -> [a] -> a)
--              -> [a] -> [a]
-- repeatedly f = go 1 2 []
--   where
--     go h n acc = \case
--                    [x] -> x:acc
--                    xs  -> let h'     = 2*h -- height of the tree to construct doubles
--                               n'     = n*n -- number of children squares
--                               chunks = chunksOf n xs
--                           in go h' n' (xs <> acc) (map (f h n) chunks)

-- embed :: VEB v -> [Node' () v]
-- embed = go
--   where
--     go = \case
--       LF v     -> [Leaf' v]
--       ND h chs -> let (ND bh _:_) = chs
--                       s   = size th
--                       top = []
--                   in top <> zipWith (\i (ND bh ch) -> Node' (s + i*2*s) () (i*2*s+s)
-- )
--                      [0..] chs


-- -- | pre; input is of size 2^h
-- repeatedly   :: (Height -> Int -> [a] -> a)
--              -> [a] -> [a]
-- repeatedly f = go 0 1 []
--   where
--     go h n acc = \case
--                    [x] -> x:acc
--                    xs  -> let h'     = 2*h -- height of the tree to construct doubles
--                               n'     = n*n -- number of children squares
--                               chunks = chunksOf n' xs
--                           in go h' n' (xs <> acc) (map (f h n) chunks)



chunksOf   :: Int -> [a] -> [[a]]
chunksOf n = go
  where
    go xs = case List.splitAt n xs of
              (ys,[])   -> [ys]
              (ys,rest) -> ys: go rest

chunksOf'   :: Int -> [a] -> [[a]]
chunksOf' n = snd . foldr (\x (i,xss@(xs:yss)) -> if i == 0 then (n-1, [x]:xss)
                                                            else (i-1, (x:xs):yss)
                          ) (n,[[]])

-- layoutOnly                :: [v] -> Height -> [VEBNode Int Int]
-- layoutOnly xs h = case h of
--                      0 -> let (x:_) = xs in [Leaf' x]
--                      1 -> let l =

--                        [Node' 0 1 ]



--------------------------------------------------------------------------------
-- * Reconstructing a Tree

-- All of this should work for any tree embedded in an array.

fromLayout'      :: Index -> VEBTree k v -> Tree k v
fromLayout' s xs = go s
  where
    go i = case xs V.! fromIntegral i of
             Leaf' v       -> Leaf v
             Node' li k ri -> Node (go li) k (go ri)

-- fromLayout'                    :: Index -> VEBTree k v -> Height -> Tree k v
-- fromLayout' s xs h | h == 0    = let Leaf' v = xs V.! (fromIntegral s)
--                                  in Leaf v
--                    | otherwise = let Node' li k ri = xs V.! (fromIntegral s)
--                                  in Node (fromLayout' li xs (h-1)) k (fromLayout' ri xs (h-1))


-- | Binary-search on a tree. the predicate indicates if we should go right
searchLeafR           :: (k -> Bool) -> VEBTree k v -> v
searchLeafR goRight t = searchLeafR' goRight 0 t h
  where
    n = V.length t
    h = lg n

-- | implementation of the binary search
searchLeafR' :: (k -> Bool) -> Index -> VEBTree k v -> Height -> v
searchLeafR' goRight = f
  where
    f s t h | h == 0    = let Leaf' lf = t V.! fromIntegral s
                          in lf
            | otherwise = let Node' li k ri = t V.! fromIntegral s
                              h'            = h - 1
                          in if goRight k then f ri t h' else f li t h'

--------------------------------------------------------------------------------




test = vebLayout $ first show $ fullTree 0 3


showRoot :: (Show k, Show v) => Tree k v -> String
showRoot = \case
  t@(Leaf _)   -> show t
  (Node _ k _) -> "Root " <> show k

showByLevels f = mapM_ (print . map showRoot)
             . Tree.levels
             . f
             . first show
             $ fullTree 0 3

showOrig = showByLevels id
showVEB  = showByLevels $ fromLayout . vebLayout

--------------------------------------------------------------------------------

fromAscList :: [(k,v)] -> VEBTree (Maybe k) (Maybe (k,v))
fromAscList = vebLayout . Complete.fromAscList

-- testT :: Tree Int Int
testT = fromAscList $ map (\x -> (x,x)) [0,1,2,3,4,5,6,7]

--------------------------------------------------------------------------------

lookup   :: Ord k => k -> VEBTree (Maybe k) (Maybe (k,v)) -> Maybe v
lookup q = fmap snd . lookup' (== Just q)

lookupGT   :: Ord k => k -> VEBTree (Maybe k) (Maybe (k,v)) -> Maybe (k,v)
lookupGT q = lookup' (> Just q)

lookupGE   :: Ord k => k -> VEBTree (Maybe k) (Maybe (k,v)) -> Maybe (k,v)
lookupGE q = lookup' (>= Just q)



-- lookupLT   :: Ord k => k -> VEBTree (Maybe k) (Maybe (k,v)) -> Maybe (k,v)
-- lookupLT q = lookup' (< Just q)

-- lookupLE   :: Ord k => k -> VEBTree (Maybe k) (Maybe (k,v)) -> Maybe (k,v)
-- lookupLE q = lookup' (<= Just q)





lookup'     :: (Maybe k -> Bool) -> VEBTree (Maybe k) (Maybe (k,v)) -> Maybe (k,v)
lookup' p t = let lf = searchLeafR p t
              in if p (fst <$> lf) then lf else Nothing

--------------------------------------------------------------------------------

genInput   :: Int -> IO [Int]
genInput n = replicateM n randomIO

testTT = fromAscList . map (\x -> (x,x)) <$> genInput 14



--------------------------------------------------------------------------------
