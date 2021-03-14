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
import           Control.Exception(assert)
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
import qualified Data.Foldable as F
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Semigroup
import           Data.Semigroup.Foldable
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


extract :: VEBNode k (VEBNode k v) -> VEBNode k v
extract = \case
  Leaf' v     -> v
  Node' l k r -> Node' l k r



roundTrip' h = roundTrip (fullTree 0 h)

roundTrip t = fromLayout (vebLayout t) == t


-- |
-- pre: input is nonEmpty
fromLayout :: VEBTree k v -> Tree k v
fromLayout = fromLayout' 0

--------------------------------------------------------------------------------

shift       :: Index -> [VEBNode k v] -> [VEBNode k v]
shift delta = map (\case
                      Node' l k r -> Node' (l + delta) k (r + delta)
                      lf          -> lf
                  )


data VEB k v = LF !v
             | ND {-# UNPACK #-} !Height
                                 (NonEmpty (k,VEB k v))
             deriving (Show,Eq)

mkVEB   :: Foldable1 f => (v -> k) -> f v -> VEB k v
mkVEB f = snd . bottomUp (\v -> (f v, LF v)) (\h _ chs@((k,_):|_) -> (k, ND h chs))


type Size = Int

bottomUp               :: Foldable1 f
                       => (v -> b)
                       -> (Height -> Size -> NonEmpty b -> b)
                       -- ^ height of the children, number of children, result from children
                       -> f v -> b
bottomUp mkLeaf mkNode = go 1 4 . baseCase . fmap mkLeaf . toNonEmpty
  where
    -- go     :: Height -> Size -> NonEmpty b -> b
    go h n = \case
               x :| [] -> x
               xs      -> let h'     = 2*h
                              n'     = n*n
                              chunks = chunksOf n xs
                          in go h' n' (fmap (mkNode h n) chunks)

    baseCase = \case
      xs@(_:|[]) -> xs
      xs         -> fmap (mkNode 0 2) . chunksOf 2 $ xs

layout2 :: (Show k, Foldable1 f) => f k -> VEBTree k k
layout2 = V.fromList . F.toList . layout' 0 . layout id

showLayout2 h = let n = pow h
                in mapM_ print . zip [0..] . V.toList . layout2 . NonEmpty.fromList $ [0..(n-1)]

data VEBL k v = LF' !v
              | ND' !Height -- height of the children
                    (NonEmpty (k,VEBL k v))
              deriving (Show,Eq)

layout   :: Foldable1 f => (v -> k) -> f v -> VEBL k v
layout f = snd . bottomUp (\v -> (f v,LF' v))
                          (\h n chs -> case h of
                            0 -> let [(m,_),(_,_)] = F.toList chs
                                 in (m, ND' 0 chs)
                            _ -> let ((m,_):|_) = chs
                                 in (m, ND' h chs)
                          )

size' = \case
  LF' _   -> 1
  ND' h _ -> pow (h + 2) - 1

layout'   :: (Show k, Show v) => Index -> VEBL k v -> NonEmpty (VEBNode k v)
layout' s = \case
    LF' v                -> Leaf' v :| []
    -- ND' 0  [(_,l),(k,r)] -> [ Node' (s+1) k (s+2) ] <> layout' (s+1) l <> layout' (s+2) r
    -- ND' 1  [(_,l),(k,r)] -> [ Node' (s+1) k (s+4) ] <> layout' (s+1) l <> layout' (s+4) r
    ND' hb chs           -> let top         = traceShowId $ layout fst bottomRoots
                                bottomRoots = mkTop s sb chs
                                sb          = size hb
                                st          = size' top
                                bottoms     = sconcat $ NonEmpty.zipWith f (0:|[1..]) chs
                                f i (_,b)   = layout' (s + st + i*sb) b
                            in fmap cleanLeaf (layout' s top) <> bottoms


cleanLeaf = \case
  Leaf' (k,lf) -> lf
  Node' l k r  -> Node' l k r

mkTop         :: Index -> Index -> NonEmpty (k, b) -> NonEmpty (k, VEBNode k v)
mkTop s sb xs = let chunks = chunksOf 2 xs in case chunks of
                  (_:|[_]) -> f (s+1) chunks
                  _        -> f s     chunks
  where
    f s' = NonEmpty.zipWith (\i ((m,l):|[(k,r)]) -> let li = s'+i*2*sb
                                                        ri = li + sb
                                                    in (m,Node' li k ri)
                            ) (0:|[1..])


-- | pre: n > 0
chunksOf   :: Foldable1 f => Int -> f a -> NonEmpty (NonEmpty a)
chunksOf n = assert (n > 0) $
             fmap NonEmpty.fromList . snd
           . foldr (\x (i,xss@(xs:|yss)) -> if i == 0 then (n-1, [x] <| xss)
                                                            else (i-1, (x:xs) :| yss)
                         ) (n, [] :| [])

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
