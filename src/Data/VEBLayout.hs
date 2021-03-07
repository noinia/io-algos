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
                                               , Height, height
                                               , foldTree, traversePrefix
                                               , labelLeaves
                                               )
import qualified Data.BinaryTree.LeafTree.Core as Tree
import           Data.Bits
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


vebLayout   :: Tree k v -> VEBTree k v
vebLayout t = V.fromList $ vebLayout' 0 t (height t)

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
vebLayout' s t h = error "vebLayout': absurd" -- $ show (s,t,h)


extract = \case
  Leaf' v     -> v
  Node' l k r -> Node' l k r

roundTrip' h = roundTrip (fullTree 0 h)

roundTrip t = fromLayout (vebLayout t) == t


-- |
-- pre: input is nonEmpty
fromLayout   :: VEBTree k v -> Tree k v
fromLayout t = fromLayout' 0 t h
  where
    n = V.length t
    h = lg n

extractLeaf = \case
  Leaf' v -> Just v
  _       -> Nothing


--------------------------------------------------------------------------------
-- * Reconstructing a Tree

-- All of this should work for any tree embedded in an array.


fromLayout'                    :: Index -> VEBTree k v -> Height -> Tree k v
fromLayout' s xs h | h == 0    = let Leaf' v = xs V.! (fromIntegral s)
                                 in Leaf v
                   | otherwise = let Node' li k ri = xs V.! (fromIntegral s)
                                 in Node (fromLayout' li xs (h-1)) k (fromLayout' ri xs (h-1))


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
    f s t h | h == 0    = let Leaf' lf = t V.! (fromIntegral s)
                          in lf
            | otherwise = let Node' li k ri = t V.! (fromIntegral s)
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
