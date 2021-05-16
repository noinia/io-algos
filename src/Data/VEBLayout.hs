module Data.VEBLayout
  ( VEBTree
  , Index
  , fromAscList

  , vebLayout
  , Embedded.fromLayout

  , Embedded.lookup, Embedded.lookupGT, Embedded.lookupGE -- , lookupLT, lookupLE
  , Embedded.searchLeafR
  ) where

import           Control.DeepSeq
import           Control.Exception (assert)
import           Control.Monad.State.Strict
import           Data.Bifunctor
import qualified Data.BinaryTree.LeafTree.Embedded as Embedded
import           Data.BinaryTree.LeafTree.Embedded (Index)
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

import           System.Random (randomRIO)

--------------------------------------------------------------------------------

type VEBTree k v = Embedded.Tree k v

-- | pre: input is a complete tree
vebLayout   :: Tree k v -> VEBTree k v
vebLayout t = V.fromList $ vebLayout' 0 t (Complete.height t)

vebLayout'                   :: Index        -- ^ starting index
                             -> Tree k v
                             -> Height       -- ^ height of the input tree
                             -> [Embedded.Node k v]
vebLayout' _ (Leaf v)       0 = [Embedded.Leaf v]
vebLayout' s t@(Node l k r) h = case h of
    1 -> let res = [ Embedded.Node (s+1) k (s+2)] <> vebLayout' (s+1) l 0 <> vebLayout' (s+2) r 0
         in res
    _ -> let ht               = div2 h -- height of the top tree
             hb               = h - ht - 1 -- height of the bottom tree
             top              = labelLeaves $ split ht t
             st               = size ht  -- size of the top tree
             sb               = size hb  -- size of a bottom tree
             leaf (i,(l,k,r)) = let sl = s + st + i*2*sb -- start of the left subtree
                                    sr = sl + sb         -- start of the right subtree
                                in ( Leaf (Embedded.Node sl k sr)
                                   , vebLayout' sl l hb <> vebLayout' sr r hb
                                   )
             (top',bottoms)   = foldTree leaf (\(l,ls) k (r,rs) -> (Node l k r,
                                                                    ls <> rs)) top
             topTree          = map extract (vebLayout' s top' ht)

         in topTree <> bottoms
vebLayout' s t h = error "vebLayout': absurd" --  $ show (s,t,h)


extract :: Embedded.Node k (Embedded.Node k v) -> Embedded.Node k v
extract = \case
  Embedded.Leaf v     -> v
  Embedded.Node l k r -> Embedded.Node l k r



roundTrip' h = roundTrip (fullTree 0 h)

roundTrip t = Embedded.fromLayout (vebLayout t) == t

sameAsTopDown h = let n = pow h in vebLayout (fullTree 0 h) == layout2 (0:|[1..(n-1)])




--------------------------------------------------------------------------------

shift       :: Index -> [Embedded.Node k v] -> [Embedded.Node k v]
shift delta = map (\case
                      Embedded.Node l k r -> Embedded.Node (l + delta) k (r + delta)
                      lf          -> lf
                  )


--------------------------------------------------------------------------------


type Size = Word

-- | Van Embde Boas recursion, in a bottom up fashion
bottomUp               :: Foldable1 f
                       => (v -> b)
                       -> (Height -> Size -> NonEmpty b -> b)
                       -- ^ height of the children, number of children, result from children
                       -> f v -> b
bottomUp mkLeaf mkNode = go 1 . baseCase . fmap mkLeaf . toNonEmpty
  where
    -- go     :: Height -> Size -> NonEmpty b -> b
    go h = \case
               x :| [] -> x
               xs      -> let h'     = 2*h
                              n      = pow h
                              chunks = chunksOf n xs
                          in go h' (fmap (mkNode h n) chunks)

    baseCase = \case
      xs@(_:|[]) -> xs
      xs         -> fmap (mkNode 0 2) . chunksOf 2 $ xs


testBottomUp h0 = bottomUp (const True) (\h n bs -> traceShow (h,F.length bs,n) $ fromIntegral (F.length bs) == n
                                                    -- && n == pow h
                                        )
                  (0:|[1..(pow h0-1)])

--------------------------------------------------------------------------------

data VEB k v = LF !v
             | ND {-# UNPACK #-} !Height               -- binaryheight of the children
                                 (NonEmpty (k,VEB k v))
             deriving (Eq,Show)




mkVEB   :: Foldable1 f => (v -> k) -> f v -> VEB k v
mkVEB f = snd . bottomUp (\v -> (f v, LF v)) (\h _ chs@((k,_):|_) -> (k, ND h chs))

--------------------------------------------------------------------------------

data VEBL k v = LF' !v
              | ND' !Height -- height of the children
                    (NonEmpty (k,VEBL k v))
              deriving (Eq,Show)

mkVEBL :: (Show k) => [k] -> VEBL k k
mkVEBL = layout id . NonEmpty.fromList

-- | Builds a VEBTree
layout   :: Foldable1 f => (v -> k) -> f v -> VEBL k v
layout f = snd . bottomUp (\v -> (f v,LF' v))
                          (\h n chs -> case h of
                            0 -> let (m,_):|[_] = chs
                                 in (m, ND' 0 chs)
                            _ -> let ((m,_):|_) = chs
                                 in (m, ND' h chs)
                          )

foldVEBL :: (v -> b) -> (Height -> NonEmpty (k,b) -> b) -> VEBL k v -> b
foldVEBL leaf node = go
  where
    go = \case
      LF' v     -> leaf v
      ND' h chs -> node h $ fmap (second go) chs







toTree :: VEBL k v -> Tree k v
toTree = foldVEBL Leaf $ \_ chs -> replaceLeaf (Complete.fromAscList2 chs)
  where
    replaceLeaf = foldTree snd Node
    -- FIXME: the keys are wrong since they use minima rather than maxima



-- | The height of the binary tree represented by this VEBL
binaryHeight :: VEBL k v -> Height
binaryHeight = Tree.height . toTree


numChildrenProp :: VEBL k v -> Bool
numChildrenProp = foldVEBL (const True) $ \h chs -> length chs == pow h




layout2    :: (Show k, Foldable1 f) => f k -> VEBTree k k
layout2 xs = V.fromList . F.toList . layout' 0 h . layout id $ xs
  where
    h = lg $ F.length xs


showLayout2 h = let n = pow h
                in mapM_ print . zip [0..] . V.toList . layout2 . NonEmpty.fromList $ [0..(n-1)]






-- | Flatten into an list of embedded tree nodes.
layout'     :: (Show k, Show v)
            => Index -- ^ first free index we can assign to
            -> Height -- ^ the binaryheight of the VEBL (given in as the next arg)
            -> VEBL k v
            -> NonEmpty (Embedded.Node k v)
layout' s h = \case
    LF' v                      -> Embedded.Leaf v :| []
    ND' hb chs | h == 2*hb     -> evenCase hb chs
               | h == 2*hb + 1 -> oddCase  hb chs
               | otherwise     -> error "layout': wrong height!"
  -- write this with divMod?
  where
    evenCase hb = \case
      (l:|[r]) -> baseCase hb l r -- only two children, so we are at the root
      chs      -> generic hb chs (hb -1)

    oddCase  hb = \case
      (l:|[r]) -> baseCase hb l r
      chs      -> generic hb chs hb

    baseCase hb (_,l) (k,r) = let li = s+1
                                  ri = li + size hb
                              in Embedded.Node li k ri <| layout' li hb l <> layout' ri hb r

    generic hb chs th = topFlat <> foldMap1 thd bottoms
      where
        sb          = size hb
        st          = size th
        bottoms     = imap f chs
        f i (k,b)   = let si = s + st + i*sb -- starting index of this bottom tree
                      in traceShow ("f",st,hb,th,i,k) (k, si, layout' si hb b)

        topLeaves   = traceShowId
                    . fmap (\((m,li,_):|[(k,ri,_)]) -> (m,Embedded.Node li k ri) )
                    . chunksOf 2
                    $ bottoms
        top         = layout fst topLeaves
        topFlat     = fmap cleanLeaf (layout' s th top) -- embed the top tree



thd (_,_,x) = x


-- | Computes the size of the tree
size' :: VEBL k v -> Size
size' = \case
  LF' _   -> 1
  ND' h _ -> pow (h + 2) - 1


cleanLeaf :: Embedded.Node k (a, Embedded.Node k v) -> Embedded.Node k v
cleanLeaf = \case
  Embedded.Leaf (k,lf) -> lf
  Embedded.Node l k r  -> Embedded.Node l k r




--------------------------------------------------------------------------------
-- * Helper functions

-- | Splits an non-empty list into chunks of size f each.
--
-- pre: n > 0
chunksOf   :: Foldable1 f => Size -> f a -> NonEmpty (NonEmpty a)
chunksOf n = assert (n > 0) $
             fmap NonEmpty.fromList . snd
           . foldr (\x (i,xss@(xs:|yss)) -> if i == 0 then (n-1, [x] <| xss)
                                                      else (i-1, (x:xs) :| yss)
                         ) (n, [] :| [])


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
showVEB  = showByLevels $ Embedded.fromLayout . vebLayout

--------------------------------------------------------------------------------

fromAscList :: NonEmpty (k,v) -> VEBTree (Maybe k) (Maybe (k,v))
fromAscList = vebLayout . Complete.fromAscList

-- testT :: Tree Int Int
testT = fromAscList . NonEmpty.fromList $ map (\x -> (x,x)) [0,1,2,3,4,5,6,7]

--------------------------------------------------------------------------------

imap   :: (Index -> a -> b) -> NonEmpty a -> NonEmpty b
imap f = NonEmpty.zipWith f (0:|[1..])

--------------------------------------------------------------------------------

-- | Generates n distinct Words in increasing order and writes them to
-- the stated file.
genInput      :: FilePath -> Int -> IO ()
genInput fp n = do
                   incs <- replicateM (n+1) (randomRIO (1 :: Word,10))
                   let xs = tail . List.scanl' (+) 0 $ incs
                   writeFile fp $ unlines . (show n:) . map show $ xs


genInputFile n = genInput ("/tmp/out_pow2_" <> show n <> ".txt") (pow n)





--------------------------------------------------------------------------------
