module VEBLayout where

import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Bits
import qualified Data.List as List
import           Data.Maybe
import qualified Data.Vector as V
import           Debug.Trace
import           GHC.Stack

--------------------------------------------------------------------------------

data Tree k v = Leaf v
              | Node (Tree k v) k (Tree k v)
                deriving (Show,Eq,Ord)

instance Bifunctor Tree where
  bimap f g = \case
    Leaf v     -> Leaf (g v)
    Node l k r -> Node (bimap f g l) (f k) (bimap f g r)

type Index = Word

fullTree                 :: Index -> Height -> Tree Index Index
fullTree s h | h == 0    = Leaf s
             | otherwise = let h' = h - 1
                           in Node (fullTree s h')
                                   (s + (pow h'))
                                   (fullTree (s + pow h') h')


root = \case
  Leaf x     -> x
  Node _ x _ -> x

-- fromAscList'' :: [v] -> Tree v v
-- fromAscList'' = head . pairup [] . map Leaf
--   where
--     pairup _acc [x]        = [x]
--     pairup acc  []         = pairup [] (reverse acc)
--     pairup acc  (x:y:rest) = pairup (Node x (root x) y : acc) rest


fromAscList' xs = let n  = length xs
                      h  = lg n
                      m  = pow h
                      m' | m == n    = m
                         | otherwise = 2*m
                  in fromAscList'' $ replicate (m' - n) Nothing <> map Just xs

-- | pre: input is has lenght a power of 2
fromAscList'' :: [v] -> Tree v v
fromAscList'' = fst . head . pairup [] . map (\x -> (Leaf x,x))
  where
    pairup _acc [x]                = [x]
    pairup acc  []                 = pairup [] (reverse acc)
    pairup acc  ((x,k):(y,m):rest) = pairup ((Node x k y,m) : acc) rest





foldTree           :: (v -> b) -> (b -> k -> b -> b) -> Tree k v -> b
foldTree leaf node = go
  where
    go = \case
      Leaf v     -> leaf v
      Node l k r -> node (go l) k (go r)


traversePrefix           :: Applicative f
                         => (v -> f v') -> (k -> f k') -> Tree k v -> f (Tree k' v')
traversePrefix leaf node = go
  where
    go = \case
      Leaf v     -> Leaf <$> leaf v
      Node l k r -> (\k' l' r' -> Node l' k' r') <$> node k <*> go l <*> go r

labelLeaves  :: Tree k v -> Tree k (Index,v)
labelLeaves = flip evalState 0 . traversePrefix (\k -> do i <- get
                                                          modify (+1)
                                                          pure (i,k)
                                                ) pure


type VEBTree k v = V.Vector (Tree' k v)

data Tree' k v = Leaf' v
               | Node' {-# UNPACK #-} !Index k {-# UNPACK #-} !Index
               deriving (Show,Eq,Ord)


type Height = Int


pow h = 2 ^ h

-- | A tree of height h has 2^h elements. Trees
height = foldTree (const 0) (\l _ r -> 1 + max l r)

myTree = Node (Leaf "a") "u" (Node (Leaf "l") "v" (Leaf "r"))

myFullTree = Node (Node (Leaf "a") "x" (Leaf "b"))
                  "u"
                  (Node (Leaf "l") "v" (Leaf "r"))

myTree1 = Node (Leaf "l") "v" (Leaf "r")

size h = pow (h+1) - 1

vebLayout   :: Tree k v -> VEBTree k v
vebLayout t = V.fromList $ vebLayout' 0 t (height t)

vebLayout'                   :: Index        -- ^ starting index
                             -> Tree k v
                             -> Height       -- ^ height of the input tree
                             -> [Tree' k v]
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


leaves = foldTree (:[]) (\l _ r -> l <> r)


extractLeaf = \case
  Leaf' v -> Just v
  _       -> Nothing


lg 1 = 0
lg n = 1 + lg (div2 n)

div2 h = h `shiftR` 1 -- h `div` 2


fromLayout'                    :: Index -> VEBTree k v -> Height -> Tree k v
fromLayout' s xs h | h == 0    = let Leaf' v = xs V.! (fromIntegral s)
                                 in Leaf v
                   | otherwise = let Node' li k ri = xs V.! (fromIntegral s)
                                 in Node (fromLayout' li xs (h-1)) k (fromLayout' ri xs (h-1))

split                            :: Height -> Tree k v -> Tree k (Tree k v, k, Tree k v)
split h (Node l k r) | h == 0    = Leaf (l,k,r)
                     | otherwise = let h' = h - 1 in Node (split h' l) k (split h' r)



test = vebLayout $ first show $ fullTree 0 3

levels   :: Tree k v -> [[Tree k v]]
levels t = [t] : case t of
  Leaf _     -> [[]]
  Node l _ r -> zipWith (<>) (levels l) (levels r)

showRoot :: (Show k, Show v) => Tree k v -> String
showRoot = \case
  t@(Leaf _)   -> show t
  (Node _ k _) -> "Root " <> show k


showByLevels f = mapM_ (print . map showRoot)
             . levels
             . f
             . first show
             $ fullTree 0 3

showOrig = showByLevels id
showVEB  = showByLevels $ fromLayout . vebLayout

--------------------------------------------------------------------------------

fromAscList :: [v] -> VEBTree (Maybe v) (Maybe v)
fromAscList = vebLayout . fromAscList'

-- | If we satisfy the predicate, go left, otherwise go right.
binarySearch'   :: (k -> Bool) -> Tree k v -> v
binarySearch' p = go
  where
    go = \case
      Leaf v                 -> v
      Node l k r | p k       -> go l
                 | otherwise -> go r

-- |
binarySearch    :: (k -> Bool) -> VEBTree k v -> v
binarySearch p = binarySearch' p . fromLayout


-- testT :: Tree Int Int
testT = fromAscList [0,1,2,3,4,5,6,7]

testQ = binarySearch (> Just 5) testT -- successor query
