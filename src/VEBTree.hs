module VEBTree where

import           BinarySearch
import           Control.Applicative
import qualified Data.DList as DList
import           Data.Functor.Classes
import qualified Data.List as List
import           Data.Maybe (mapMaybe, isJust)
import qualified Data.Vector as V
import           Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as U
-- import           GHC.Exts (Int(..))
-- import           GHC.Integer.Logarithms (integerLog2#)
import qualified Merge
import           Prelude hiding (lookup)

import           Debug.Trace

--------------------------------------------------------------------------------
-- * Pointer based trees

data PTree k a = PLeaf (k,a)
               | PNode (PTree k a) k (PTree k a) deriving (Show,Eq,Ord,Functor)





getK (PLeaf (k,_)) = k
getK (PNode _ k _) = k

-- | pre: length = 2^h for some height h.
pTreeFromList :: Ord k => [(k,a)] -> PTree k a
pTreeFromList xs = fst $ pTreeFromList' (length xs) xs


-- | Pre, there are 2^h nodes, for height h
height = \case
  PLeaf _ -> 1
  PNode l _ _ -> 1 + height l



both f (l,r) = (f l, f r)

pTreeFromList' _ [x@(k,_)] = (PLeaf x, k)
pTreeFromList' n xs        = let h             = n `div` 2
                                 ((l,k),(r,m)) = both (pTreeFromList' h) $ splitAt h xs
                             in (PNode l k r, m)

pLookupGE q = go
  where
    go = \case
      PLeaf t@(k,_) | q <= k    -> Just t
                    | otherwise -> Nothing
      PNode l k r               -> case q `compare` k of
                                     LT -> go l <|> go r
                                     EQ -> go l <|> go r
                                     GT -> go r



-- | Map an accumulating function over all leaves, from "left" to "right"
mapLeavesWith   :: (acc -> a -> SP acc b) -> acc -> PTree k a -> SP acc (PTree k b)
mapLeavesWith f = go
  where
    go acc = \case
      PLeaf (k,x) -> let SP acc' y = f acc x in SP acc' (PLeaf (k,y))
      PNode l k r -> let SP acc'  l' = go acc  l
                         SP acc'' r' = go acc' r
                     in SP acc'' (PNode l' k r')



--------------------------------------------------------------------------------

-- | Data we store at each location in the array
data VNode k = VLeaf { _keyVal  :: !k
                     , _dataIdx :: {-# UNPACK #-} !Int -- index in the data array
                     }
             | VNode { _leftChildIdx  :: {-# UNPACK #-} !Int
                     , _keyVal        :: !k
                     , _rightChildIdx :: {-# UNPACK #-} !Int
                     }
             deriving (Show,Eq,Ord)

-- | BST In VanEmdeBoas Layout
data VEBTree vk va k a = VEBTree { _keys   :: !(vk (VNode k))
                                 , _values :: !(va a)
                                 }

instance (VEB vk va k a, Show k, Show a) => Show (VEBTree vk va k a) where
  show (VEBTree ks vs) = mconcat ["VEBTree ", show (GV.toList ks), " ", show (GV.toList vs)]


type VEB vk va k a = ( Vector vk (VNode k)
                     , Vector va a
                     )


vebSingleton k x = VEBTree (GV.singleton (VLeaf k 0)) (GV.singleton x)




--------------------------------------------------------------------------------
-- * Pretending to be a regular "pointer-based" Tree

data Tree vk va k a = Tree { _treeData :: !(VEBTree vk va k a)
                           , _idx      :: {-# UNPACK #-} !Int
                           }




singleton k x = Tree (vebSingleton k x) 0

root   :: VEBTree vk va k a -> Tree vk va k a
root t = Tree t 0


data Leaf' k a = Leaf' !k a deriving (Show,Eq)

data Node' vk va k a = Node' (Tree vk va k a) !k (Tree vk va k a)



asLeaf                          :: VEB vk va k a => Tree vk va k a -> Maybe (Leaf' k a)
asLeaf (Tree (VEBTree ks vs) i) = ks GV.!? i >>= \case
                                    VLeaf k j   -> Leaf' k <$> vs GV.!? j
                                    VNode _ _ _ -> Nothing


asNode                            :: VEB vk va k a => Tree vk va k a -> Maybe (Node' vk va k a)
asNode (Tree t@(VEBTree ks vs) i) = ks GV.!? i >>= \case
                                      VLeaf _ _   -> Nothing
                                      VNode l k r -> Just $ Node' (Tree t l) k (Tree t r)


pattern Leaf     :: VEB vk va k a => k -> a -> Tree vk va k a
pattern Leaf k x <- (asLeaf -> Just (Leaf' k x))

pattern Node       :: VEB vk va k a => Tree vk va k a -> k -> Tree vk va k a -> Tree vk va k a
pattern Node l k r <- (asNode -> Just (Node' l k r))


instance (Show k, Show a, VEB vk va k a) => Show (Tree vk va k a) where
  show = \case
    Leaf k x   -> mconcat ["Leaf ", show k, " ", show x]
    Node l k r -> mconcat ["Node (", show l, ") ", show k, " (", show r, ")"]
    Tree v i -> mconcat [show v, "XXXXX", show i]





--------------------------------------------------------------------------------
-- * Constructing a VEB Tree

fromPTree   :: (VEB vk va k a, Functor vk
               , Show k, Show a
               ) => PTree k a -> Tree vk va k a
fromPTree t = fromPTreeWithHeight (height t) t

fromPTreeWithHeight     :: (VEB vk va k a, Functor vk
                           , Show a, Show k
                           ) => Int -> PTree k a -> Tree vk va k a
fromPTreeWithHeight h t = Tree (fromPTree' h t) 0

data SP a b = SP !a !b deriving (Functor)

data GAcc a = Acc { _deltaK  :: {-# UNPACK #-} !Int
                  , _deltaA  :: {-# UNPACK #-} !Int
                  , _bottoms :: !a
                  } deriving (Show,Eq)
type Acc vk va k a = GAcc (DList.DList (VEBTree vk va k a))


-- FIXME:
-- hmm, this still seems to have running time: O(n log log n), since I
-- think we may have to concat the arrays at every level of recursion.
-- That is still not very nice.
fromPTree'                :: forall vk va k a. (VEB vk va k a, Functor vk
                                               , Show a, Show k
                                               )
                          => Int -- ^ the height of the tree
                          -> PTree k a
                          -> VEBTree vk va k a
-- fromPTree' h t | h /= (height t) = error $ "fromPTree' wrong height: " <> show (h,t)
fromPTree' _ (PLeaf (k,x))                         = vebSingleton k x
fromPTree' _ (PNode (PLeaf (l,x)) k (PLeaf (r,y))) = VEBTree ks vs
  where
    ks = GV.fromList [VNode 1 k 2, VLeaf l 0, VLeaf r 1]
    vs = GV.fromList [x,y]
-- fromPTree' h t | traceShow ("fromPTree'", h,t) False = undefined
fromPTree' h t                                     = combine top (DList.toList bottoms)
  where
    ht = (h + 1) `div` 2
    hb = 1 + h - ht
    -- in total ht + hb should sum to h+1, since the a leaf in the top
    -- tree coincides with the root node in the bottom trees.

    -- t' will be a tree of height ht
    t' = splitAtDepth (ht-1) t

    -- The number of internal nodes in the top tree; note that all
    -- leaves in the top tree do not need their own VNode, since they
    -- actually correspond with the roots of the bottom trees.
    ts = sizeH (ht -1)

    -- -- size of the bottom trees
    -- bs = sizeH hb

    -- | Transform the top tree
    top = fromPTree' ht top'



    -- computes the new indices at which the bottom trees will start;
    -- this information will now be stored in leaves. Simultaneously,
    -- collect the bottom trees
    SP (Acc _ _ bottoms) top' = mapLeavesWith (embed hb) (Acc ts 0 DList.empty) t'
    -- note that we shift the keys by ts; the number of keys in the
    -- top tree, and the data by 0; since once we embed the top tree,
    -- it does not actually have any data anymore.

-- | Embeds a bottom tree.
embed                               :: forall vk va k a. (VEB vk va k a, Functor vk
                                                         , Show k, Show a
                                                         )
                                    => Int           -- ^ height of a bottom tree
                                    -> Acc vk va k a -- ^ accumulator
                                    -> PTree k a     -- ^ The bottom tree itself
                                    -> SP (Acc vk va k a) Int
embed hb (Acc deltaK deltaA bss) bt = SP acc deltaK
      where
        bs = sizeH hb
        ls = numLeaves hb -- we shift the data values by by the amount of leaves

        acc = Acc (deltaK+bs) (deltaA+ls) (bss `DList.snoc` bt')
        bt' = shift deltaK deltaA $ fromPTree' hb bt

-- | Ditch the leaves of the top tree and combine into a single VEBTree
combine       :: (VEB vk va k a, Functor vk)
              => VEBTree vk U.Vector k Int -> [VEBTree vk va k a] -> VEBTree vk va k a
combine t bts = VEBTree (GV.concat $ topKeys t : map _keys bts)
                        (GV.concat $ map _values bts)

-- | Replace the Keys in the leaves of the top tree by the values
topKeys                   :: (Vector vk (VNode k))
                          => VEBTree vk U.Vector k Int -> vk (VNode k)
topKeys t@(VEBTree ks vs) = GV.imapMaybe f ks
  where
    f i n = case (Tree t i) of
              Leaf _ _                     -> Nothing
              Node (Tree _ l) k (Tree _ r) -> Just $ VNode (repl l) k (repl r)

    -- if a leaf; replace by its value, otherwise leave in tact
    repl i = case (Tree t i) of
               Leaf _ j   -> j
               Node _ _ _ -> i

class Shiftable t where
  shift :: Int -- ^ the delta to shift the key indices with
        -> Int -- ^ the delta to shift the values indices with
        -> t -> t

instance Shiftable (VNode k) where
  shift deltaK deltaA = \case
      VLeaf k i   -> VLeaf k (i+deltaA)
      VNode l k r -> VNode (l+deltaK) k (r+deltaK)

instance (Functor vk, Vector vk (VNode k)) => Shiftable (VEBTree vk va k a) where
  shift deltaK deltaA (VEBTree ks vs) = VEBTree (shift deltaK deltaA <$> ks) vs
instance (Functor vk, Vector vk (VNode k)) => Shiftable (Tree vk va k a) where
  shift deltaK deltaA (Tree t i) = Tree (shift deltaK deltaA t) (i+deltaK)


-- | Cuts the tree at a given depth. The root is considered to have
-- depth zero. Note that cutting at depth d gives a tree of height d+1
--
--
splitAtDepth :: Int -> PTree k a -> PTree k (PTree k a)
splitAtDepth = go
  where
    go 0 t = PLeaf (getK t, t)
    go d (PLeaf _)     = error "splitAtDepth: tree not deep enough"
    go d (PNode l k r) = let d' = d-1 in PNode (go d' l) k (go d' r)

leaves :: PTree k a -> [(k,a)]
leaves = \case
    PLeaf x     -> [x]
    PNode l _ r -> leaves l <> leaves r

--------------------------------------------------------------------------------


lookupGE q = go
  where
    go = \case
      Leaf k x | q <= k    -> Just (k,x)
               | otherwise -> Nothing
      Node l k r           -> case q `compare` k of
                                LT -> go l <|> go r
                                EQ -> go l <|> go r
                                GT -> go r

--------------------------------------------------------------------------------

-- lg   :: Int -> Int
-- lg i = I# (integerLog2# (fromIntegral i))

-- | size of a tree with n leaves
size n = 2*n - 1

-- -- | Height of a tree with n leaves
-- height n = 1 + lg n

-- | number of leaves in a full tree of height h
numLeaves 0 = 0
numLeaves h = 2 ^ (h - 1)

-- | size of a tree as a function of its height
sizeH = size . numLeaves

--------------------------------------------------------------------------------

testPTree = pTreeFromList [(1,1),(2,2),(3,3),(5,5)]

test :: Tree V.Vector V.Vector Int Int
test = fromPTree testPTree


testPTree' = pTreeFromList [ (i,i) | i <- [1..8] ]

test' :: Tree V.Vector V.Vector Int Int
test' = fromPTree testPTree'
