module VEBTree where

import           BinarySearch
import           Control.Applicative
import qualified Data.DList as DList
import qualified Data.List as List
import           Data.Maybe (mapMaybe, isJust)
import           Data.Proxy
import qualified Data.Vector as V
import           Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as U
import qualified Merge
import           Prelude hiding (lookup)

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

lookupGE q = go
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


--------------------------------------------------------------------------------
-- * Constructing a VEB Tree

fromPTree     :: (VEB vk va k a, Functor vk) => Int -> PTree k a -> Tree vk va k a
fromPTree h t = Tree (fromPTree' h t) 0

data SP a b = SP !a !b deriving (Functor)

fromPTree'                :: forall (vk :: * -> *) va k a. (VEB vk va k a, Functor vk)
                          => Int -> PTree k a -> VEBTree vk va k a
fromPTree' _ (PLeaf (k,x)) = vebSingleton k x
fromPTree' h t             = combine top bottoms
  where
    ht = (h + 1) `div` 2
    hb = h - ht

    t' = splitAtDepth ht t

    -- The number of internal nodes in the top tree; note that all
    -- leaves in the top tree do not need their own VNode, since they
    -- actually correspond with the roots of the bottom trees.
    ts = size (ht -1)

    -- size of the bottom trees
    bs = size hb

    top = undefined
    bottoms = undefined
-- -- mapLeavesWith   :: (acc -> a -> SP acc b) -> acc -> PTree k a -> SP acc (PTree k b)

    proxyK :: Proxy k
    proxyK = Proxy

    proxyVK :: Proxy vk
    proxyVK = Proxy

--     top     :: VEB vk U.Vector k Int => VEBTree vk U.Vector k Int
--     top     = fromPTree' ht top'

--     bottoms :: [VEBTree vk va k a]
--     bottoms = DList.toList $ bottoms'

--     top' :: PTree k Int
    -- bottoms' :: DList.DList (VEBTree vk va k a)
    SP (SP _ bottoms') top' = mapLeavesWith (embed bs hb proxyK proxyVK) (SP ts DList.empty) t'


embed                   :: forall (vk :: * -> *) va (k :: *) a. (VEB vk va k a, Functor vk)
                        => Int
                        -> Int
                        -> Proxy k
                        -> Proxy vk
                        -> SP Int (DList.DList (VEBTree vk va k a))
                        -> PTree k a
                        -> SP (SP Int (DList.DList (VEBTree vk va k a))) Int
embed bs hb _ _ (SP delta bss) bt = SP acc delta
      where
        acc = SP (delta+bs) (bss `DList.snoc` bt')
        bt' = shift delta delta $ fromPTree' hb bt

-- | Ditch the leaves of the top tree and combine into a single VEBTree
combine       :: (VEB vk va k a, Functor vk)
              => VEBTree vk U.Vector k Int -> [VEBTree vk va k a] -> VEBTree vk va k a
combine t bts = VEBTree (GV.concat $ topKeys t : map _keys bts)
                        (GV.concat $ map _values bts)

-- | Replace the Keys in the laves of the top tree by the values
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
  shift :: Int -> Int -> t -> t

instance Shiftable (VNode k) where
  shift deltaK deltaA = \case
      VLeaf k i   -> VLeaf k (i+deltaA)
      VNode l k r -> VNode (l+deltaK) k (r+deltaK)
instance (Functor vk, Vector vk (VNode k)) => Shiftable (VEBTree vk va k a) where
  shift deltaK deltaA (VEBTree ks vs) = VEBTree (shift deltaK deltaA <$> ks) vs
instance (Functor vk, Vector vk (VNode k)) => Shiftable (Tree vk va k a) where
  shift deltaK deltaA (Tree t i) = Tree (shift deltaK deltaA t) (i+deltaK)


-- | Cuts the tree at a given depth
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

lg :: Int -> Int
lg = ceiling . logBase 2 . fromIntegral

-- | size of a tree with n leaves
size n = 2*n - 1

-- -- | Height of a tree with n leaves
-- height n = 1 + lg n

--------------------------------------------------------------------------------

testPTree = pTreeFromList [(1,1),(2,2),(3,3),(5,5)]
