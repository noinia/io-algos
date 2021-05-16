{-# LANGUAGE  QuantifiedConstraints #-}
module Data.VEB.Tree where
  -- ( VEBTree
  -- , Index
  -- , fromAscList

  -- , vebLayout
  -- , Embedded.fromLayout

  -- , Embedded.lookup, Embedded.lookupGT, Embedded.lookupGE -- , lookupLT, lookupLE
  -- , Embedded.searchLeafR
  -- ) where

import           Control.DeepSeq
import           Control.Exception (assert)
import           Control.Monad.Identity (Identity(..))
import           Control.Monad.State.Strict
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.BinaryTree.LeafTree.Complete (split, lg, pow, div2, fullTree)
import qualified Data.BinaryTree.LeafTree.Complete as Complete
import           Data.BinaryTree.LeafTree.Core ( Height
                                               , foldTree, traversePrefix
                                               , labelLeaves
                                               )
import qualified Data.BinaryTree.LeafTree.Core as Tree
import           Data.BinaryTree.LeafTree.Embedded (Index)
import qualified Data.BinaryTree.LeafTree.Embedded as Embedded
import           Data.Bitraversable
import qualified Data.DList as DList
import qualified Data.DList.DNonEmpty as DNonEmpty
import qualified Data.Foldable as F
import           Data.Foldable.WithIndex
import           Data.Functor.WithIndex
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Semigroup
import           Data.Semigroup.Bifoldable
import           Data.Semigroup.Foldable
import           Data.Traversable.WithIndex
import qualified Data.Vector as V
import           Debug.Trace
import           GHC.Generics
import           Prelude hiding (lookup)

--------------------------------------------------------------------------------

-- $setup
-- myTree = fromAscList . NonEmpty.fromList $ [(0,"a"),(1,"b"),(2,"c"),(3,"d")]
myTree = fromAscList . NonEmpty.fromList $ [(0,"a"),(1,"b"),(2,"c"),(3,"d")]

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


data Two a = Two !a !a
  deriving (Show,Eq,Functor,Traversable,Foldable)

instance Foldable1 Two

-- | pre: even number of elements
--
-- >>> pairUp [0..5]
-- [Two 0 1,Two 2 3,Two 4 5]
pairUp :: Foldable f => f a -> [Two a]
pairUp = go . F.toList
  where
    go []         = []
    go [_]        = error "pairUp: Odd number of elements!"
    go (a:b:rest) = Two a b : go rest


-- | pre: n = m*k for some natural number m
--
-- >>> chunksOf 3 (0 :| [1..8])
-- (0 :| [1,2]) :| [3 :| [4,5],6 :| [7,8]]
chunksOf   :: Foldable1 f => Size -> f a -> NonEmpty (NonEmpty a)
chunksOf k = go . F.toList
  where
    go xs = case List.genericSplitAt k xs of
              (ys,[]) -> NonEmpty.fromList ys :| []
              (ys,zs) -> NonEmpty.fromList ys <| go zs

--------------------------------------------------------------------------------

type BinaryHeight = Height

-- | A VEBTree, the children of an internal node are stored in an 'f
-- :: Type -> Type -> Type'.
--
data VEBTree f k v = Leaf {-#UNPACK #-} !k !v
                   | Node {-#UNPACK #-} !BinaryHeight -- ^ binary height of the children
                                        !(f k (Two (VEBTree f k v))) -- ^ Children

deriving instance (Show k, Show v, forall a b. (Show a, Show b) => Show (f a b)
                  ) => Show (VEBTree f k v)

deriving instance (Eq k, Eq v, forall a b. (Eq a, Eq b) => Eq (f a b)
                  ) => Eq (VEBTree f k v)

instance Functor (f k) => Functor (VEBTree f k) where
  fmap f = \case
    Leaf k x   -> Leaf k $ f x
    Node h chs -> Node h $ fmap (fmap (fmap f)) chs

instance Foldable (f k) => Foldable (VEBTree f k) where
  foldMap f = \case
    Leaf _ x   -> f x
    Node h chs -> foldMap (foldMap (foldMap f)) chs
instance Foldable1 (f k) => Foldable1 (VEBTree f k)

instance Traversable (f k) => Traversable (VEBTree f k) where
  traverse f = \case
    Leaf k x   -> Leaf k <$> f x
    Node h chs -> Node h <$> traverse (traverse (traverse f)) chs


instance Bifunctor f => Bifunctor (VEBTree f) where
  bimap f g = \case
    Leaf k x   -> Leaf (f k) (g x)
    Node h chs -> Node h $ bimap f (fmap (bimap f g)) chs

-- instance Traversable (f k) => FunctorWithIndex     k (VEBTree f k)
-- instance Traversable (f k) => FoldableWithIndex    k (VEBTree f k)
-- instance Traversable (f k) => TraversableWithIndex k (VEBTree f k) where
--   itraverse f s =

--     snd $ runIndexing (traverse (\a -> Indexing (\i -> i `seq` (i + 1, f i a))) s) 0
--   {-# INLINE itraverse #-}


-- instance Traversable (f k) => TraversableWithIndex Index (VEBTree f k) where
--   itraverse f s = snd $ runIndexing (traverse (\a -> Indexing (\i -> i `seq` (i + 1, f i a))) s) 0
--   {-# INLINE itraverse #-}



-- biitraverse     :: (Applicative g, Traversable (f k))
--                 => (Index -> k -> v -> g b)
--                 -> VEBTree f k v
--                 -> g (VEBTree f k b)
-- biitraverse f s = snd $ runBiIndexing (traverse (\a -> BiIndexing (\i k -> i `seq` (i + 1, f i k a))) s) 0
-- {-# INLINE biitraverse #-}



instance Bifoldable f => Bifoldable (VEBTree f) where
  bifoldMap f g = \case
    Leaf k x   -> f k <> g x
    Node h chs -> bifoldMap f (foldMap (bifoldMap f g)) chs

instance Bifoldable1 f => Bifoldable1 (VEBTree f)

instance Bitraversable f => Bitraversable (VEBTree f) where
  bitraverse f g = \case
    Leaf k x   -> Leaf <$> f k <*> g x
    Node h chs -> Node h <$> bitraverse f (traverse (bitraverse f g)) chs

-- | Bottom up fold over a VEBTree
foldVEB           :: Bifunctor f
                  => (k -> v -> b) -> (Height -> f k (Two b) -> b) -> VEBTree f k v -> b
foldVEB leaf node = go
  where
    go = \case
      Leaf k v   -> leaf k v
      Node h chs -> node h $ second (fmap $ foldVEB leaf node) chs


-- | Get a list of bottom trees
--
-- >>> mapM_ print $ bottoms myTree
-- Node 0 (NList ((0,Two (Leaf 0 (0,"a")) (Leaf 1 (1,"b"))) :| []))
-- Node 0 (NList ((2,Two (Leaf 2 (2,"c")) (Leaf 3 (3,"d"))) :| []))
bottoms :: Foldable (f k) => VEBTree f k v -> [VEBTree f k v]
bottoms = \case
  Leaf _ _   -> []
  Node _ chs -> concatMap F.toList $ F.toList chs


-- | Returns the keys in ascending order
--
-- >>> keys myTree
-- 0 :| [1,2,3]
keys :: (Foldable1 (f k), Bifunctor f) => VEBTree f k v -> NonEmpty k
keys = fmap fst . toAscList

-- | Returns the elements, in order of ascending key value
elems :: (Foldable1 (f k), Bifunctor f) => VEBTree f k v -> NonEmpty v
elems = fmap snd . toAscList


toAscList :: (Foldable1 (f k), Bifunctor f) => VEBTree f k v -> NonEmpty (k,v)
toAscList = foldVEB (curry singleton) (\_ chs -> foldMap1 (\(Two l r) -> l <> r) chs)
  -- FIXME: make this run in linear time, which I don't think it does at the moment

-- toAscList' :: (Foldable1 (f k), Bifunctor f) => VEBTree f k (k,v) -> NonEmpty (k,v)
-- toAscList' = fmap snd . toAscList










----------------------------------------

newtype NList a b = NList (NonEmpty (a,b))
                  deriving (Show,Eq,Ord,Read,Functor,Foldable,Traversable)

instance Bifunctor NList where
  bimap f g (NList xs) = NList $ fmap (bimap f g) xs

instance Bifoldable NList where
  bifoldMap f g (NList xs) = foldMap (bifoldMap f g) xs
instance Bifoldable1 NList

instance Bitraversable NList where
  bitraverse f g (NList xs) = NList <$> traverse (bitraverse f g) xs

instance Foldable1 (NList a)

----------------------------------------

-- | Build a VEBTree from an ascending (/non-descending) list of key,values
--
-- |
-- >>> fromAscList . NonEmpty.fromList $ [(0,0),(1,1),(2,2),(3,3)]
-- Node 1 (NList ((1,Two (Node 0 (NList ((0,Two (Leaf 0 (0,0)) (Leaf 1 (1,1))) :| []))) (Node 0 (NList ((2,Two (Leaf 2 (2,2)) (Leaf 3 (3,3))) :| [])))) :| []))
fromAscList :: Foldable1 f => f (k,v) -> VEBTree NList k (k,v)
fromAscList = build fst






-- | Builds a VEBTree, from v's in increasing order
build   :: Foldable1 f => (v -> k) -> f v -> VEBTree NList k v
build f = snd . bottomUp (mkLeaf f)
                         (\h n chs -> Node h <$> buildHelper chs)


mkLeaf f v = let k = f v in (k, Leaf k v)


buildHelper     :: Foldable f => f (a, b) -> (a, NList a (Two b))
buildHelper chs = case NonEmpty.nonEmpty $ pairUp' chs of
                    Nothing -> error "too few children; need at least 2"
                    Just xs -> let m = fst $ NonEmpty.last xs
                               in (m,NList . fmap snd $ xs)



pairUp' :: Foldable f => f (k, b) -> [(k, (k, Two b))]
pairUp' = fmap (\(Two (k,l) (m,r)) -> (m, (k, Two l r))) .  pairUp







-- | Convert the VEBTree into a normal (complete) binary leaf tree
toTree      :: (v -> k) -> VEBTree NList k v -> Tree.Tree k (k,v)
toTree getK = foldVEB leaf node
  where
    leaf k v = Tree.Leaf (k,v)
    node _ (NList chs) = replaceLeaf (Complete.fromAscList2 chs)

    replaceLeaf = foldTree (\(k,Two l r) -> Tree.Node l k r) Tree.Node



-- -- | The height of the binary tree represented by this VEBL
-- binaryHeight :: VEBL k v -> Height
-- binaryHeight = Tree.height . toTree


-- numChildrenProp :: VEBL k v -> Bool
-- numChildrenProp = foldVEBL (const True) $ \h chs -> length chs == pow h




--------------------------------------------------------------------------------
--

newtype TopVeb k v = TopVeb (VEBTree TopVeb k v)
  deriving (Show,Eq,Functor,Foldable,Traversable,Bifunctor,Bifoldable
           ,Foldable1
           -- ,FunctorWithIndex Index,FoldableWithIndex Index
           )

toVEBTree (TopVeb t) = t



-- instance TraversableWithIndex Index (TopVeb k) where
--   itraverse f (TopVeb t) = TopVeb <$> itraverse f t

instance Bitraversable TopVeb where
  bitraverse f g (TopVeb t) = TopVeb <$> bitraverse f g t

pattern Leaf' k x = TopVeb (Leaf k x)
pattern Node' h chs = TopVeb (Node h chs)



-- | Convert the VEBTree into a normal (complete) binary leaf tree
toTree' :: VEBTree TopVeb k v -> Tree.Tree k (k,v)
toTree' = foldVEB leaf node
  where
    leaf k v = Tree.Leaf (k,v)
    topLeaf k (Two l r) = Tree.Node l k r
    node _ (TopVeb chs) = foldVEB topLeaf node chs






fromAscKeys  :: (Foldable1 f, Functor f) => f k -> TopVeb k k
fromAscKeys = build' id

fromAscList' :: Foldable1 f => f (k,v) -> TopVeb k (k,v)
fromAscList' = build' fst

-- | Builds a VEBTree, from v's in increasing order
build'   :: Foldable1 f => (v -> k) -> f v -> TopVeb k v
build' f = TopVeb . snd . bottomUp (mkLeaf f)
                                   (\h n chs -> Node h <$> buildHelper' chs)

buildHelper'     :: Foldable f => f (a, b) -> (a, TopVeb a (Two b))
buildHelper' chs = case NonEmpty.nonEmpty $ pairUp' chs of
                     Nothing -> error "too few children; need at least 2"
                     Just xs -> let m = fst $ NonEmpty.last xs
                                in (m, second snd . fromAscList' . fmap snd $ xs)


-- | Stores the children of every internal node into a VEBTree itself.
withTopTrees :: VEBTree NList k v -> VEBTree (VEBTree NList) k v
withTopTrees = foldVEB Leaf $ \h (NList chs) -> Node h (second snd . fromAscList $ chs)


test :: [Int] -> TopVeb Int Int
test = second snd . fromAscList' . NonEmpty.fromList . map (\x -> (x,x))


-- -- | Stores the children of every internal node into a VEBTree itself.
-- withTopTrees' :: VEBTree NList k v -> TopVeb k v
-- withTopTrees' = foldVEB Leaf' $ \h (NList chs) -> Node' h (second snd . fromAscList' $ chs)

size :: TopVeb k v -> Size
size = \case
  Leaf' _ _   -> 1
  Node' h top -> Complete.size h + size top


type IBuild k v = Index -> NonEmpty (Embedded.Node k (k,v))


printEmbedding :: (Show k, Show v) => TopVeb k v -> IO ()
printEmbedding = mapM_ print . zip [0..] . F.toList . embed 0

loops = filter (\(i,t) -> case t of
                           Embedded.Leaf  _    -> False
                           Embedded.Node l _ r -> i == l || i == r
               )

printLoops :: (Show k, Show v) => TopVeb k v -> IO ()
printLoops = mapM_ print . loops . zip [0..] . F.toList . embed 0



printTopVEB            :: (Show k, Show v) => TopVeb k v -> String
printTopVEB (TopVeb t) = case t of
    Leaf _ _   -> show t <> "\n"
    Node h chs -> mconcat [ "Node \n"
                                   , "top: ("
                                   , printTopVEB . second (const ()) $ chs
                                   , ")\n"
                                   , "bottoms:\n"
                                   , concatMap (printTopVEB . TopVeb) $ bottoms t
                                   , "\n"
                                   ]



-- splitTopBottom :: VEBTree TopVeb k v -> [(k,Two (VEBTree TopVeb k v))]
-- splitTopBottom = \case
--   Leaf _ _            -> []
--   Node _ (TopVeb chs) -> F.toList $ toAscList chs





embed              :: Index -- ^ first free index we can assign to
                   -> TopVeb k v
                   -> NonEmpty (Embedded.Node k (k,v))
embed s (TopVeb t) = foldVEB leaf node t s
  where
    leaf        :: k -> v -> IBuild k v
    leaf k v _i = singleton $ Embedded.Leaf (k,v)

    node         :: Height -> TopVeb k (Two (IBuild k v)) -> IBuild k v
    node h top i = let top'  = imap' (f (i + size top) (Complete.size h)) top
                       top'' = second (fmap fst) top'
                   in embedTop top'' i <> foldMap1 (foldMap1 snd) top'

    -- computes the indices corresponding to the trees l and r
    f s' sb i (Two l r) = let li = s' + i*2*sb
                              ri = li + sb
                          in Two (li, l li) (ri, r ri)

    embedTop            :: TopVeb k (Two Index) -> IBuild k v
    embedTop (TopVeb t) = foldVEB topLeaf node t

    topLeaf                :: k -> Two Index -> IBuild k v
    topLeaf k (Two l r) _i = singleton $ Embedded.Node l k r

--------------------------------------------------------------------------------
-- * Helper functions

singleton   :: a -> NonEmpty a
singleton x = x :| []

--------------------------------------------------------------------------------
-- Directly taken from indexed-traversables but using 'Index' rather than 'Int'


imap'   :: Traversable (t k)
        => (Index -> v -> v')
        -> t k v
        -> t k v'
imap' f = runIdentity . itraverse' (\i v -> Identity $ f i v)
{-# INLINE imap' #-}

itraverse'     :: (Traversable (t k), Applicative f)
               => (Index -> v -> f v')
               -> t k v
               -> f (t k v')
itraverse' f s = snd $ runIndexing (traverse (\a -> Indexing (\i -> i `seq` (i + 1, f i a))) s) 0
{-# INLINE itraverse' #-}

-- | 'Applicative' composition of
-- @'Control.Monad.Trans.State.Lazy.State' 'Index'@ with a 'Functor',
-- used by 'Control.Lens.Indexed.indexed'.
newtype Indexing f a = Indexing { runIndexing :: Index -> (Index, f a) }

instance Functor f => Functor (Indexing f) where
  fmap f (Indexing m) = Indexing $ \i -> case m i of
    (j, x) -> (j, fmap f x)
  {-# INLINE fmap #-}

instance Applicative f => Applicative (Indexing f) where
  pure x = Indexing $ \i -> (i, pure x)
  {-# INLINE pure #-}
  Indexing mf <*> Indexing ma = Indexing $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <*> fa)
  {-# INLINE (<*>) #-}
