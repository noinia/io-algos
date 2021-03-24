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
import           Control.Monad.State.Strict
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.BinaryTree.LeafTree.Complete (split, lg, pow, div2, size, fullTree)
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

-- pre: even number of elements
pairUp :: Foldable f => f a -> [Two a]
pairUp = go . F.toList
  where
    go []         = []
    go [_]        = error "pairUp: Odd number of elements!"
    go (a:b:rest) = Two a b : go rest

-- | pre: n = m*k for some natural number m
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
data VEBTree f k v = Leaf !v
                   | Node {-#UNPACK #-} !BinaryHeight -- ^ binary height of the children
                                        !(f k (Two (VEBTree f k v))) -- ^ Children











deriving instance (Show k, Show v, forall a b. (Show a, Show b) => Show (f a b)
                  ) => Show (VEBTree f k v)

deriving instance (Eq k, Eq v, forall a b. (Eq a, Eq b) => Eq (f a b)
                  ) => Eq (VEBTree f k v)

instance Functor (f k) => Functor (VEBTree f k) where
  fmap f = \case
    Leaf x     -> Leaf $ f x
    Node h chs -> Node h $ fmap (fmap (fmap f)) chs

instance Foldable (f k) => Foldable (VEBTree f k) where
  foldMap f = \case
    Leaf x     -> f x
    Node h chs -> foldMap (foldMap (foldMap f)) chs

instance Traversable (f k) => Traversable (VEBTree f k) where
  traverse f = \case
    Leaf x     -> Leaf <$> f x
    Node h chs -> Node h <$> traverse (traverse (traverse f)) chs


instance Bifunctor f => Bifunctor (VEBTree f) where
  bimap f g = \case
    Leaf x     -> Leaf $ g x
    Node h chs -> Node h $ bimap f (fmap (bimap f g)) chs

instance Traversable (f k) => FunctorWithIndex     Index (VEBTree f k)
instance Traversable (f k) => FoldableWithIndex    Index (VEBTree f k)
instance Traversable (f k) => TraversableWithIndex Index (VEBTree f k) where
  itraverse f s = snd $ runIndexing (traverse (\a -> Indexing (\i -> i `seq` (i + 1, f i a))) s) 0
  {-# INLINE itraverse #-}



instance Bifoldable f => Bifoldable (VEBTree f) where
  bifoldMap f g = \case
    Leaf x     -> g x
    Node h chs -> bifoldMap f (foldMap (bifoldMap f g)) chs

instance Bifoldable1 f => Bifoldable1 (VEBTree f)

instance Bitraversable f => Bitraversable (VEBTree f) where
  bitraverse f g = \case
    Leaf x     -> Leaf <$> g x
    Node h chs -> Node h <$> bitraverse f (traverse (bitraverse f g)) chs


foldVEB           :: Bifunctor f
                  => (v -> b) -> (Height -> f k (Two b) -> b) -> VEBTree f k v -> b
foldVEB leaf node = go
  where
    go = \case
      Leaf v     -> leaf v
      Node h chs -> node h $ second (fmap $ foldVEB leaf node) chs

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

----------------------------------------





fromAscList :: Foldable1 f => f (k,v) -> VEBTree NList k (k,v)
fromAscList = build fst


-- | Builds a VEBTree, from v's in increasing order
build   :: Foldable1 f => (v -> k) -> f v -> VEBTree NList k v
build f = snd . bottomUp (\v -> (f v, Leaf v))
                          (\h n chs -> Node h <$> buildHelper chs)

buildHelper     :: Foldable f => f (a, b) -> (a, NList a (Two b))
buildHelper chs = case NonEmpty.nonEmpty $ pairUp' chs of
                    Nothing -> error "too few children; need at least 2"
                    Just xs -> let m = fst $ NonEmpty.last xs
                               in (m,NList . fmap snd $ xs)

pairUp' :: Foldable f => f (k, b) -> [(k, (k, Two b))]
pairUp' = fmap (\(Two (k,l) (m,r)) -> (m, (k, Two l r))) .  pairUp







-- | Convert the VEBTree into a normal (complete) binary leaf tree
toTree :: VEBTree NList k v -> Tree.Tree k v
toTree = foldVEB Tree.Leaf $ \_ (NList chs) -> replaceLeaf (Complete.fromAscList2 chs)
  where
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
           ,FunctorWithIndex Index,FoldableWithIndex Index
           )

instance TraversableWithIndex Index (TopVeb k) where
  itraverse f (TopVeb t) = TopVeb <$> itraverse f t

instance Bitraversable TopVeb where
  bitraverse f g (TopVeb t) = TopVeb <$> bitraverse f g t

pattern Leaf' x = TopVeb (Leaf x)
pattern Node' h chs = TopVeb (Node h chs)


fromAscKeys  :: (Foldable1 f, Functor f) => f k -> TopVeb k k
fromAscKeys = build' id

fromAscList' :: Foldable1 f => f (k,v) -> TopVeb k (k,v)
fromAscList' = build' fst

-- | Builds a VEBTree, from v's in increasing order
build'   :: Foldable1 f => (v -> k) -> f v -> TopVeb k v
build' f = TopVeb . snd . bottomUp (\v -> (f v, Leaf v))
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





embed              :: Index -- ^ first free index we can assign to
                   -> TopVeb k v
                   -> NonEmpty (Embedded.Node k v)
embed s (TopVeb t) = foldVEB (singleton . Embedded.Leaf) (\h chs -> undefined) t


--------------------------------------------------------------------------------
-- * Helper functions

singleton   :: a -> NonEmpty a
singleton x = x :| []

--------------------------------------------------------------------------------
-- Directly taken from indexed-traversables but using 'Index' rather than 'Int'

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
