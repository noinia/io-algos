{-# LANGUAGE  ScopedTypeVariables #-}
module Data.VEB.TreeSpec where

import           Control.Monad (forM_)
import           Data.Bifunctor
import qualified Data.BinaryTree.LeafTree.Complete as Complete
import           Data.BinaryTree.LeafTree.Embedded (Tree, Node(..))
import qualified Data.BinaryTree.LeafTree.Embedded as E
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup.Foldable
import qualified Data.VEB.Tree as VEBTree
import qualified Data.Vector as V
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
--------------------------------------------------------------------------------


newtype AscList a = AscList (NonEmpty a)
  deriving (Show,Eq,Ord,Functor,Foldable,Foldable1,Traversable)


type AscK  = AscList Int

type AscKV = AscList (Int,Int)


instance (Arbitrary a, Ord a) => Arbitrary (AscList a) where
  -- ^ up to roughly a milion elements.
  arbitrary = do (h :: Int) <- arbitrary `suchThat` \c -> 0 < c && c < 20
                 AscList . NonEmpty.sort . NonEmpty.fromList <$> vector (Complete.pow h)


withValues :: Functor f => f a -> f (a,a)
withValues = fmap (\x -> (x,x))

spec :: Spec
spec = describe "VEBTree NList tests" $ do
         it "toAscList . fromAscList" $ do
           property $ \(AscList xs :: AscKV) ->
                        VEBTree.toAscList (VEBTree.fromAscList2 xs) == xs
         describe "TopVEB tests" $ do
           it "toAscList . fromAscList" $ do
             property $ \(AscList xs :: AscKV) ->
                        VEBTree.toAscList (VEBTree.toVEBTree $ VEBTree.fromAscList' xs) == xs
           describe "embedding small" $ do
             forM_ answersSmall $ \(h,ans) ->
               it ("tree of height " <> show h) $
                 VEBTree.toEmbedded (test h) `shouldBe` ans

-- describe "VEBTree Tests" $ do
--          property "Building complete tree NList" $ \(AscList xs :: AscKV) ->
--            Complete.fromAscList2 xs == VEBTree.toTree  (VEBTree.fromAscList xs)


test   :: Int -> VEBTree.TopVeb Int String
test h = VEBTree.fromAscList' . NonEmpty.fromList $ zip [0..(Complete.pow h - 1)] vals

vals :: [String]
vals = [ f i s | i <- [1..], s <- strs ]
  where
    f i s = concat $ replicate i s
    strs = map (:[]) chars
    chars = ['a'..'z'] <> ['A'..'Z']

answersSmall :: [(VEBTree.BinaryHeight, E.Tree Int (Int,String))]
answersSmall = second V.fromList <$>
               [ (0, [Leaf (0,"a")])
               , (1, [Node 1 0 2,Leaf (0,"a"),Leaf (1,"b")])
               , (2, [Node 1 1 4,Node 2 0 3,Leaf (0,"a"),Leaf (1,"b"),Node 5 2 6,Leaf (2,"c"),Leaf (3,"d")])
               ]
