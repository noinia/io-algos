{-# LANGUAGE  ScopedTypeVariables #-}
module Data.VEB.TreeSpec where

import qualified Data.BinaryTree.LeafTree.Complete as Complete
import qualified Data.BinaryTree.LeafTree.Core as Tree
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup.Foldable
import qualified Data.VEB.Tree as VEBTree
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




-- describe "VEBTree Tests" $ do
--          property "Building complete tree NList" $ \(AscList xs :: AscKV) ->
--            Complete.fromAscList2 xs == VEBTree.toTree  (VEBTree.fromAscList xs)
