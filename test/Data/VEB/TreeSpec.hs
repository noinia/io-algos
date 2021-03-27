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

--------------------------------------------------------------------------------


newtype AscList a = AscList (NonEmpty a)
  deriving (Show,Eq,Ord,Functor,Foldable,Foldable1,Traversable)


type AscK  = AscList Int

type AscKV = AscList (Int,Int)


instance Arbitrary AscK where
  arbitrary =




withValues :: Functor f => f a -> f (a,a)
withValues = fmap (\x -> (x,x))




spec :: Spec
spec = undefined

-- describe "VEBTree Tests" $ do
--          property "Building complete tree NList" $ \(AscList xs :: AscKV) ->
--            Complete.fromAscList2 xs == VEBTree.toTree  (VEBTree.fromAscList xs)
