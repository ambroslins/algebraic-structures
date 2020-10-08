module Test.Algebra where

import Algebra
import Control.DeepSeq (NFData (..))
import Test.Hspec
import Test.QuickCheck
import Prelude (Eq (..), Show (..), ($))

testMagma :: (Show a, Arbitrary a, NFData a, Magma a) => Gen a -> Spec
testMagma gen = it "is total" $
  forAll gen $ \x y -> total (x <> y)

testSemigroup :: (Show m, Arbitrary m, NFData m, Semigroup m, Eq m) => Gen m -> Spec
testSemigroup gen = do
  testMagma gen
  it "is associative" $
    forAll gen $ \x y z ->
      (x <> y) <> z === x <> (y <> z)

testCommutativeSemigroup :: (Show m, Arbitrary m, NFData m, CommutativeSemigroup m, Eq m) => Gen m -> Spec
testCommutativeSemigroup gen = do
  testSemigroup gen
  it "is commutative" $
    forAll gen $ \x y -> x <> y === y <> x