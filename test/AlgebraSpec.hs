{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AlgebraSpec where

import Algebra
import Algebra.Prelude
import qualified Algebra.Properties as Properties
import Control.DeepSeq (NFData)
import Data.Proxy
import Test.Hspec
import Test.QuickCheck
import qualified Prelude as P

instance Arbitrary Natural where
  arbitrary = P.fromInteger . P.abs <$> (arbitrary :: Gen Integer)

spec :: Spec
spec = do
  describe "Integer" $ specRing (Proxy :: Proxy Integer)
  describe "Int" $ specRing (Proxy :: Proxy Int)
  describe "Sum Natural" $ specCommutativeMonoid (Proxy :: Proxy (Sum Natural))
  describe "Sum Nat" $ specCommutativeMonoid (Proxy :: Proxy (Sum Nat))
  describe "Product Natural" $ specCommutativeMonoid (Proxy :: Proxy (Product Natural))
  describe "Product Nat" $ specCommutativeMonoid (Proxy :: Proxy (Product Nat))

specClosed ::
  forall a.
  (Show a, Arbitrary a, NFData a, Magma a) =>
  Proxy a ->
  Spec
specClosed _ = it "is closed" $ property (Properties.closed :: a -> a -> Bool)

specAssociative ::
  forall a.
  (Show a, Arbitrary a, Eq a, Semigroup a) =>
  Proxy a ->
  Spec
specAssociative _ = it "is associative" $ property (Properties.associative :: a -> a -> a -> Bool)

specDivision ::
  forall a.
  (Show a, Arbitrary a, Eq a, Quasigroup a) =>
  Proxy a ->
  Spec
specDivision _ = it "satisfies divison" $ property (Properties.division :: a -> a -> Bool)

specIdentity ::
  forall a.
  (Show a, Arbitrary a, Eq a, IdentityElement a) =>
  Proxy a ->
  Spec
specIdentity _ = it "has identity" $ property (Properties.identity :: a -> Bool)

specCommutative ::
  forall a.
  (Show a, Arbitrary a, Eq a, CommutativeSemigroup a) =>
  Proxy a ->
  Spec
specCommutative _ = it "is commutative" $ property (Properties.commutative :: a -> a -> Bool)

specInverse ::
  forall a.
  (Show a, Arbitrary a, Eq a, Group a) =>
  Proxy a ->
  Spec
specInverse _ = it "has inverse" $ property (\(x :: a) -> Properties.leftInverse x && Properties.rightInverse x)

specDistributive ::
  forall a.
  (Show a, Arbitrary a, Eq a, Ring a) =>
  Proxy a ->
  Spec
specDistributive _ = it "is distributive" $ property (Properties.distributive :: a -> a -> a -> Bool)

specMagma :: (Show a, Arbitrary a, NFData a, Magma a) => Proxy a -> Spec
specMagma p = describe "is Magma" $ specClosed p

specSemigroup :: (Show a, Arbitrary a, Eq a, Semigroup a) => Proxy a -> Spec
specSemigroup p = describe "is Semigroup" $ do
  specAssociative p

specCommutativeSemigroup :: (Show a, Arbitrary a, Eq a, CommutativeSemigroup a) => Proxy a -> Spec
specCommutativeSemigroup p = describe "is CommutativeSemigroup" $ do
  specAssociative p
  specCommutative p

specMonoid :: (Show a, Arbitrary a, Eq a, Monoid a) => Proxy a -> Spec
specMonoid p = describe "is Monoid" $ do
  specAssociative p
  specIdentity p

specCommutativeMonoid :: (Show a, Arbitrary a, Eq a, CommutativeMonoid a) => Proxy a -> Spec
specCommutativeMonoid p = describe "is CommutativeMonoid" $ do
  specAssociative p
  specCommutative p
  specIdentity p

specGroup :: (Show a, Arbitrary a, Eq a, Group a) => Proxy a -> Spec
specGroup p = describe "is Group" $ do
  specAssociative p
  specDivision p
  specIdentity p
  specInverse p

specAbelian :: (Show a, Arbitrary a, Eq a, Abelian a) => Proxy a -> Spec
specAbelian p = describe "is Abelian" $ do
  specAssociative p
  specDivision p
  specIdentity p
  specInverse p
  specCommutative p

specRing :: (Show a, Arbitrary a, NFData a, Eq a, Ring a) => Proxy a -> Spec
specRing p = describe "is Ring" $ do
  describe "under Addition" $ specAbelian (Sum <$> p)
  describe "under Multiplication" $ specMonoid (Product <$> p)
  specDistributive p