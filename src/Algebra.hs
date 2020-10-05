{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Algebra
  ( Nat,
    Natural,
    Int,
    Integer,
    Double,
    Magma (..),
    Semigroup (..),
    (+),
    (*),
    Quasigroup (..),
    (-),
    (/),
    Monoid (..),
    CommutativeMonoid (..),
    Loop (..),
    Group (..),
    Abelian (..),
  )
where

import Data.Semigroup (Product (..), Sum (..))
import GHC.Float (Double)
import GHC.Int (Int)
import GHC.Num (Integer, Natural)
import GHC.Word (Word)
import qualified Prelude as P

type Nat = Word

-- Magma

class Magma m where
  magma :: m -> m -> m

instance Magma (Sum Nat) where
  magma (Sum x) (Sum y) = Sum (x P.+ y)

instance Magma (Sum Natural) where
  magma (Sum x) (Sum y) = Sum (x P.+ y)

instance Magma (Sum Int) where
  magma (Sum x) (Sum y) = Sum (x P.+ y)

instance Magma (Sum Integer) where
  magma (Sum x) (Sum y) = Sum (x P.+ y)

instance Magma (Sum Double) where
  magma (Sum x) (Sum y) = Sum (x P.+ y)

instance Magma (Product Nat) where
  magma (Product x) (Product y) = Product (x P.* y)

instance Magma (Product Natural) where
  magma (Product x) (Product y) = Product (x P.* y)

instance Magma (Product Int) where
  magma (Product x) (Product y) = Product (x P.* y)

instance Magma (Product Integer) where
  magma (Product x) (Product y) = Product (x P.* y)

instance Magma (Product Double) where
  magma (Product x) (Product y) = Product (x P.* y)

-- Semigroup

class Magma m => Semigroup m

(+) :: Semigroup (Sum m) => m -> m -> m
x + y = getSum (Sum x `magma` Sum y)

(*) :: Semigroup (Product m) => m -> m -> m
x * y = getProduct (Product x `magma` Product y)

instance Semigroup (Sum Nat)

instance Semigroup (Sum Natural)

instance Semigroup (Sum Int)

instance Semigroup (Sum Integer)

instance Semigroup (Sum Double)

instance Semigroup (Product Nat)

instance Semigroup (Product Natural)

instance Semigroup (Product Int)

instance Semigroup (Product Integer)

instance Semigroup (Product Double)

-- Quasigroup

class Magma m => Quasigroup m where
  leftDivision :: m -> m -> m
  rightDivistion :: m -> m -> m

(-) :: Quasigroup (Sum a) => a -> a -> a
x - y = getSum (Sum x `rightDivistion` Sum y)

(/) :: Quasigroup (Product a) => a -> a -> a
x / y = getProduct (Product x `rightDivistion` Product y)

instance Quasigroup (Sum Nat) where
  leftDivision (Sum x) (Sum y) = Sum (y P.- x)
  rightDivistion (Sum x) (Sum y) = Sum (x P.- y)

instance Quasigroup (Sum Natural) where
  leftDivision (Sum x) (Sum y) = Sum (y P.- x)
  rightDivistion (Sum x) (Sum y) = Sum (x P.- y)

instance Quasigroup (Sum Int) where
  leftDivision (Sum x) (Sum y) = Sum (y P.- x)
  rightDivistion (Sum x) (Sum y) = Sum (x P.- y)

instance Quasigroup (Sum Integer) where
  leftDivision (Sum x) (Sum y) = Sum (y P.- x)
  rightDivistion (Sum x) (Sum y) = Sum (x P.- y)

instance Quasigroup (Sum Double) where
  leftDivision (Sum x) (Sum y) = Sum (y P.- x)
  rightDivistion (Sum x) (Sum y) = Sum (x P.- y)

instance Quasigroup (Product Double) where
  leftDivision (Product x) (Product y) = Product (y P./ x)
  rightDivistion (Product x) (Product y) = Product (x P./ y)

-- IdentityElement

class Magma m => IdentityElement m where
  identityElement :: m

instance IdentityElement (Sum Nat) where
  identityElement = Sum 0

instance IdentityElement (Sum Natural) where
  identityElement = Sum 0

instance IdentityElement (Sum Int) where
  identityElement = Sum 0

instance IdentityElement (Sum Integer) where
  identityElement = Sum 0

instance IdentityElement (Sum Double) where
  identityElement = Sum 0

instance IdentityElement (Product Nat) where
  identityElement = Product 1

instance IdentityElement (Product Natural) where
  identityElement = Product 1

instance IdentityElement (Product Int) where
  identityElement = Product 1

instance IdentityElement (Product Integer) where
  identityElement = Product 1

instance IdentityElement (Product Double) where
  identityElement = Product 1.0

-- Monoid

class (Semigroup m, IdentityElement m) => Monoid m where
  power :: m -> Nat -> m
  power x n = P.foldr magma identityElement (P.replicate (P.fromIntegral n) x)

instance {-# OVERLAPPABLE #-} (Semigroup m, IdentityElement m) => Monoid m

instance Monoid (Sum Nat)

instance Monoid (Sum Natural)

instance Monoid (Sum Int)

instance Monoid (Sum Integer)

instance Monoid (Sum Double)

instance Monoid (Product Nat)

instance Monoid (Product Natural)

instance Monoid (Product Int)

instance Monoid (Product Integer)

instance Monoid (Product Double)

-- CommutativeMonoid

class Monoid m => CommutativeMonoid m

instance CommutativeMonoid (Sum Nat)

instance CommutativeMonoid (Sum Natural)

instance CommutativeMonoid (Sum Int)

instance CommutativeMonoid (Sum Integer)

instance CommutativeMonoid (Sum Double)

instance CommutativeMonoid (Product Nat)

instance CommutativeMonoid (Product Natural)

instance CommutativeMonoid (Product Int)

instance CommutativeMonoid (Product Integer)

instance CommutativeMonoid (Product Double)

-- Loop

type Loop l = (Quasigroup l, IdentityElement l)

-- Group

class (Loop g, Monoid g) => Group g where
  inverse :: g -> g
  inverse = rightDivistion identityElement

instance Group (Sum Int) where
  inverse = P.fmap P.negate

instance Group (Sum Integer) where
  inverse = P.fmap P.negate

instance Group (Sum Double) where
  inverse = P.fmap P.negate

instance Group (Product Double) where
  inverse = P.fmap P.recip

-- Abelian group

type Abelian a = (CommutativeMonoid a, Group a)
