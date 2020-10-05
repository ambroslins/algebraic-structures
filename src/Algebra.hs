{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Algebra where

import Data.Semigroup (Product (..), Sum (..))
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

instance Magma (Product Nat) where
  magma (Product x) (Product y) = Product (x P.* y)

instance Magma (Product Natural) where
  magma (Product x) (Product y) = Product (x P.* y)

instance Magma (Product Int) where
  magma (Product x) (Product y) = Product (x P.* y)

instance Magma (Product Integer) where
  magma (Product x) (Product y) = Product (x P.* y)

class Magma m => Semigroup m

instance Semigroup (Sum Nat)

instance Semigroup (Sum Natural)

instance Semigroup (Sum Int)

instance Semigroup (Sum Integer)

instance Semigroup (Product Nat)

instance Semigroup (Product Natural)

instance Semigroup (Product Int)

instance Semigroup (Product Integer)

class Magma m => Quasigroup m where
  leftDivision :: m -> m -> m
  rightDivistion :: m -> m -> m

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

type Monoid m = (Semigroup m, IdentityElement m)

class Monoid m => CommutativeMonoid m

instance CommutativeMonoid (Sum Nat)

instance CommutativeMonoid (Sum Natural)

instance CommutativeMonoid (Sum Int)

instance CommutativeMonoid (Sum Integer)

type Loop l = (Quasigroup l, IdentityElement l)

class (Loop g, Monoid g) => Group g where
  inverse :: g -> g
