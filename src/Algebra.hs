{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Algebra
  ( Magma (..),
    Semigroup (..),
    CommutativeSemigroup (..),
    (+),
    (*),
    Quasigroup (..),
    (-),
    (/),
    IdentityElement (..),
    zero,
    one,
    Monoid (..),
    CommutativeMonoid (..),
    Loop (..),
    Group (..),
    Abelian (..),
    Semiring (..),
    Ring (..),
    CommutativeRing (..),
    Field (..),
    Idempotent (..),
    Semilattice (..),
    BoundSemilattice (..),
    Lattice (..),
    BoundLattice (..),
    LeftSemimodule (..),
    RightSemimodule (..),
    Semimodule (..),
    LeftModule (..),
    RighModule (..),
    Module (..),
  )
where

import Algebra.Prelude
import qualified Prelude as P

newtype Join a = Join {getJoin :: a}
  deriving (Eq, Ord, Show, Read)

newtype Meet a = Meet {getMeet :: a}
  deriving (Eq, Ord, Show, Read)

-- Magma

class Magma m where
  (<>) :: m -> m -> m

instance Magma (Join Bool) where
  Join x <> Join y = Join (x || y)

instance Magma (Meet Bool) where
  Meet x <> Meet y = Meet (x && y)

instance Magma (Sum Nat) where
  Sum x <> Sum y = Sum (x P.+ y)

instance Magma (Sum Natural) where
  Sum x <> Sum y = Sum (x P.+ y)

instance Magma (Sum Int) where
  Sum x <> Sum y = Sum (x P.+ y)

instance Magma (Sum Integer) where
  Sum x <> Sum y = Sum (x P.+ y)

instance Magma (Sum Double) where
  Sum x <> Sum y = Sum (x P.+ y)

instance Magma (Product Nat) where
  Product x <> Product y = Product (x P.* y)

instance Magma (Product Natural) where
  Product x <> Product y = Product (x P.* y)

instance Magma (Product Int) where
  Product x <> Product y = Product (x P.* y)

instance Magma (Product Integer) where
  Product x <> Product y = Product (x P.* y)

instance Magma (Product Double) where
  Product x <> Product y = Product (x P.* y)

-- Semigroup

class Magma m => Semigroup m

instance Semigroup (Join Bool)

instance Semigroup (Meet Bool)

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

-- Commutative Semigroup

class Semigroup m => CommutativeSemigroup m

(+) :: Semigroup (Sum m) => m -> m -> m
x + y = getSum (Sum x <> Sum y)

(*) :: Semigroup (Product m) => m -> m -> m
x * y = getProduct (Product x <> Product y)

instance CommutativeSemigroup (Join Bool)

instance CommutativeSemigroup (Meet Bool)

instance CommutativeSemigroup (Sum Nat)

instance CommutativeSemigroup (Sum Natural)

instance CommutativeSemigroup (Sum Int)

instance CommutativeSemigroup (Sum Integer)

instance CommutativeSemigroup (Sum Double)

instance CommutativeSemigroup (Product Nat)

instance CommutativeSemigroup (Product Natural)

instance CommutativeSemigroup (Product Int)

instance CommutativeSemigroup (Product Integer)

instance CommutativeSemigroup (Product Double)

-- Quasigroup

class Magma m => Quasigroup m where
  -- | left division
  (<\>) :: m -> m -> m

  -- | right division
  (</>) :: m -> m -> m

(-) :: Quasigroup (Sum a) => a -> a -> a
x - y = getSum (Sum x </> Sum y)

(/) :: Quasigroup (Product a) => a -> a -> a
x / y = getProduct (Product x </> Product y)

instance Quasigroup (Sum Nat) where
  Sum x <\> Sum y = Sum (y P.- x)
  Sum x </> Sum y = Sum (x P.- y)

instance Quasigroup (Sum Natural) where
  Sum x <\> Sum y = Sum (y P.- x)
  Sum x </> Sum y = Sum (x P.- y)

instance Quasigroup (Sum Int) where
  Sum x <\> Sum y = Sum (y P.- x)
  Sum x </> Sum y = Sum (x P.- y)

instance Quasigroup (Sum Integer) where
  Sum x <\> Sum y = Sum (y P.- x)
  Sum x </> Sum y = Sum (x P.- y)

instance Quasigroup (Sum Double) where
  Sum x <\> Sum y = Sum (y P.- x)
  Sum x </> Sum y = Sum (x P.- y)

instance Quasigroup (Product Double) where
  Product x <\> Product y = Product (y P./ x)
  Product x </> Product y = Product (x P./ y)

-- IdentityElement

class Magma m => IdentityElement m where
  identityElement :: m

zero :: IdentityElement (Sum a) => a
zero = getSum identityElement

one :: IdentityElement (Product a) => a
one = getProduct identityElement

instance IdentityElement (Join Bool) where
  identityElement = Join False

instance IdentityElement (Meet Bool) where
  identityElement = Meet True

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

class (Semigroup m, IdentityElement m) => Monoid m

instance (Semigroup m, IdentityElement m) => Monoid m

-- CommutativeMonoid

class (CommutativeSemigroup m, Monoid m) => CommutativeMonoid m

instance (CommutativeSemigroup m, Monoid m) => CommutativeMonoid m

-- Loop

class (Quasigroup l, IdentityElement l) => Loop l

instance (Quasigroup l, IdentityElement l) => Loop l

-- Group

class (Loop g, Monoid g) => Group g where
  inverse :: g -> g
  inverse = (</> identityElement)

instance Group (Sum Int) where
  inverse = fmap P.negate

instance Group (Sum Integer) where
  inverse = fmap P.negate

instance Group (Sum Double) where
  inverse = fmap P.negate

instance Group (Product Double) where
  inverse = fmap P.recip

-- Abelian group

class (CommutativeMonoid a, Group a) => Abelian a

instance (CommutativeMonoid a, Group a) => Abelian a

-- Semiring

class (CommutativeMonoid (Sum r), Monoid (Product r)) => Semiring r

instance (CommutativeMonoid (Sum r), Monoid (Product r)) => Semiring r

-- Ring

class (Abelian (Sum r), Monoid (Product r)) => Ring r

instance (Abelian (Sum r), Monoid (Product r)) => Ring r

class (Abelian (Sum r), CommutativeMonoid (Product r)) => CommutativeRing r

instance (Abelian (Sum r), CommutativeMonoid (Product r)) => CommutativeRing r

-- Field

class (Abelian (Sum f), Abelian (Product f)) => Field f

instance (Abelian (Sum f), Abelian (Product f)) => Field f

-- Idempotent

class Magma m => Idempotent m

instance Idempotent (Join Bool)

instance Idempotent (Meet Bool)

-- Semilattice

class (CommutativeSemigroup l, Idempotent l) => Semilattice l

instance (CommutativeSemigroup l, Idempotent l) => Semilattice l

class (CommutativeMonoid l, Idempotent l) => BoundSemilattice l

instance (CommutativeMonoid l, Idempotent l) => BoundSemilattice l

-- Lattice

class (Semilattice (Join l), Semilattice (Meet l)) => Lattice l

instance (Semilattice (Join l), Semilattice (Meet l)) => Lattice l

class (BoundSemilattice (Join l), BoundSemilattice (Meet l)) => BoundLattice l

instance (BoundSemilattice (Join l), BoundSemilattice (Meet l)) => BoundLattice l

-- Semimodule

class (Semiring r, CommutativeMonoid m) => LeftSemimodule r m where
  (.*) :: r -> m -> m

class (Semiring r, CommutativeMonoid m) => RightSemimodule r m where
  (*.) :: m -> r -> m

class (LeftSemimodule r m, RightSemimodule r m) => Semimodule r m

instance (LeftSemimodule r m, RightSemimodule r m) => Semimodule r m

-- Module

class (LeftSemimodule r m, Abelian m) => LeftModule r m

instance (LeftSemimodule r m, Abelian m) => LeftModule r m

class (RightSemimodule r m, Abelian m) => RighModule r m

instance (RightSemimodule r m, Abelian m) => RighModule r m

class (Semimodule r m, Abelian m) => Module r m

instance (Semimodule r m, Abelian m) => Module r m

-- Vector space

class (LeftModule f v, Field f) => LeftVectorSpace f v

instance (LeftModule f v, Field f) => LeftVectorSpace f v

class (RighModule f v, Field f) => RighVectorSpace f v

instance (RighModule f v, Field f) => RighVectorSpace f v

class (Module f v, Field f) => VectorSpace f v

instance (Module f v, Field f) => VectorSpace f v
