{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Algebra
  ( Nat,
    Natural,
    Int,
    Integer,
    Double,
    Bool (..),
    Sum (..),
    Product (..),
    Magma (..),
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
    Idempotent (..),
    Semilattice (..),
    (||),
    (&&),
    BoundSemilattice (..),
    Lattice (..),
    BoundLattice (..),
  )
where

import Data.Bool (Bool (..), (&&), (||))
import Data.Function
import Data.Functor
import Data.Semigroup (Product (..), Sum (..))
import Data.Word (Word)
import GHC.Float (Double)
import GHC.Int (Int)
import GHC.Num (Integer, Natural)
import Prelude
  ( Eq (..),
    Ord (..),
    Read (..),
    Show (..),
  )
import qualified Prelude as P

type Nat = Word

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

(+) :: CommutativeSemigroup (Sum m) => m -> m -> m
x + y = getSum (Sum x <> Sum y)

(*) :: CommutativeSemigroup (Product m) => m -> m -> m
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

type Monoid m = (Semigroup m, IdentityElement m)

-- CommutativeMonoid

type CommutativeMonoid m = (CommutativeSemigroup m, Monoid m)

-- Loop

type Loop l = (Quasigroup l, IdentityElement l)

-- Group

class (Loop g, Monoid g) => Group g where
  inverse :: g -> g
  inverse = (</> identityElement)

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

-- Semiring

type Semiring r = (CommutativeMonoid (Sum r), Monoid (Product r))

-- Ring

type Ring r = (Abelian (Sum r), Monoid (Product r))

type CommutativeRing r = (Abelian (Sum r), CommutativeMonoid (Product r))

-- Field

type Field f = (Abelian (Sum f), Abelian (Product f))

-- Idempotent

class Magma m => Idempotent m

instance Idempotent (Join Bool)

instance Idempotent (Meet Bool)

-- Semilattice

type Semilattice l = (CommutativeSemigroup l, Idempotent l)

type BoundSemilattice l = (CommutativeMonoid l, Idempotent l)

-- Lattice

type Lattice l = (Semilattice (Join l), Semilattice (Meet l))

type BoundLattice l = (BoundSemilattice (Join l), BoundSemilattice (Meet l))

-- Semimodule

class (Semiring r, CommutativeMonoid m) => LeftSemimodule r m where
  (.*) :: r -> m -> m

class (Semiring r, CommutativeMonoid m) => RightSemimodule r m where
  (*.) :: m -> r -> m

type Semimodule r m = (LeftSemimodule r m, RightSemimodule r m)

-- Module

type LeftModule r m = (LeftSemimodule r m, Abelian m)

type RighModule r m = (RightSemimodule r m, Abelian m)

type Module r m = (Semimodule r m, Abelian m)

-- Vector space

type LeftVectorSpace f v = (LeftModule f v, Field f)

type RighVectorSpace f v = (RighModule f v, Field f)

type VectorSpace f v = (Module f v, Field f)

-- Integral Domain

class (Ring r) => IntegralDomain r

instance IntegralDomain Int

instance IntegralDomain Integer

instance IntegralDomain Double