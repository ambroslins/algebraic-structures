{-# LANGUAGE MonoLocalBinds #-}

module Algebra.Properties where

import Algebra
import Algebra.Prelude
import Control.DeepSeq (NFData, deepseq)

closed :: (NFData a, Magma a) => a -> a -> Bool
closed x y = deepseq (x <> y) True

associative :: (Eq m, Semigroup m) => m -> m -> m -> Bool
associative x y z = (x <> y) <> z == x <> (y <> z)

leftDivision :: (Eq m, Quasigroup m) => m -> m -> Bool
leftDivision x y = y == x <> (x <\> y) && y == x <\> (x <> y)

rightDivision :: (Eq m, Quasigroup m) => m -> m -> Bool
rightDivision x y = y == (y </> x) <> x && y == (y <> x) </> x

division :: (Eq m, Quasigroup m) => m -> m -> Bool
division x y = leftDivision x y && rightDivision x y

leftIdentity :: (Eq a, IdentityElement a) => a -> Bool
leftIdentity x = identityElement <> x == x

rightIdentity :: (Eq a, IdentityElement a) => a -> Bool
rightIdentity x = x <> identityElement == x

identity :: (Eq a, IdentityElement a) => a -> Bool
identity x = leftIdentity x && rightIdentity x

commutative :: (Eq m, CommutativeSemigroup m) => m -> m -> Bool
commutative x y = x <> y == y <> x

leftInverse :: (Eq a, Group a) => a -> Bool
leftInverse x = inverse x <> x == identityElement

rightInverse :: (Eq a, Group a) => a -> Bool
rightInverse x = x <> inverse x == identityElement

leftDistributive :: (Eq m, Ring m) => m -> m -> m -> Bool
leftDistributive x y z = x * (y + z) == (x * y) + (x * z)

rightDistributive :: (Eq m, Ring m) => m -> m -> m -> Bool
rightDistributive x y z = (y + z) * x == (y * x) + (z * x)

distributive :: (Eq m, Ring m) => m -> m -> m -> Bool
distributive x y z = leftDistributive x y z && rightDistributive x y z