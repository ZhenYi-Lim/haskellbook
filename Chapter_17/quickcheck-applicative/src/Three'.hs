module Three'
    ( Three' (Three')
    )where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return $ Three' x y z

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

instance (Semigroup a, Semigroup b) => Semigroup (Three' a b) where
    (Three' x y z) <> (Three' x' y' z') = Three' (x <> x') (y <> y') (z <> z')

instance (Monoid a, Monoid b) => Monoid (Three' a b) where
    mempty = Three' mempty mempty mempty

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
    pure x = Three' mempty x x
    (Three' z f g) <*> (Three' z' x y) = Three' (z <> z') (f x) (g y)