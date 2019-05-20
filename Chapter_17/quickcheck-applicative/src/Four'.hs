module Four'
    ( Four' (Four')
    )where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        w <- arbitrary
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return $ Four' w x y z

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

instance (Semigroup a, Semigroup b) => Semigroup (Four' a b) where
    (Four' w x y z) <> (Four' w' x' y' z') = Four' (w <> w') (x <> x') (y <> y') (z <> z')

instance (Monoid a, Monoid b) => Monoid (Four' a b) where
    mempty = Four' mempty mempty mempty mempty

instance Functor (Four' a) where
    fmap f (Four' w x y z) = Four' w x y (f z)

instance Monoid a => Applicative (Four' a) where
    pure x = Four' mempty mempty mempty x
    (Four' w y z f) <*> (Four' w' y' z' x) = Four' (w <> w') (y <> y') (z <> z') (f x)