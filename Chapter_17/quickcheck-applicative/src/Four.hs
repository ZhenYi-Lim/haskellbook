module Four
    ( Four (Four)
    )where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        w <- arbitrary
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return $ Four w x y z

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (Four w x y z) <> (Four w' x' y' z') = Four (w <> w') (x <> x') (y <> y') (z <> z')

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
    mempty = Four mempty mempty mempty mempty

instance Functor (Four a b c) where
    fmap f (Four w x y z) = Four w x y (f z)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure x = Four mempty mempty mempty x
    (Four w y z f) <*> (Four w' y' z' x) = Four (w <> w') (y <> y') (z <> z') (f x)