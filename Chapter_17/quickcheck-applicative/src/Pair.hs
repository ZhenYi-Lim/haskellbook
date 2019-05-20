module Pair
    ( Pair (Pair)
    )where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Pair x y

instance Eq a => EqProp (Pair a) where
    (=-=) = eq

instance Semigroup a => Semigroup (Pair a) where
    (Pair x y) <> (Pair x' y') = Pair (x <> x') (y <> y')

instance Monoid a => Monoid (Pair a) where
    mempty = Pair mempty mempty

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)