module Pair
      (Pair (Pair)
      ) where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a b = Pair a b deriving (Eq, Show)
    
instance Functor (Pair a) where
    fmap f (Pair y x) = Pair y (f x)

instance Foldable (Pair a) where
    foldMap f (Pair _ x) = f x

instance Traversable (Pair a) where
    traverse f (Pair y x) = fmap (Pair y) (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Pair x y)

instance (Eq a, Eq b) => EqProp (Pair a b) where
    (=-=) = eq