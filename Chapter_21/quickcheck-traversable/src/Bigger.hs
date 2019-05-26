module Bigger
      (Bigger (Bigger)
      ) where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bigger a b = Bigger a b b b deriving (Eq, Show)
    
instance Functor (Bigger a) where
    fmap f (Bigger y x1 x2 x3) = Bigger y (f x1) (f x2) (f x3)

instance Foldable (Bigger a) where
    foldMap f (Bigger _ x1 x2 x3) = (f x1) <> (f x2) <> (f x3)

instance Traversable (Bigger a) where
    traverse f (Bigger y x1 x2 x3) = (fmap (Bigger y) (f x1)) <*> (f x2) <*> (f x3)
    --traverse f (Big y x1 x2) = (Big y) <$> (f x1) <*> (f x2) <*> (f x3)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
    arbitrary = do
        w <- arbitrary
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Bigger w x y z)

instance (Eq a, Eq b) => EqProp (Bigger a b) where
    (=-=) = eq