module Sum
     ( Sum (First, Second)
     ) where

import Test.QuickCheck

data Sum b a = First a | Second b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return (First x)), (1, return (Second y)) ]

instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap _ (Second b) = Second b