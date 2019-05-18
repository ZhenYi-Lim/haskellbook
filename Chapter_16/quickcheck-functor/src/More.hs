module More
     ( More (L, R)
     ) where

import Test.QuickCheck

data More b a = L a b a | R b a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (More a b) where
    arbitrary = do
        w <- arbitrary
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        frequency [ (1, return (L w y x)), (1, return (R y w z)) ]

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'