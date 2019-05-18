module Three'
     ( Three' (Three')
     ) where

import Test.QuickCheck

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return $ Three' x y z

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)