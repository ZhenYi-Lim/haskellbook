module Four'
     ( Four' (Four')
     ) where

import Test.QuickCheck

data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        w <- arbitrary
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return $ Four' w x y z

instance Functor (Four' a) where
    fmap f (Four' w x y z) = Four' w x y (f z)