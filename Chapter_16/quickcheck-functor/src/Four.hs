module Four
     ( Four (Four)
     ) where

import Test.QuickCheck

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        w <- arbitrary
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return $ Four w x y z

instance Functor (Four a b c) where
    fmap f (Four w x y z) = Four w x y (f z)