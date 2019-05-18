module Three
     ( Three (Three)
     ) where

import Test.QuickCheck

data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return $ Three x y z

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)