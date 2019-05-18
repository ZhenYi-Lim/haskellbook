module Two
     ( Two (Two)
     ) where

import Test.QuickCheck

data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Two x y

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)