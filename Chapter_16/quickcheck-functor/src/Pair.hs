module Pair
     ( Pair (Pair)
     ) where

import Test.QuickCheck

data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Pair x y

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)