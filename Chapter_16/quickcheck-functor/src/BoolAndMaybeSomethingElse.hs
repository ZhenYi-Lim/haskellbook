module BoolAndMaybeSomethingElse
     ( BoolAndMaybeSomethingElse (Falsish, Truish)
     ) where

import Test.QuickCheck

data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (BoolAndMaybeSomethingElse a) where
    arbitrary = do
        x <- arbitrary
        frequency [ (1, return Falsish), (1, return (Truish x)) ]

instance Functor BoolAndMaybeSomethingElse where
    fmap f Falsish = Falsish
    fmap f (Truish x) = Truish (f x)