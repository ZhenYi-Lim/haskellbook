module BoolAndSomethingElse
     ( BoolAndSomethingElse (False', True')
     ) where

import Test.QuickCheck

data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (BoolAndSomethingElse a) where
    arbitrary = do
        x <- arbitrary
        frequency [ (1, return (False' x)), (1, return (True' x)) ]

instance Functor BoolAndSomethingElse where
    fmap f (False' x) = False' (f x)
    fmap f (True' x) = True' (f x)