module Validation 
     ( Validation (Failure, Success)
     , ValidationAssoc
     ) where

import Test.QuickCheck

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return (Validation.Failure x)), (1, return (Validation.Success y)) ]
    
instance Semigroup a => Semigroup (Validation a b) where
    (Validation.Failure x) <> (Validation.Failure y) = (Validation.Failure (x <> y))
    (Validation.Success x) <> _ = (Validation.Success x)
    _ <> (Validation.Success x) = (Validation.Success x)

instance Monoid a => Monoid (Validation a b) where
    mempty = Validation.Failure mempty

type ValidationAssoc = Validation String Int -> Validation String Int -> Validation String Int -> Bool