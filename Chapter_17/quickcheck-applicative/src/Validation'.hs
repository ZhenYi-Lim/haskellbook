module Validation'
    ( Validation' (Failure', Success')
    )where

--import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation' e a = Failure' e | Success' a deriving (Eq, Show)

-- same as Either
instance Functor (Validation' e) where
fmap _ (Failure' e) = Failure' e
fmap f (Success' x) = Success' (f x)

-- This is different
instance Monoid e => Applicative (Validation' e) where
pure = Success'
(Failure' errs1) <*> (Failure' errs2) = Failure' (errs1 <> errs2)
(Failure' errs) <*> _ = Failure' errs
_ <*> (Failure' errs) = Failure' errs
(Success' f) <*> (Success' x) = Success' (f x)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation' e a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return (Failure' x)), (1, return (Success' y)) ]

instance (Eq a, Eq e) => EqProp (Validation' e a) where
    (=-=) = eq