module PhhhbbtttEither
    (PhhhbbtttEither (Left', Right')
    ) where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data PhhhbbtttEither b a = Left' a | Right' b deriving (Eq, Show)
instance Functor (PhhhbbtttEither b) where
    fmap f (Left' x) = Left' (f x)
    fmap _ (Right' x) = Right' x

instance Applicative (PhhhbbtttEither b) where
    pure = Left'
    (Left' f) <*> (Left' x) = Left' (f x)
    (Right' x) <*> _ = Right' x
    _ <*> (Right' x) = Right' x

instance Monad (PhhhbbtttEither b) where
    return = pure
    (Left' x) >>= f = f x
    (Right' x) >>= _ = Right' x

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return (Left' x)), (1, return (Right' y)) ]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
    (=-=) = eq