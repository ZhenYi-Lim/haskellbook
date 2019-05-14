module First'
    ( First'
    , FirstMappend
    , FstId
    , firstMappend
    ) where

import Test.QuickCheck
import Optional

newtype First' a =
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        x <- arbitrary
        return (First' x)

instance Monoid (First' a) where
    mempty = First' Nada

instance Semigroup (First' a) where
    (First' (Only x)) <> (First' (Only y)) = (First' (Only x))
    x <> (First' Nada) = x
    (First' Nada) <> x = x

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool