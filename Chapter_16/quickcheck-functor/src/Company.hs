module Company
     ( Company (DeepBlue, Something)
     ) where

import Test.QuickCheck

data Company a c b = DeepBlue a c | Something b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Company a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        frequency [ (1, return (DeepBlue x z)), (1, return (Something y)) ]

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c