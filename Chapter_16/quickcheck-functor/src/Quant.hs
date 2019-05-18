module Quant
     ( Quant (Finance, Desk, Floor)
     ) where

import Test.QuickCheck

data Quant a b = Finance | Desk a | Floor b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return Finance), (1, return (Desk x)), (1, return (Floor y)) ]

instance Functor (Quant a) where
    fmap f (Floor x) = Floor (f x)
    fmap _ Finance = Finance
    fmap _ (Desk x) = Desk x
    --fmap _ x = x --Doesn't work