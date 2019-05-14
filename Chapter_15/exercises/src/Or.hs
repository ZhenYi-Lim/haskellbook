module Or 
     ( Or (Fst, Snd)
     , OrAssoc
     ) where

import Test.QuickCheck

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return (Fst x)), (1, return (Snd y)) ]

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
    (Snd x) <> _ = (Snd x)
    _ <> (Snd x) = (Snd x)
    _ <> (Fst x) = (Fst x)

type OrAssoc = Or String String -> Or String String -> Or String String -> Bool