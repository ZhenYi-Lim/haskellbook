module Optional
    ( Optional (Only, Nada)
    ) where

import Control.Monad
import Test.QuickCheck

data Optional a = Nada | Only a
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = do
        x <- arbitrary
        frequency [ (1, return Nada),
                    (4, return (Only x))]

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
    (Only x) <> (Only y) = Only (x <> y)
    x <> Nada = x
    Nada <> x = x