module K
     ( K (K)
     ) where

import Test.QuickCheck

data K a b = K a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (K a b) where
    arbitrary = do
        x <- arbitrary
        return $ K x

instance Functor (K a) where
    fmap _ (K x) = K x