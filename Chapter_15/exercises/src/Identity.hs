module Identity 
     ( Identity
     , IdenAssoc
     ) where

import Test.QuickCheck

newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return $ Identity x

instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = (Identity (x <> y))

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

type IdenAssoc = Identity String -> Identity String -> Identity String -> Bool