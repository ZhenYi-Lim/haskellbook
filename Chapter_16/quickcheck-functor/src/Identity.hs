module Identity
     ( Identity (Identity)
     ) where

import Test.QuickCheck

newtype Identity a = Identity a deriving (Eq, Show)

{- instance Show a => Show (Identity a) where
    show (Identity a) = "Identity " <> (show a)

instance Eq a => Eq (Identity a) where
    (Identity x) == (Identity y) = x == y -}

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return $ Identity x

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)