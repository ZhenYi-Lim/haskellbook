{-# LANGUAGE FlexibleInstances #-}

module K'
     ( K' (K')
     , Flip (Flip)
     ) where

import Test.QuickCheck

newtype K' a b = K' a deriving (Eq, Show)

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (K' a b) where
    arbitrary = do
        x <- arbitrary
        return $ K' x

--Don't know how to write the arbitrary instance for this.
{- instance Arbitrary a => Arbitrary (Flip f a b) where
    arbitrary = do
        x <- arbitrary
        return $ Flip (K x) -}

instance Functor (K' a) where
    fmap _ (K' x) = K' x

instance Functor (Flip K' a) where
    fmap f (Flip (K' x)) = Flip (K' (f x))