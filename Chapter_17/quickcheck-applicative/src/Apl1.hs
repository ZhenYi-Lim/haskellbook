module Apl1 
    ( ZipLst (ZipLst)
    )where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- this isn't going to work properly
newtype ZipLst a = ZipLst [a] deriving (Eq, Show)

instance Semigroup a => Semigroup (ZipLst a) where
    (ZipLst xs) <> (ZipLst ys) = ZipLst (zipWith (<>) xs ys)

instance Monoid a => Monoid (ZipLst a) where
    --mempty = ZipLst mempty
    mempty = ZipLst (repeat mempty)

instance Arbitrary a => Arbitrary (ZipLst a) where
    arbitrary = ZipLst <$> arbitrary

{- instance Arbitrary a => Arbitrary (Sum a) where
    arbitrary = Sum <$> arbitrary -}

instance Eq a => EqProp (ZipLst a) where
    (=-=) = eq
