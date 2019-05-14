--https://stackoverflow.com/questions/41350192/how-to-test-semigroup-law-for-this-data-type
-- !!! QuickCheck
module Combine 
     ( Combine (Combine, unCombine)
     , CombineAssoc
     ) where

import Data.Monoid
import Test.QuickCheck
--import Test.QuickCheck.Checker

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
    show _ = "Combine"

{- instance Eq (Combine a b) where
    (Combine f) == (Combine g) = f =-= g -}

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = Combine <$> arbitrary

instance Semigroup b => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine ( \n -> (f n) <> (g n) )

instance Monoid b => Monoid (Combine a b) where
    mempty = Combine ( \_ -> mempty )

type CombineAssoc = Combine Bool (Product Int) -> Combine Bool (Product Int) -> Combine Bool (Product Int) -> Bool -> Bool