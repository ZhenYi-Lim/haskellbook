module Trivial 
     ( Trivial
     , TrivAssoc
     ) where

import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
    arbitrary = return Trivial

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool