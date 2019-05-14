-- !!! QuickCheck
module Comp 
     ( Comp (Comp, unComp)
     , CompAssoc
     ) where

import Data.Monoid
import Test.QuickCheck

newtype Comp a = Comp { unComp :: (a -> a) }

instance Show (Comp a) where
    show _ = "Comp"

{- instance Eq (Comp a b) where
    (Comp f) == (Comp g) = f =-= g -}

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
    arbitrary = Comp <$> arbitrary

instance Semigroup a => Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp (f.g)

instance Monoid a => Monoid (Comp a) where
    mempty = Comp ( \n -> n )

type CompAssoc = Comp (Product Int) -> Comp (Product Int) -> Comp (Product Int) -> Product Int -> Bool