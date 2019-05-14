module BoolDisj 
     ( BoolDisj
     , BoolDisjAssoc
     ) where

import Test.QuickCheck

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Arbitrary BoolDisj where
    arbitrary = do
        x <- arbitrary
        return $ BoolDisj x

instance Semigroup BoolDisj where
    (BoolDisj False) <> (BoolDisj False) = (BoolDisj False)
    _ <> _ = (BoolDisj True)

instance Monoid BoolDisj where
    mempty = BoolDisj False

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool